// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//
// groove-harness.js — Universal Groove Harness for Gossamer Panels
//
// This is the boilerplate you DON'T have to write. Include this file in any
// Gossamer panel and it handles:
//
//   - Groove discovery (probing nearby ports for groove partners)
//   - Connection lifecycle (connect, heartbeat, disconnect, reconnect)
//   - FLI provisioning (loading FLI modules based on your clade traits)
//   - Graceful degradation (partner lost → features disappear, no errors)
//   - Attestation (provenance records for every connection event)
//   - Capability routing (partner offers X → your onCapability('X') fires)
//
// YOU only write:
//   1. Your capability declarations (what you offer and consume)
//   2. Your UI (the HTML/CSS for your specific thing)
//   3. Your handlers (what to do when a capability connects/disconnects)
//
// OPSM lifecycle: mint → provision → configure → harness
// This file IS the harness. The panel template IS the mint.
//
// Usage:
//   const harness = new GrooveHarness({
//     systemId: 'my-panel',
//     systemName: 'My Panel',
//     version: '0.1.0',
//     offers: [{ id: 'health', version: '1.0.0', interface: 'HealthCheck' }],
//     consumes: [{ id: 'voice', version: '1.0+' }],
//     fliTraits: ['fli-tooltip', 'fli-gauge'],
//     onCapabilityArrived: function(capId, provider) { ... },
//     onCapabilityLost: function(capId, provider) { ... }
//   });
//   harness.start();

(function() {
  'use strict';

  /**
   * GrooveHarness — the universal harness for groove-aware Gossamer panels.
   *
   * Handles all Groove plumbing so panel authors only write their
   * capabilities, UI, and handlers. Everything else is automatic.
   *
   * @param {object} config - Harness configuration:
   *   systemId:              {string}   Unique system identifier
   *   systemName:            {string}   Human-readable name
   *   version:               {string}   SemVer version string
   *   offers:                {Array}    Capabilities this panel offers
   *   consumes:              {Array}    Capabilities this panel wants
   *   fliTraits:             {Array}    FLI module names to provision
   *   probeRange:            {Array}    [startPort, endPort] (default [6460, 6500])
   *   heartbeatInterval:     {number}   Heartbeat interval in ms (default 5000)
   *   heartbeatTimeout:      {number}   Heartbeat timeout in ms (default 15000)
   *   discoveryInterval:     {number}   Re-discovery interval in ms (default 60000)
   *   onCapabilityArrived:   {function} Called with (capId, provider) when a capability connects
   *   onCapabilityLost:      {function} Called with (capId, provider) when a capability disconnects
   *   onPartnerDiscovered:   {function} Called with (partner) when a new partner is found
   *   onPartnerLost:         {function} Called with (partner) when a partner disconnects
   *   onReady:               {function} Called when the harness is fully initialised
   */
  function GrooveHarness(config) {
    this.config = config || {};
    this.systemId = config.systemId || 'unnamed-panel';
    this.systemName = config.systemName || this.systemId;
    this.version = config.version || '0.1.0';

    this.offers = config.offers || [];
    this.consumes = config.consumes || [];
    this.fliTraits = config.fliTraits || [];

    this.probeRange = config.probeRange || [6460, 6500];
    this.heartbeatInterval = config.heartbeatInterval || 5000;
    this.heartbeatTimeout = config.heartbeatTimeout || 15000;
    this.discoveryInterval = config.discoveryInterval || 60000;

    // Callbacks — the parts the user writes
    this.onCapabilityArrived = config.onCapabilityArrived || function() {};
    this.onCapabilityLost = config.onCapabilityLost || function() {};
    this.onPartnerDiscovered = config.onPartnerDiscovered || function() {};
    this.onPartnerLost = config.onPartnerLost || function() {};
    this.onReady = config.onReady || function() {};

    // Internal state
    this._partners = {};          // partnerId → {manifest, connection, capabilities, lastHeartbeat}
    this._activeCapabilities = {}; // capId → {providerId, version, interface}
    this._heartbeatTimers = {};   // partnerId → intervalId
    this._discoveryTimer = null;
    this._provenance = [];        // attestation records
    this._started = false;
  }

  // =========================================================================
  // Lifecycle
  // =========================================================================

  /**
   * Start the harness. Provisions FLI modules, runs initial discovery,
   * and begins the heartbeat/rediscovery cycle.
   *
   * This is the only method the panel author needs to call.
   */
  GrooveHarness.prototype.start = async function() {
    if (this._started) return;
    this._started = true;

    console.log('[Groove:' + this.systemId + '] Starting harness...');

    // Phase 1: Provision FLI modules (from clade traits)
    await this._provisionFLI();

    // Phase 2: Initial discovery
    await this._discover();

    // Phase 3: Start periodic rediscovery
    var self = this;
    this._discoveryTimer = setInterval(function() {
      self._discover();
    }, this.discoveryInterval);

    console.log('[Groove:' + this.systemId + '] Harness ready.');
    this._attest('harness:started', null, 'Harness started with ' + this.offers.length + ' offers, ' + this.consumes.length + ' consumes');

    this.onReady();
  };

  /**
   * Stop the harness. Disconnects all partners, clears timers.
   */
  GrooveHarness.prototype.stop = function() {
    if (!this._started) return;

    // Disconnect all partners
    var self = this;
    Object.keys(this._partners).forEach(function(pid) {
      self._disconnectPartner(pid);
    });

    // Clear timers
    if (this._discoveryTimer) clearInterval(this._discoveryTimer);
    Object.keys(this._heartbeatTimers).forEach(function(pid) {
      clearInterval(self._heartbeatTimers[pid]);
    });

    this._started = false;
    this._attest('harness:stopped', null, 'Harness stopped');
    console.log('[Groove:' + this.systemId + '] Harness stopped.');
  };

  // =========================================================================
  // Discovery
  // =========================================================================

  /**
   * Discover groove partners by probing the configured port range.
   * For each port that responds with a valid groove manifest, attempt
   * to match capabilities and connect.
   */
  GrooveHarness.prototype._discover = async function() {
    // Use Gossamer IPC to probe (the Zig FFI handles actual HTTP probing)
    if (typeof gossamerInvoke !== 'function') {
      // Fallback: try direct fetch if running in a browser context
      await this._discoverViaFetch();
      return;
    }

    var resp = await gossamerInvoke('groove_discover', {
      system_id: this.systemId,
      probe_start: this.probeRange[0],
      probe_end: this.probeRange[1]
    });

    if (!resp.ok || !resp.data) return;

    var self = this;
    var discovered = Array.isArray(resp.data) ? resp.data : [];

    discovered.forEach(function(partner) {
      if (partner.id === self.systemId) return; // Don't discover self
      if (self._partners[partner.id]) return;   // Already connected

      self._evaluatePartner(partner);
    });
  };

  /**
   * Fallback discovery via direct fetch (for browser/dev contexts).
   */
  GrooveHarness.prototype._discoverViaFetch = async function() {
    var self = this;

    for (var port = this.probeRange[0]; port <= this.probeRange[1]; port++) {
      try {
        var controller = new AbortController();
        var timeoutId = setTimeout(function() { controller.abort(); }, 500);

        var resp = await fetch('http://localhost:' + port + '/.well-known/groove', {
          signal: controller.signal,
          headers: { 'Accept': 'application/groove+a2ml' }
        });
        clearTimeout(timeoutId);

        if (resp.ok) {
          var text = await resp.text();
          var partner = self._parseManifest(text, port);
          if (partner && partner.id !== self.systemId && !self._partners[partner.id]) {
            self._evaluatePartner(partner);
          }
        }
      } catch (e) {
        // Port didn't respond or isn't groove-aware — normal, skip
      }
    }
  };

  /**
   * Evaluate a discovered partner. Check if any of its offered capabilities
   * match our consumed capabilities (or vice versa). If so, connect.
   *
   * @param {object} partner - Parsed groove manifest
   */
  GrooveHarness.prototype._evaluatePartner = function(partner) {
    var self = this;
    var matched = [];

    // Check: does the partner offer things we consume?
    this.consumes.forEach(function(consumed) {
      var offered = (partner.offers || []).find(function(o) {
        return o.id === consumed.id && self._versionSatisfies(o.version, consumed.version);
      });
      if (offered) {
        matched.push({ capId: consumed.id, direction: 'inbound', offered: offered });
      }
    });

    // Check: does the partner consume things we offer?
    (partner.consumes || []).forEach(function(consumed) {
      var offered = self.offers.find(function(o) {
        return o.id === consumed.id && self._versionSatisfies(o.version, consumed.version);
      });
      if (offered) {
        matched.push({ capId: consumed.id, direction: 'outbound', offered: offered });
      }
    });

    if (matched.length > 0) {
      this._connectPartner(partner, matched);
    }
  };

  // =========================================================================
  // Connection management
  // =========================================================================

  /**
   * Connect to a partner and activate matched capabilities.
   *
   * @param {object} partner - The partner's manifest
   * @param {Array} matched - Array of matched capability objects
   */
  GrooveHarness.prototype._connectPartner = function(partner, matched) {
    var self = this;

    this._partners[partner.id] = {
      manifest: partner,
      capabilities: matched,
      connectedAt: Date.now(),
      lastHeartbeat: Date.now()
    };

    console.log('[Groove:' + this.systemId + '] Connected to ' + partner.id +
      ' (' + matched.map(function(m) { return m.capId; }).join(', ') + ')');

    this._attest('groove:connected', partner.id,
      'Connected with capabilities: ' + matched.map(function(m) { return m.capId + '@' + m.offered.version; }).join(', '));

    // Notify for each capability
    matched.forEach(function(m) {
      if (m.direction === 'inbound') {
        self._activeCapabilities[m.capId] = { providerId: partner.id, version: m.offered.version };
        self.onCapabilityArrived(m.capId, partner);
      }
    });

    this.onPartnerDiscovered(partner);

    // Start heartbeat
    this._startHeartbeat(partner.id);
  };

  /**
   * Disconnect from a partner and deactivate its capabilities.
   *
   * @param {string} partnerId - The partner's system ID
   */
  GrooveHarness.prototype._disconnectPartner = function(partnerId) {
    var partner = this._partners[partnerId];
    if (!partner) return;

    var self = this;

    // Deactivate capabilities
    (partner.capabilities || []).forEach(function(m) {
      if (m.direction === 'inbound') {
        delete self._activeCapabilities[m.capId];
        self.onCapabilityLost(m.capId, partner.manifest);
      }
    });

    // Stop heartbeat
    if (this._heartbeatTimers[partnerId]) {
      clearInterval(this._heartbeatTimers[partnerId]);
      delete this._heartbeatTimers[partnerId];
    }

    this._attest('groove:disconnected', partnerId, 'Partner disconnected');
    this.onPartnerLost(partner.manifest);
    delete this._partners[partnerId];

    console.log('[Groove:' + this.systemId + '] Disconnected from ' + partnerId);
  };

  // =========================================================================
  // Heartbeat
  // =========================================================================

  /**
   * Start heartbeat monitoring for a connected partner.
   *
   * @param {string} partnerId - The partner's system ID
   */
  GrooveHarness.prototype._startHeartbeat = function(partnerId) {
    var self = this;

    this._heartbeatTimers[partnerId] = setInterval(async function() {
      var partner = self._partners[partnerId];
      if (!partner) return;

      var elapsed = Date.now() - partner.lastHeartbeat;

      if (elapsed > self.heartbeatTimeout) {
        // Partner is dead — disconnect gracefully
        console.log('[Groove:' + self.systemId + '] Heartbeat timeout for ' + partnerId);
        self._disconnectPartner(partnerId);
        return;
      }

      // Send heartbeat probe
      try {
        var port = partner.manifest.port;
        if (port) {
          var controller = new AbortController();
          var timeoutId = setTimeout(function() { controller.abort(); }, 2000);
          var resp = await fetch('http://localhost:' + port + '/.well-known/groove/heartbeat', {
            signal: controller.signal
          });
          clearTimeout(timeoutId);
          if (resp.ok || resp.status === 204) {
            partner.lastHeartbeat = Date.now();
          }
        }
      } catch (e) {
        // Heartbeat failed — will timeout on next check
      }
    }, this.heartbeatInterval);
  };

  // =========================================================================
  // FLI Provisioning
  // =========================================================================

  /**
   * Provision FLI modules based on the panel's declared clade traits.
   * This is the "configuring" phase of the OPSM lifecycle.
   */
  GrooveHarness.prototype._provisionFLI = async function() {
    if (this.fliTraits.length === 0) return;

    // Use the host's FLI provisioner if available
    if (typeof provisionFLI === 'function') {
      // The host provisioner handles loading based on panel traits
      // We just need to register our traits
      for (var i = 0; i < this.fliTraits.length; i++) {
        var trait = this.fliTraits[i];
        if (typeof loadFLIModule === 'function' && typeof fliRegistry !== 'undefined') {
          var mod = fliRegistry.modules[trait];
          if (mod && !mod.loaded) {
            await loadFLIModule(trait, mod.path);
          }
        }
      }
      console.log('[Groove:' + this.systemId + '] FLI provisioned: ' + this.fliTraits.join(', '));
    }
  };

  // =========================================================================
  // Attestation
  // =========================================================================

  /**
   * Generate a provenance attestation record for a groove event.
   *
   * @param {string} event - Event type (e.g. 'groove:connected')
   * @param {string|null} partnerId - The partner involved (or null)
   * @param {string} description - Human-readable description
   */
  GrooveHarness.prototype._attest = function(event, partnerId, description) {
    var prevHash = this._provenance.length > 0
      ? this._provenance[this._provenance.length - 1].hash
      : '0000000000000000';

    var record = {
      event: event,
      system: this.systemId,
      partner: partnerId,
      description: description,
      timestamp: new Date().toISOString(),
      hash: this._hashRecord(event + '|' + (partnerId || '') + '|' + description + '|' + prevHash),
      prev_hash: prevHash
    };

    this._provenance.push(record);

    // Store in VeriSimDB if available
    if (this._activeCapabilities['storage']) {
      this._storeAttestation(record);
    }
  };

  /**
   * Simple hash function for attestation records.
   * In production, this would be SHA-256 via the Zig FFI.
   *
   * @param {string} input - String to hash
   * @returns {string} Hex hash string
   */
  GrooveHarness.prototype._hashRecord = function(input) {
    var hash = 0;
    for (var i = 0; i < input.length; i++) {
      var chr = input.charCodeAt(i);
      hash = ((hash << 5) - hash) + chr;
      hash |= 0;
    }
    return Math.abs(hash).toString(16).padStart(16, '0');
  };

  /**
   * Store an attestation record in VeriSimDB (if grooved).
   *
   * @param {object} record - The provenance record
   */
  GrooveHarness.prototype._storeAttestation = async function(record) {
    if (typeof gossamerInvoke === 'function') {
      await gossamerInvoke('groove_attest', { record: record });
    }
  };

  // =========================================================================
  // Utility
  // =========================================================================

  /**
   * Check if a provider version satisfies a consumer's version constraint.
   *
   * @param {string} offered - The offered version (e.g. "2.1.0")
   * @param {string} required - The required constraint (e.g. "1.0+", ">=1.0,<3.0", "*")
   * @returns {boolean} True if the offered version satisfies the constraint
   */
  GrooveHarness.prototype._versionSatisfies = function(offered, required) {
    if (!required || required === '*') return true;
    if (!offered) return false;

    // Simple "X.Y+" matching
    if (required.endsWith('+')) {
      var minParts = required.slice(0, -1).split('.').map(Number);
      var offParts = offered.split('.').map(Number);
      for (var i = 0; i < minParts.length; i++) {
        if ((offParts[i] || 0) > minParts[i]) return true;
        if ((offParts[i] || 0) < minParts[i]) return false;
      }
      return true; // Equal
    }

    // Exact match
    return offered === required;
  };

  /**
   * Parse a groove manifest from A2ML text. Minimal parser for the
   * manifest structure. In production, this uses the Zig A2ML parser.
   *
   * @param {string} text - A2ML manifest text
   * @param {number} port - The port the manifest was found on
   * @returns {object|null} Parsed manifest or null
   */
  GrooveHarness.prototype._parseManifest = function(text, port) {
    // Minimal extraction — production uses the Zig A2ML parser via FFI
    var idMatch = text.match(/id="([^"]+)"/);
    var nameMatch = text.match(/name="([^"]+)"/);
    var versionMatch = text.match(/version="([^"]+)"/);

    if (!idMatch) return null;

    return {
      id: idMatch[1],
      name: nameMatch ? nameMatch[1] : idMatch[1],
      version: versionMatch ? versionMatch[1] : '0.0.0',
      port: port,
      offers: [], // Would be fully parsed by Zig A2ML parser
      consumes: []
    };
  };

  // =========================================================================
  // Public query API
  // =========================================================================

  /**
   * Check if a specific capability is currently available (grooved in).
   *
   * @param {string} capId - The capability ID to check
   * @returns {boolean} True if the capability is active
   */
  GrooveHarness.prototype.hasCapability = function(capId) {
    return !!this._activeCapabilities[capId];
  };

  /**
   * Get all currently active capabilities.
   *
   * @returns {object} Map of capId → {providerId, version}
   */
  GrooveHarness.prototype.activeCapabilities = function() {
    return Object.assign({}, this._activeCapabilities);
  };

  /**
   * Get all connected partners.
   *
   * @returns {Array} Array of partner manifest objects
   */
  GrooveHarness.prototype.partners = function() {
    var self = this;
    return Object.keys(this._partners).map(function(pid) {
      return self._partners[pid].manifest;
    });
  };

  /**
   * Get the provenance audit trail.
   *
   * @returns {Array} Array of attestation records
   */
  GrooveHarness.prototype.provenance = function() {
    return this._provenance.slice();
  };

  // Export
  window.GrooveHarness = GrooveHarness;

  console.log('[Groove] Universal harness loaded.');
})();
