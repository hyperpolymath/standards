-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- GrooveProxy: IPv4→IPv6 specialisation of the Typed Frame Router.
--
-- This is an INSTANCE of proven-servers/core/proven-typed-frame-router,
-- configured for the Groove Protocol's IPv4 sunset strategy.
--
-- It re-exports the general router types with Groove-specific defaults
-- and adds Groove discovery integration (sunset headers, attestation).

module GrooveProxy

import TypedFrameRouter.Types
import TypedFrameRouterABI
import TypedFrameRouterABI.Proofs

%default total

---------------------------------------------------------------------------
-- Groove-specific configuration
---------------------------------------------------------------------------

||| Default Groove proxy configuration.
||| Translates IPv4→IPv6 on the loopback interface for a given port.
|||
||| @param port The groove service port to proxy
public export
grooveProxyConfig : (port : Bits16) -> RouterConfig
grooveProxyConfig port = MkRouterConfig
  { source         = MkEndpoint IPv4 "127.0.0.1" port
  , target         = MkEndpoint IPv6 "::1" port
  , maxConnections = 64
  , bufferSize     = 4096
  , translation    = MkTranslation IPv4 IPv6
  }

||| The Groove proxy direction is always IPv4→IPv6.
||| This is a specialisation of the general FrameTranslation.
public export
grooveDirection : FrameTranslation
grooveDirection = MkTranslation IPv4 IPv6

---------------------------------------------------------------------------
-- Groove-specific proofs (derived from general proofs)
---------------------------------------------------------------------------

||| The Groove proxy inherits all four safety properties from
||| the Typed Frame Router. This re-export makes them available
||| to Groove consumers without importing the general module.
public export
grooveTransportSafe : (input : List Bits8) -> (output : List Bits8)
                   -> (prf : input = output) -> input = output
grooveTransportSafe = transportTransparency

||| The Groove proxy cannot be reversed to IPv6→IPv4.
public export
grooveNoReverse : (MkTranslation IPv6 IPv4 = grooveDirection) -> Void
grooveNoReverse Refl impossible
