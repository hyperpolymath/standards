# Technical Vignette: Blockchain Lineage Registration

## Scenario: Immutable Attribution via Decentralized Ledger

**Project**: Palimpsest Lineage Registry (blockchain-based attribution system)
**Technology**: IPFS + Ethereum smart contracts
**Purpose**: Quantum-proof, immutable cultural lineage tracking
**Outcome**: Permanent, tamper-proof attribution for creative works

---

## The Challenge

Traditional attribution systems are vulnerable:
- Metadata can be stripped from files
- Websites can be taken down
- Centralized databases can be compromised
- Future technologies (quantum computing) might break current cryptography

Palimpsest's vision: **Quantum-proof traceability**—attribution that survives technological change, censorship, and time.

**Solution**: Blockchain-based lineage registry using content-addressable storage (IPFS) and smart contracts (Ethereum).

---

## Technical Architecture

**Components:**

1. **IPFS (InterPlanetary File System)**: Decentralized storage for creative works
2. **Ethereum Smart Contract**: Immutable registry linking creators to content hashes
3. **Palimpsest Metadata Standard**: JSON-LD format for attribution
4. **Web Interface**: User-friendly registration and verification portal

### 1. Content Storage (IPFS)

```bash
# Upload work to IPFS
ipfs add my-poem.txt
# Returns: QmXyZ... (content hash - unique identifier based on file content)

# Upload metadata
ipfs add palimpsest-metadata.json
# Returns: QmAbC... (metadata hash)
```

**Why IPFS?**
- Content-addressable: Hash is derived from content (tamper-proof)
- Decentralized: No single point of failure
- Permanent: Content persists as long as anyone pins it
- Verifiable: Anyone can retrieve and verify content matches its hash

### 2. Smart Contract (Ethereum)

```solidity
// SPDX-License-Identifier: MPL-2.0-or-later
pragma solidity ^0.8.0;

/**
 * @title PalimpsestRegistry
 * @dev Immutable registry for Palimpsest-licensed creative works
 */
contract PalimpsestRegistry {

    struct Registration {
        address creator;              // Ethereum address of creator
        string ipfsContentHash;       // IPFS hash of the creative work
        string ipfsMetadataHash;      // IPFS hash of Palimpsest metadata
        uint256 timestamp;            // Block timestamp of registration
        string licenseVersion;        // Palimpsest license version (e.g., "0.4")
        string culturalContext;       // Brief cultural context (on-chain for visibility)
        bool aiConsentGiven;          // AI training consent flag
    }

    // Mapping: IPFS content hash => Registration
    mapping(string => Registration) public registry;

    // Mapping: Creator address => array of content hashes
    mapping(address => string[]) public creatorWorks;

    // Events for transparency
    event WorkRegistered(
        address indexed creator,
        string ipfsContentHash,
        string licenseVersion,
        uint256 timestamp
    );

    event AIConsentUpdated(
        string ipfsContentHash,
        bool consentGiven,
        uint256 timestamp
    );

    /**
     * @dev Register a new Palimpsest-licensed work
     * @param _contentHash IPFS hash of the creative work
     * @param _metadataHash IPFS hash of Palimpsest metadata JSON
     * @param _culturalContext Brief description (max 256 chars, stored on-chain)
     * @param _aiConsent AI training consent (true/false)
     */
    function registerWork(
        string memory _contentHash,
        string memory _metadataHash,
        string memory _culturalContext,
        bool _aiConsent
    ) public {
        // Ensure work isn't already registered
        require(registry[_contentHash].timestamp == 0, "Work already registered");

        // Create registration
        registry[_contentHash] = Registration({
            creator: msg.sender,
            ipfsContentHash: _contentHash,
            ipfsMetadataHash: _metadataHash,
            timestamp: block.timestamp,
            licenseVersion: "0.4",
            culturalContext: _culturalContext,
            aiConsentGiven: _aiConsent
        });

        // Add to creator's works
        creatorWorks[msg.sender].push(_contentHash);

        emit WorkRegistered(msg.sender, _contentHash, "0.4", block.timestamp);
    }

    /**
     * @dev Update AI training consent (only creator can update)
     * @param _contentHash IPFS hash of the work
     * @param _consent New consent status
     */
    function updateAIConsent(string memory _contentHash, bool _consent) public {
        require(registry[_contentHash].creator == msg.sender, "Only creator can update consent");

        registry[_contentHash].aiConsentGiven = _consent;
        emit AIConsentUpdated(_contentHash, _consent, block.timestamp);
    }

    /**
     * @dev Verify attribution for a work
     * @param _contentHash IPFS hash of the work
     * @return creator, timestamp, AI consent status
     */
    function verifyAttribution(string memory _contentHash)
        public
        view
        returns (address creator, uint256 timestamp, bool aiConsent)
    {
        Registration memory reg = registry[_contentHash];
        require(reg.timestamp != 0, "Work not registered");

        return (reg.creator, reg.timestamp, reg.aiConsentGiven);
    }

    /**
     * @dev Get all works by a creator
     * @param _creator Ethereum address of creator
     * @return Array of IPFS content hashes
     */
    function getCreatorWorks(address _creator) public view returns (string[] memory) {
        return creatorWorks[_creator];
    }

    /**
     * @dev Get full registration details
     * @param _contentHash IPFS hash of the work
     */
    function getRegistration(string memory _contentHash)
        public
        view
        returns (Registration memory)
    {
        require(registry[_contentHash].timestamp != 0, "Work not registered");
        return registry[_contentHash];
    }
}
```

### 3. Web Interface (Registration Portal)

```javascript
// Frontend: register-work.js
import { create } from 'ipfs-http-client';
import { ethers } from 'ethers';

class PalimpsestRegistrar {
    constructor() {
        this.ipfs = create({ url: 'https://ipfs.infura.io:5001' });
        this.contractAddress = '0x...'; // Deployed contract address
        this.contractABI = [...]; // Contract ABI
    }

    async registerWork(file, metadata) {
        try {
            // 1. Upload file to IPFS
            console.log('Uploading work to IPFS...');
            const fileResult = await this.ipfs.add(file);
            const contentHash = fileResult.path;
            console.log(`Work uploaded: ${contentHash}`);

            // 2. Upload metadata to IPFS
            console.log('Uploading metadata to IPFS...');
            const metadataJSON = JSON.stringify(metadata);
            const metadataResult = await this.ipfs.add(metadataJSON);
            const metadataHash = metadataResult.path;
            console.log(`Metadata uploaded: ${metadataHash}`);

            // 3. Connect to Ethereum wallet (MetaMask)
            if (!window.ethereum) {
                throw new Error('MetaMask not installed');
            }

            await window.ethereum.request({ method: 'eth_requestAccounts' });
            const provider = new ethers.providers.Web3Provider(window.ethereum);
            const signer = provider.getSigner();

            // 4. Interact with smart contract
            const contract = new ethers.Contract(
                this.contractAddress,
                this.contractABI,
                signer
            );

            // 5. Register on blockchain
            console.log('Registering on blockchain...');
            const tx = await contract.registerWork(
                contentHash,
                metadataHash,
                metadata.culturalContext.substring(0, 256), // On-chain (truncated)
                metadata.aiConsentGiven
            );

            console.log('Transaction submitted:', tx.hash);
            await tx.wait(); // Wait for confirmation
            console.log('Registration confirmed!');

            // 6. Return registration details
            return {
                success: true,
                contentHash,
                metadataHash,
                transactionHash: tx.hash,
                explorerUrl: `https://etherscan.io/tx/${tx.hash}`,
                ipfsUrl: `https://ipfs.io/ipfs/${contentHash}`,
                timestamp: new Date().toISOString()
            };

        } catch (error) {
            console.error('Registration failed:', error);
            return {
                success: false,
                error: error.message
            };
        }
    }

    async verifyWork(contentHash) {
        // Query smart contract for attribution
        const provider = new ethers.providers.Web3Provider(window.ethereum);
        const contract = new ethers.Contract(
            this.contractAddress,
            this.contractABI,
            provider
        );

        const registration = await contract.getRegistration(contentHash);

        return {
            creator: registration.creator,
            timestamp: new Date(registration.timestamp * 1000).toISOString(),
            licenseVersion: registration.licenseVersion,
            culturalContext: registration.culturalContext,
            aiConsent: registration.aiConsentGiven,
            metadataUrl: `https://ipfs.io/ipfs/${registration.ipfsMetadataHash}`
        };
    }
}

// Usage
const registrar = new PalimpsestRegistrar();

// Register work
const result = await registrar.registerWork(fileBlob, {
    title: "Migration Dreams",
    author: "Amara Osman",
    culturalContext: "Syrian refugee narrative poem about longing and resilience",
    emotionalIntent: "To honor refugees' emotional journeys",
    aiConsentGiven: false
});

console.log('Registered:', result.contentHash);
console.log('View on IPFS:', result.ipfsUrl);
console.log('Blockchain tx:', result.explorerUrl);
```

---

## Real-World Use Case

**Scenario**: Syrian poet registers work on blockchain

**Step 1**: Poet uploads poem and metadata

```bash
$ ipfs add my-poem.txt
added QmPoem123... my-poem.txt
```

**Step 2**: Registers on Ethereum smart contract

Transaction creates permanent record:
- Creator: 0xPoet...
- Content Hash: QmPoem123...
- Timestamp: 2024-11-23 14:30:00 UTC
- License: Palimpsest v0.4
- Cultural Context: "Syrian refugee narrative"
- AI Consent: False

**Step 3**: Verification

Anyone can verify:
1. Retrieve poem from IPFS using content hash
2. Query smart contract for attribution
3. Confirm creator's Ethereum address
4. Check AI consent status
5. Access full metadata from IPFS

**Tamper-proof benefits:**
- Content hash changes if poem is altered (instant detection)
- Blockchain timestamp proves "first registration" (prior art)
- Decentralized storage prevents takedowns
- Smart contract is immutable (can't be changed or deleted)
- Survives website shutdowns, company failures, government censorship

---

## Quantum-Proof Strategy

Current cryptography (SHA-256, ECDSA) could be broken by quantum computers. Palimpsest Registry's future-proofing:

**1. Content Hashes**:
- IPFS uses SHA-256 (vulnerable to quantum attacks)
- Plan: Migrate to post-quantum hash functions (e.g., SHA-3, BLAKE3)
- Backwards compatible: Old hashes remain valid, new registrations use stronger hashes

**2. Blockchain Signatures**:
- Ethereum uses ECDSA (vulnerable)
- Plan: Ethereum 2.0 roadmap includes post-quantum signature schemes
- Palimpsest contract designed to be upgradeable to new signature standards

**3. Migration Strategy**:
- All registrations include metadata hash pointing to full attribution data
- If quantum computers break current hashes, re-hash content with post-quantum algorithms
- Original timestamps and attribution remain provable via blockchain history

---

## Benefits vs. Limitations

**Benefits:**
- Immutable attribution (can't be deleted or altered)
- Decentralized (no single point of failure)
- Transparent (anyone can verify)
- Timestamped (proves creation date)
- Censorship-resistant (governments can't take down)
- Survives organizational collapse

**Limitations:**
- Costs gas fees (~$5-50 per registration depending on Ethereum prices)
- Requires technical knowledge (Ethereum wallet, IPFS)
- Doesn't prevent copying (only proves attribution)
- Environmental concerns (Ethereum energy use, though post-Merge this is minimal)
- Not legally recognized in all jurisdictions (still emerging technology)

---

## Best Practices

### For Creators

**Do:**
- Register high-value or culturally significant works
- Include comprehensive metadata in IPFS
- Use a secure Ethereum wallet
- Back up your private keys
- Update AI consent as needed

**Don't:**
- Register works you don't have rights to
- Lose your private keys (you'll lose access)
- Forget to pin IPFS content (or it may become inaccessible)
- Register extremely personal content publicly (blockchain is permanent)

### For Platforms

**Do:**
- Integrate blockchain verification into attribution systems
- Display blockchain registration badges
- Provide tools for creators to register easily
- Verify content hashes before use

**Don't:**
- Ignore blockchain-registered works
- Claim blockchain registration is unnecessary
- Strip attribution from blockchain-registered content

---

## Discussion Questions

1. Should blockchain registration be required for Palimpsest licensing? Or remain optional?

2. Do gas fees create inequality (only wealthy creators can afford blockchain registration)?

3. Is blockchain's permanence a feature or a bug? What if a creator wants to delete something they registered?

4. Should there be multiple blockchain registries? Or one canonical Palimpsest registry?

5. How do we handle the transition to post-quantum cryptography without losing current registrations?

---

**License Note**: This vignette is licensed under CC BY-SA 4.0. The Palimpsest Lineage Registry smart contract and examples are fictional but based on real blockchain and IPFS technologies. Contract code is illustrative; production use requires security audits.
