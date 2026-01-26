/**
 * E2EE Module - Public API
 * 
 * End-to-end encryption for WhatsApp Clone
 * Messages are encrypted on sender device and only decrypted on recipient device.
 * The server (Supabase) only ever sees ciphertext.
 */

// Core crypto functions
export {
  generateKeyPair,
  encryptMessage,
  decryptMessage,
  hasEncryptionKeys,
  getStoredKeyPair,
  storeKeyPair,
  clearEncryptionKeys,
  exportRecoveryKey,
  importRecoveryKey,
  type KeyPair,
  type EncryptedMessage,
  type DecryptedMessage,
} from './crypto';

// Key management
export {
  initializeE2EE,
  uploadPublicKey,
  fetchPublicKey,
  getMyPublicKey,
  getMyPrivateKey,
  isE2EEEnabled,
  clearKeyCache,
  storePublicKeyLocally,
} from './key-manager';

// High-level message service
export {
  encryptMessageForSending,
  decryptReceivedMessage,
  isMessageEncrypted,
  getEncryptionStatus,
  type E2EEMessagePayload,
} from './message-service';
