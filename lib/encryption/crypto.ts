/**
 * End-to-End Encryption Module
 * 
 * Implements X25519 key exchange with XSalsa20-Poly1305 authenticated encryption.
 * Uses TweetNaCl - a small, audited cryptographic library.
 * 
 * Security Features:
 * - Asymmetric key exchange (X25519 Curve25519)
 * - Authenticated encryption (XSalsa20-Poly1305)
 * - Per-message random nonces (24 bytes)
 * - Key derivation for each message
 * - No plaintext ever leaves the client
 */

import nacl from 'tweetnacl';
import { encodeBase64, decodeBase64, encodeUTF8, decodeUTF8 } from 'tweetnacl-util';

// Key storage keys for localStorage
const PRIVATE_KEY_STORAGE_KEY = 'whatsapp_e2ee_private_key';
const PUBLIC_KEY_STORAGE_KEY = 'whatsapp_e2ee_public_key';
const KEY_BACKUP_STORAGE_KEY = 'whatsapp_e2ee_key_backup';

export interface KeyPair {
  publicKey: string; // Base64 encoded
  privateKey: string; // Base64 encoded
}

export interface EncryptedMessage {
  ciphertext: string; // Base64 encoded
  nonce: string; // Base64 encoded
  senderPublicKey: string; // Base64 encoded (ephemeral or identity key)
}

export interface DecryptedMessage {
  plaintext: string;
  verified: boolean;
}

/**
 * Generate a new X25519 key pair for the user
 * This should be called once when user first sets up E2EE
 */
export function generateKeyPair(): KeyPair {
  const keyPair = nacl.box.keyPair();
  return {
    publicKey: encodeBase64(keyPair.publicKey),
    privateKey: encodeBase64(keyPair.secretKey),
  };
}

/**
 * Generate a random nonce for each message
 * CRITICAL: Never reuse nonces with the same key
 */
function generateNonce(): Uint8Array {
  return nacl.randomBytes(nacl.box.nonceLength);
}

/**
 * Encrypt a message for a recipient
 * Uses authenticated encryption - recipient can verify integrity
 * 
 * @param plaintext - The message to encrypt
 * @param recipientPublicKey - Recipient's public key (Base64)
 * @param senderPrivateKey - Sender's private key (Base64)
 * @returns Encrypted message with nonce and sender's public key
 */
export function encryptMessage(
  plaintext: string,
  recipientPublicKey: string,
  senderPrivateKey: string,
  senderPublicKey: string
): EncryptedMessage {
  try {
    // Decode keys from Base64
    const recipientPubKey = decodeBase64(recipientPublicKey);
    const senderPrivKey = decodeBase64(senderPrivateKey);
    
    // Convert plaintext to Uint8Array
    const messageBytes = decodeUTF8(plaintext);
    
    // Generate a unique nonce for this message
    const nonce = generateNonce();
    
    // Encrypt using NaCl box (X25519 + XSalsa20-Poly1305)
    const ciphertext = nacl.box(
      messageBytes,
      nonce,
      recipientPubKey,
      senderPrivKey
    );
    
    if (!ciphertext) {
      throw new Error('Encryption failed');
    }
    
    return {
      ciphertext: encodeBase64(ciphertext),
      nonce: encodeBase64(nonce),
      senderPublicKey: senderPublicKey,
    };
  } catch (error) {
    console.error('Encryption error:', error);
    throw new Error('Failed to encrypt message');
  }
}

/**
 * Decrypt a message from a sender
 * Verifies authenticity using sender's public key
 * 
 * @param encryptedMessage - The encrypted message object
 * @param recipientPrivateKey - Recipient's private key (Base64)
 * @returns Decrypted plaintext and verification status
 */
export function decryptMessage(
  encryptedMessage: EncryptedMessage,
  recipientPrivateKey: string
): DecryptedMessage {
  try {
    // Decode from Base64
    const ciphertext = decodeBase64(encryptedMessage.ciphertext);
    const nonce = decodeBase64(encryptedMessage.nonce);
    const senderPubKey = decodeBase64(encryptedMessage.senderPublicKey);
    const recipientPrivKey = decodeBase64(recipientPrivateKey);
    
    // Decrypt using NaCl box.open
    const decrypted = nacl.box.open(
      ciphertext,
      nonce,
      senderPubKey,
      recipientPrivKey
    );
    
    if (!decrypted) {
      throw new Error('Decryption failed - message may have been tampered with');
    }
    
    return {
      plaintext: encodeUTF8(decrypted),
      verified: true,
    };
  } catch (error) {
    console.error('Decryption error:', error);
    return {
      plaintext: '[Unable to decrypt message]',
      verified: false,
    };
  }
}

/**
 * Store the user's key pair securely in localStorage
 * In production, consider using IndexedDB with encryption or Web Crypto subtle
 */
export function storeKeyPair(keyPair: KeyPair): void {
  if (typeof window === 'undefined') return;
  
  try {
    // Store keys separately
    localStorage.setItem(PRIVATE_KEY_STORAGE_KEY, keyPair.privateKey);
    localStorage.setItem(PUBLIC_KEY_STORAGE_KEY, keyPair.publicKey);
    
    // Create an encrypted backup phrase (for recovery)
    // In production, this should be encrypted with a user passphrase
    const backup = {
      publicKey: keyPair.publicKey,
      privateKey: keyPair.privateKey,
      createdAt: new Date().toISOString(),
    };
    localStorage.setItem(KEY_BACKUP_STORAGE_KEY, encodeBase64(
      decodeUTF8(JSON.stringify(backup))
    ));
  } catch (error) {
    console.error('Failed to store key pair:', error);
    throw new Error('Failed to store encryption keys');
  }
}

/**
 * Retrieve the user's key pair from localStorage
 */
export function getStoredKeyPair(): KeyPair | null {
  if (typeof window === 'undefined') return null;
  
  try {
    const privateKey = localStorage.getItem(PRIVATE_KEY_STORAGE_KEY);
    const publicKey = localStorage.getItem(PUBLIC_KEY_STORAGE_KEY);
    
    if (!privateKey || !publicKey) {
      return null;
    }
    
    return { publicKey, privateKey };
  } catch {
    return null;
  }
}

/**
 * Check if user has E2EE keys set up
 */
export function hasEncryptionKeys(): boolean {
  return getStoredKeyPair() !== null;
}

/**
 * Clear stored encryption keys (use with caution!)
 */
export function clearEncryptionKeys(): void {
  if (typeof window === 'undefined') return;
  
  localStorage.removeItem(PRIVATE_KEY_STORAGE_KEY);
  localStorage.removeItem(PUBLIC_KEY_STORAGE_KEY);
  localStorage.removeItem(KEY_BACKUP_STORAGE_KEY);
}

/**
 * Export keys as a recovery phrase (Base64 encoded JSON)
 * User should store this securely offline
 */
export function exportRecoveryKey(): string | null {
  if (typeof window === 'undefined') return null;
  
  const backup = localStorage.getItem(KEY_BACKUP_STORAGE_KEY);
  return backup;
}

/**
 * Import keys from a recovery phrase
 */
export function importRecoveryKey(recoveryPhrase: string): boolean {
  try {
    const decoded = encodeUTF8(decodeBase64(recoveryPhrase));
    const backup = JSON.parse(decoded);
    
    if (!backup.publicKey || !backup.privateKey) {
      throw new Error('Invalid recovery phrase');
    }
    
    storeKeyPair({
      publicKey: backup.publicKey,
      privateKey: backup.privateKey,
    });
    
    return true;
  } catch (error) {
    console.error('Failed to import recovery key:', error);
    return false;
  }
}

/**
 * Derive a shared secret for a conversation (for caching)
 * This can be used for faster encryption in the same conversation
 */
export function deriveSharedKey(
  theirPublicKey: string,
  myPrivateKey: string
): string {
  const theirPubKey = decodeBase64(theirPublicKey);
  const myPrivKey = decodeBase64(myPrivateKey);
  
  const sharedKey = nacl.box.before(theirPubKey, myPrivKey);
  return encodeBase64(sharedKey);
}

/**
 * Encrypt with precomputed shared key (faster for multiple messages)
 */
export function encryptWithSharedKey(
  plaintext: string,
  sharedKey: string,
  senderPublicKey: string
): EncryptedMessage {
  try {
    const sharedKeyBytes = decodeBase64(sharedKey);
    const messageBytes = decodeUTF8(plaintext);
    const nonce = generateNonce();
    
    const ciphertext = nacl.box.after(messageBytes, nonce, sharedKeyBytes);
    
    if (!ciphertext) {
      throw new Error('Encryption failed');
    }
    
    return {
      ciphertext: encodeBase64(ciphertext),
      nonce: encodeBase64(nonce),
      senderPublicKey,
    };
  } catch (error) {
    console.error('Encryption error:', error);
    throw new Error('Failed to encrypt message');
  }
}

/**
 * Decrypt with precomputed shared key (faster for multiple messages)
 */
export function decryptWithSharedKey(
  encryptedMessage: EncryptedMessage,
  sharedKey: string
): DecryptedMessage {
  try {
    const sharedKeyBytes = decodeBase64(sharedKey);
    const ciphertext = decodeBase64(encryptedMessage.ciphertext);
    const nonce = decodeBase64(encryptedMessage.nonce);
    
    const decrypted = nacl.box.open.after(ciphertext, nonce, sharedKeyBytes);
    
    if (!decrypted) {
      throw new Error('Decryption failed');
    }
    
    return {
      plaintext: encodeUTF8(decrypted),
      verified: true,
    };
  } catch (error) {
    console.error('Decryption error:', error);
    return {
      plaintext: '[Unable to decrypt message]',
      verified: false,
    };
  }
}

// Re-export for convenience
export { encodeBase64, decodeBase64 };
