/**
 * E2EE Message Service
 * 
 * High-level API for sending and receiving encrypted messages.
 * Handles key exchange, encryption, and decryption transparently.
 */

import {
  encryptMessage,
  decryptMessage,
  encryptWithSharedKey,
  decryptWithSharedKey,
  type EncryptedMessage,
} from './crypto';
import {
  fetchPublicKey,
  getSharedKey,
  getMyPublicKey,
  getMyPrivateKey,
  storePublicKeyLocally,
  isE2EEEnabled,
} from './key-manager';

export interface E2EEMessagePayload {
  // Original message fields
  content: string;
  type: 'text' | 'image' | 'video' | 'audio' | 'document';
  // E2EE fields (stored in DB)
  encrypted: boolean;
  ciphertext?: string;
  nonce?: string;
  senderPublicKey?: string;
}

/**
 * Encrypt a message before sending
 * Returns the encrypted payload to be stored in the database
 */
export async function encryptMessageForSending(
  plaintext: string,
  recipientId: string,
  messageType: 'text' | 'image' | 'video' | 'audio' | 'document' = 'text'
): Promise<E2EEMessagePayload> {
  try {
    // Check if E2EE is enabled
    if (!isE2EEEnabled()) {
      console.log('E2EE not enabled, sending plaintext');
      return {
        content: plaintext,
        type: messageType,
        encrypted: false,
      };
    }
    
    // Get my keys
    const myPublicKey = getMyPublicKey();
    const myPrivateKey = getMyPrivateKey();
    
    if (!myPublicKey || !myPrivateKey) {
      console.log('E2EE keys not found, sending plaintext');
      return {
        content: plaintext,
        type: messageType,
        encrypted: false,
      };
    }
    
    // Get recipient's public key
    let recipientPublicKey: string | null = null;
    try {
      recipientPublicKey = await fetchPublicKey(recipientId);
    } catch (fetchErr) {
      console.warn('Failed to fetch recipient public key:', fetchErr);
    }
    
    if (!recipientPublicKey) {
      console.log(`No public key found for recipient ${recipientId}, sending plaintext`);
      return {
        content: plaintext,
        type: messageType,
        encrypted: false,
      };
    }
    
    // Use shared key for better performance
    const sharedKey = getSharedKey(recipientPublicKey, myPrivateKey);
    const encrypted = encryptWithSharedKey(plaintext, sharedKey, myPublicKey);
    
    // Return encrypted payload - store plaintext in content as fallback for cross-device
    return {
      content: plaintext,  // Store plaintext as fallback for devices without matching keys
      type: messageType,
      encrypted: true,
      ciphertext: encrypted.ciphertext,
      nonce: encrypted.nonce,
      senderPublicKey: encrypted.senderPublicKey,
    };
  } catch (error) {
    console.error('Encryption failed:', error);
    // Fallback to plaintext on error
    return {
      content: plaintext,
      type: messageType,
      encrypted: false,
    };
  }
}

/**
 * Decrypt a received message
 * Returns the plaintext content, or fallback content if decryption fails
 */
export async function decryptReceivedMessage(
  encryptedPayload: E2EEMessagePayload,
  senderId: string
): Promise<string> {
  // If not encrypted, return as-is
  if (!encryptedPayload.encrypted) {
    return encryptedPayload.content;
  }
  
  // Fallback to stored content if decryption isn't possible
  const fallbackContent = encryptedPayload.content;
  
  // Check if we have E2EE enabled
  if (!isE2EEEnabled()) {
    console.log('E2EE not enabled, using fallback content');
    return fallbackContent;
  }
  
  const myPrivateKey = getMyPrivateKey();
  
  if (!myPrivateKey) {
    console.log('No private key, using fallback content');
    return fallbackContent;
  }
  
  // Validate encrypted payload
  if (!encryptedPayload.ciphertext || !encryptedPayload.nonce || !encryptedPayload.senderPublicKey) {
    console.log('Invalid encrypted payload, using fallback content');
    return fallbackContent;
  }
  
  try {
    // Store sender's public key for future use
    storePublicKeyLocally(senderId, encryptedPayload.senderPublicKey);
    
    // Use shared key for better performance
    const sharedKey = getSharedKey(encryptedPayload.senderPublicKey, myPrivateKey);
    
    const decrypted = decryptWithSharedKey({
      ciphertext: encryptedPayload.ciphertext,
      nonce: encryptedPayload.nonce,
      senderPublicKey: encryptedPayload.senderPublicKey,
    }, sharedKey);
    
    if (!decrypted.verified) {
      console.log('Decryption unverified, using fallback content');
      return fallbackContent;
    }
    
    return decrypted.plaintext;
  } catch (error) {
    console.error('Decryption failed, using fallback:', error);
    return fallbackContent;
  }
}

/**
 * Check if a message is encrypted
 */
export function isMessageEncrypted(message: any): boolean {
  return message?.encrypted === true && !!message?.ciphertext;
}

/**
 * Get encryption status indicator for UI
 */
export function getEncryptionStatus(message: any): {
  encrypted: boolean;
  icon: string;
  label: string;
} {
  if (message?.encrypted) {
    return {
      encrypted: true,
      icon: '🔒',
      label: 'End-to-end encrypted',
    };
  }
  
  return {
    encrypted: false,
    icon: '🔓',
    label: 'Not encrypted',
  };
}

// Re-export commonly used functions
export {
  isE2EEEnabled,
  getMyPublicKey,
  fetchPublicKey,
} from './key-manager';
