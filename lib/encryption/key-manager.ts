/**
 * E2EE Key Management Service
 * 
 * Handles key generation, storage, and synchronization with Supabase.
 * Public keys are stored in Supabase for key exchange.
 * Private keys NEVER leave the client device.
 */

import { supabase, isSupabaseConfigured } from '@/lib/supabase/client';
import {
  generateKeyPair,
  getStoredKeyPair,
  storeKeyPair,
  hasEncryptionKeys,
  deriveSharedKey,
  type KeyPair,
} from './crypto';

// Cache for recipient public keys
const publicKeyCache = new Map<string, string>();

// Cache for derived shared keys (for faster encryption)
const sharedKeyCache = new Map<string, string>();

export interface UserPublicKey {
  userId: string;
  publicKey: string;
  deviceId?: string;
  createdAt: string;
  isActive: boolean;
}

/**
 * Initialize E2EE for the current user
 * Generates keys if not present, uploads public key to Supabase
 */
export async function initializeE2EE(userId: string): Promise<KeyPair> {
  // Check if user already has keys
  let keyPair = getStoredKeyPair();
  
  if (!keyPair) {
    // Generate new key pair
    console.log('🔐 Generating new E2EE key pair...');
    keyPair = generateKeyPair();
    storeKeyPair(keyPair);
    console.log('🔐 E2EE key pair generated and stored locally');
  }
  
  // Upload public key to Supabase (if configured)
  if (isSupabaseConfigured()) {
    await uploadPublicKey(userId, keyPair.publicKey);
  }
  
  return keyPair;
}

/**
 * Upload user's public key to Supabase
 * This allows other users to encrypt messages for this user
 */
export async function uploadPublicKey(userId: string, publicKey: string): Promise<boolean> {
  if (!isSupabaseConfigured() || !supabase) {
    console.warn('Supabase not configured, skipping public key upload');
    return false;
  }
  
  try {
    // Upsert to handle both new and existing keys
    const { error } = await supabase
      .from('user_public_keys')
      .upsert({
        user_id: userId,
        public_key: publicKey,
        device_id: getDeviceId(),
        key_type: 'X25519',
        is_active: true,
        updated_at: new Date().toISOString(),
      }, {
        onConflict: 'user_id,device_id',
      });
    
    if (error) {
      // If table doesn't exist, just warn (we'll use localStorage fallback)
      if (error.code === '42P01') {
        console.warn('user_public_keys table not found - using local key exchange');
        return false;
      }
      console.error('Failed to upload public key:', error);
      return false;
    }
    
    console.log('🔐 Public key uploaded to Supabase');
    return true;
  } catch (err) {
    console.error('Error uploading public key:', err);
    return false;
  }
}

/**
 * Fetch a user's public key from Supabase
 */
export async function fetchPublicKey(userId: string): Promise<string | null> {
  // Check cache first
  if (publicKeyCache.has(userId)) {
    return publicKeyCache.get(userId)!;
  }
  
  if (!isSupabaseConfigured() || !supabase) {
    return getPublicKeyFromLocalStorage(userId);
  }
  
  try {
    const { data, error } = await supabase
      .from('user_public_keys')
      .select('public_key')
      .eq('user_id', userId)
      .eq('is_active', true)
      .order('created_at', { ascending: false })
      .limit(1)
      .single();
    
    if (error) {
      // If table doesn't exist, try localStorage
      if (error.code === '42P01' || error.code === 'PGRST116') {
        return getPublicKeyFromLocalStorage(userId);
      }
      console.error('Failed to fetch public key:', error);
      return null;
    }
    
    if (data?.public_key) {
      // Cache the key
      publicKeyCache.set(userId, data.public_key);
      return data.public_key;
    }
    
    return null;
  } catch (err) {
    console.error('Error fetching public key:', err);
    return getPublicKeyFromLocalStorage(userId);
  }
}

/**
 * Fallback: Get public key from localStorage (for local key exchange)
 */
function getPublicKeyFromLocalStorage(userId: string): string | null {
  if (typeof window === 'undefined') return null;
  
  const localKeys = localStorage.getItem('whatsapp_e2ee_public_keys');
  if (!localKeys) return null;
  
  try {
    const keys = JSON.parse(localKeys);
    return keys[userId] || null;
  } catch {
    return null;
  }
}

/**
 * Store a user's public key in localStorage (for offline/fallback)
 */
export function storePublicKeyLocally(userId: string, publicKey: string): void {
  if (typeof window === 'undefined') return;
  
  try {
    const localKeys = localStorage.getItem('whatsapp_e2ee_public_keys');
    const keys = localKeys ? JSON.parse(localKeys) : {};
    keys[userId] = publicKey;
    localStorage.setItem('whatsapp_e2ee_public_keys', JSON.stringify(keys));
  } catch (err) {
    console.error('Error storing public key locally:', err);
  }
}

/**
 * Get or derive a shared key for a conversation
 * Uses caching for performance
 */
export function getSharedKey(theirPublicKey: string, myPrivateKey: string): string {
  const cacheKey = `${theirPublicKey}_${myPrivateKey.slice(0, 10)}`;
  
  if (sharedKeyCache.has(cacheKey)) {
    return sharedKeyCache.get(cacheKey)!;
  }
  
  const sharedKey = deriveSharedKey(theirPublicKey, myPrivateKey);
  sharedKeyCache.set(cacheKey, sharedKey);
  
  return sharedKey;
}

/**
 * Clear cached keys (call on logout)
 */
export function clearKeyCache(): void {
  publicKeyCache.clear();
  sharedKeyCache.clear();
}

/**
 * Get a unique device ID for multi-device support
 */
function getDeviceId(): string {
  if (typeof window === 'undefined') return 'server';
  
  let deviceId = localStorage.getItem('whatsapp_device_id');
  
  if (!deviceId) {
    // Generate a new device ID
    deviceId = `device_${Date.now()}_${Math.random().toString(36).slice(2, 11)}`;
    localStorage.setItem('whatsapp_device_id', deviceId);
  }
  
  return deviceId;
}

/**
 * Check if E2EE is enabled and configured
 */
export function isE2EEEnabled(): boolean {
  return hasEncryptionKeys();
}

/**
 * Get the current user's public key
 */
export function getMyPublicKey(): string | null {
  const keyPair = getStoredKeyPair();
  return keyPair?.publicKey || null;
}

/**
 * Get the current user's private key
 */
export function getMyPrivateKey(): string | null {
  const keyPair = getStoredKeyPair();
  return keyPair?.privateKey || null;
}

// Export types
export type { KeyPair };
