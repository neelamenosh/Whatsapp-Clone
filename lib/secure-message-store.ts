import type { Message } from '@/lib/types';

const KEY_KEY = 'whatsapp_clone:aes_key_v1';
const CHAT_PREFIX = 'whatsapp_clone:chat:';

function bytesToBase64(bytes: Uint8Array): string {
  let binary = '';
  for (const b of bytes) binary += String.fromCharCode(b);
  return btoa(binary);
}

function base64ToBytes(b64: string): Uint8Array {
  const binary = atob(b64);
  const out = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) out[i] = binary.charCodeAt(i);
  return out;
}

async function getOrCreateKey(): Promise<CryptoKey> {
  const existing = typeof window !== 'undefined' ? window.localStorage.getItem(KEY_KEY) : null;
  if (existing) {
    const raw = base64ToBytes(existing);
    return crypto.subtle.importKey('raw', raw, 'AES-GCM', true, ['encrypt', 'decrypt']);
  }

  const key = await crypto.subtle.generateKey({ name: 'AES-GCM', length: 256 }, true, [
    'encrypt',
    'decrypt',
  ]);
  const raw = new Uint8Array(await crypto.subtle.exportKey('raw', key));
  window.localStorage.setItem(KEY_KEY, bytesToBase64(raw));
  return key;
}

async function encryptJson<T>(value: T): Promise<string> {
  const key = await getOrCreateKey();
  const iv = crypto.getRandomValues(new Uint8Array(12));
  const encoded = new TextEncoder().encode(JSON.stringify(value));
  const ciphertext = new Uint8Array(
    await crypto.subtle.encrypt({ name: 'AES-GCM', iv }, key, encoded)
  );

  // Store as iv:ciphertext (base64)
  return `${bytesToBase64(iv)}:${bytesToBase64(ciphertext)}`;
}

async function decryptJson<T>(payload: string): Promise<T | null> {
  try {
    const [ivB64, ctB64] = payload.split(':');
    if (!ivB64 || !ctB64) return null;

    const key = await getOrCreateKey();
    const iv = base64ToBytes(ivB64);
    const ciphertext = base64ToBytes(ctB64);
    const plaintext = new Uint8Array(
      await crypto.subtle.decrypt({ name: 'AES-GCM', iv }, key, ciphertext)
    );
    const json = new TextDecoder().decode(plaintext);
    return JSON.parse(json) as T;
  } catch {
    return null;
  }
}

type StoredMessage = Omit<Message, 'timestamp' | 'expiresAt'> & {
  timestamp: string;
  expiresAt?: string;
};

function toStored(m: Message): StoredMessage {
  return {
    ...m,
    timestamp: m.timestamp.toISOString(),
    expiresAt: m.expiresAt ? m.expiresAt.toISOString() : undefined,
  };
}

function fromStored(m: StoredMessage): Message {
  return {
    ...m,
    timestamp: new Date(m.timestamp),
    expiresAt: m.expiresAt ? new Date(m.expiresAt) : undefined,
  };
}

export async function loadChatMessages(chatId: string): Promise<Message[] | null> {
  if (typeof window === 'undefined') return null;
  const raw = window.localStorage.getItem(CHAT_PREFIX + chatId);
  if (!raw) return null;

  const decoded = await decryptJson<{ messages: StoredMessage[] }>(raw);
  if (!decoded) return null;
  return decoded.messages.map(fromStored);
}

export async function saveChatMessages(chatId: string, messages: Message[]): Promise<void> {
  if (typeof window === 'undefined') return;
  const payload = await encryptJson({ messages: messages.map(toStored) });
  window.localStorage.setItem(CHAT_PREFIX + chatId, payload);
}
