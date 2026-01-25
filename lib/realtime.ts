type WsClientMessage =
  | { type: 'subscribe'; chatId: string; cursor?: string }
  | { type: 'unsubscribe'; chatId: string }
  | { type: 'message'; chatId: string; payload: unknown };

type WsServerMessage =
  | { type: 'subscribed'; chatId: string; cursor?: string }
  | { type: 'unsubscribed'; chatId: string }
  | { type: 'message'; chatId: string; payload: unknown; _catchup?: boolean; _cursor?: string };

type Listener = (msg: WsServerMessage) => void;

export class RealtimeClient {
  private ws: WebSocket | null = null;
  private listeners = new Set<Listener>();
  private reconnectTimer: number | null = null;
  private readonly url: string;
  private readonly subscriptions = new Map<string, string | null>(); // chatId -> cursor
  private connectionAttempts = 0;
  private maxReconnectDelay = 30000; // Max 30s between reconnects

  constructor(url: string) {
    this.url = url;
  }

  connect() {
    if (typeof window === 'undefined') return;
    if (this.ws && (this.ws.readyState === WebSocket.OPEN || this.ws.readyState === WebSocket.CONNECTING)) {
      return;
    }

    const ws = new WebSocket(this.url);
    this.ws = ws;

    ws.onopen = () => {
      this.connectionAttempts = 0; // Reset on successful connection
      // Resubscribe with cursors for catch-up
      for (const [chatId, cursor] of this.subscriptions) {
        this.send({ type: 'subscribe', chatId, cursor: cursor ?? undefined });
      }
    };

    ws.onmessage = (evt) => {
      try {
        const parsed = JSON.parse(String(evt.data)) as WsServerMessage;
        
        // Update cursor on subscribed and message events
        if (parsed.type === 'subscribed' && parsed.cursor) {
          this.subscriptions.set(parsed.chatId, parsed.cursor);
        }
        if (parsed.type === 'message' && (parsed as any)._cursor) {
          this.subscriptions.set(parsed.chatId, (parsed as any)._cursor);
        }
        
        for (const l of this.listeners) l(parsed);
      } catch {
        // ignore
      }
    };

    ws.onclose = () => {
      this.scheduleReconnect();
    };

    ws.onerror = () => {
      // Let onclose handle reconnect.
    };
  }

  disconnect() {
    if (this.reconnectTimer) window.clearTimeout(this.reconnectTimer);
    this.reconnectTimer = null;
    this.ws?.close();
    this.ws = null;
  }

  private scheduleReconnect() {
    if (typeof window === 'undefined') return;
    if (this.reconnectTimer) return;

    // Exponential backoff with jitter
    this.connectionAttempts++;
    const baseDelay = Math.min(750 * Math.pow(2, this.connectionAttempts - 1), this.maxReconnectDelay);
    const jitter = Math.random() * 0.3 * baseDelay;
    const delay = baseDelay + jitter;

    this.reconnectTimer = window.setTimeout(() => {
      this.reconnectTimer = null;
      this.connect();
    }, delay);
  }

  on(listener: Listener) {
    this.listeners.add(listener);
    return () => this.listeners.delete(listener);
  }

  subscribe(chatId: string, cursor?: string) {
    // Store cursor for reconnection
    if (!this.subscriptions.has(chatId)) {
      this.subscriptions.set(chatId, cursor ?? null);
    }
    this.connect();
    this.send({ type: 'subscribe', chatId, cursor: this.subscriptions.get(chatId) ?? undefined });
  }

  unsubscribe(chatId: string) {
    this.subscriptions.delete(chatId);
    this.send({ type: 'unsubscribe', chatId });
  }

  // Get current cursor for a subscription (for persistence)
  getCursor(chatId: string): string | null {
    return this.subscriptions.get(chatId) ?? null;
  }

  publish(chatId: string, payload: unknown) {
    this.send({ type: 'message', chatId, payload });
  }

  private send(msg: WsClientMessage) {
    if (!this.ws || this.ws.readyState !== WebSocket.OPEN) return;
    this.ws.send(JSON.stringify(msg));
  }
}

let singleton: RealtimeClient | null = null;

export function getRealtimeClient(): RealtimeClient {
  if (typeof window === 'undefined') {
    throw new Error('getRealtimeClient must be called in the browser');
  }
  if (singleton) return singleton;

  const envUrl = process.env.NEXT_PUBLIC_WS_URL;
  const url =
    envUrl ??
    `${window.location.protocol === 'https:' ? 'wss' : 'ws'}://${window.location.hostname}:3001`;

  singleton = new RealtimeClient(url);
  return singleton;
}
