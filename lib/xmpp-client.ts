/**
 * XMPP WebSocket Client
 * 
 * Enterprise-grade XMPP signaling client for video calls
 * Connects to Erlang backend via WebSocket
 */

export type XMPPMessageType =
    | 'auth'
    | 'auth_success'
    | 'auth_error'
    | 'call_initiate'
    | 'call_accept'
    | 'call_reject'
    | 'call_end'
    | 'call_accepted'
    | 'call_rejected'
    | 'call_ended'
    | 'call_failed'
    | 'ice_candidate'
    | 'sdp_offer'
    | 'sdp_answer'
    | 'presence'
    | 'ping'
    | 'pong'
    | 'error';

export interface XMPPMessage {
    type: XMPPMessageType;
    from?: string;
    to?: string;
    call_id?: string;
    call_type?: 'video' | 'audio';
    sdp?: RTCSessionDescriptionInit;
    candidate?: RTCIceCandidateInit;
    reason?: string;
    message?: string;
    user_id?: string;
    [key: string]: any;
}

export type ConnectionState = 'disconnected' | 'connecting' | 'connected' | 'authenticated' | 'error';

type MessageHandler = (message: XMPPMessage) => void;

export class XMPPClient {
    private ws: WebSocket | null = null;
    private serverUrl: string;
    private token: string | null = null;
    private userId: string | null = null;
    private connectionState: ConnectionState = 'disconnected';
    private messageHandlers: Map<XMPPMessageType, Set<MessageHandler>> = new Map();
    private reconnectAttempts = 0;
    private maxReconnectAttempts = 5;
    private reconnectDelay = 1000;
    private heartbeatInterval: NodeJS.Timeout | null = null;
    private pendingMessages: XMPPMessage[] = [];

    constructor(serverUrl: string = 'ws://localhost:8080/ws') {
        this.serverUrl = serverUrl;
    }

    /**
     * Connect to the XMPP server
     */
    async connect(token: string): Promise<void> {
        if (this.connectionState !== 'disconnected') {
            return;
        }

        this.token = token;
        this.connectionState = 'connecting';

        return new Promise((resolve, reject) => {
            try {
                this.ws = new WebSocket(this.serverUrl);

                this.ws.onopen = () => {
                    console.log('[XMPP] WebSocket connected');
                    this.authenticate();
                };

                this.ws.onmessage = (event) => {
                    this.handleMessage(event.data);
                };

                this.ws.onclose = (event) => {
                    console.log('[XMPP] WebSocket closed:', event.code, event.reason);
                    this.handleDisconnect();
                };

                this.ws.onerror = (error) => {
                    console.error('[XMPP] WebSocket error:', error);
                    this.connectionState = 'error';
                    reject(error);
                };

                // Listen for auth success
                const authHandler = (msg: XMPPMessage) => {
                    if (msg.type === 'auth_success') {
                        this.userId = msg.user_id || null;
                        this.connectionState = 'authenticated';
                        this.reconnectAttempts = 0;
                        this.startHeartbeat();
                        this.flushPendingMessages();
                        this.off('auth_success', authHandler);
                        resolve();
                    } else if (msg.type === 'auth_error') {
                        this.off('auth_error', authHandler);
                        reject(new Error(msg.message || 'Authentication failed'));
                    }
                };

                this.on('auth_success', authHandler);
                this.on('auth_error', authHandler);

            } catch (error) {
                this.connectionState = 'error';
                reject(error);
            }
        });
    }

    /**
     * Disconnect from the server
     */
    disconnect(): void {
        this.stopHeartbeat();

        if (this.ws) {
            this.ws.close(1000, 'Client disconnect');
            this.ws = null;
        }

        this.connectionState = 'disconnected';
        this.token = null;
        this.userId = null;
    }

    /**
     * Send authentication message
     */
    private authenticate(): void {
        if (!this.token) {
            console.error('[XMPP] No token available for authentication');
            return;
        }

        this.send({
            type: 'auth',
            token: this.token
        });
    }

    /**
     * Initiate a call
     */
    initiateCall(calleeId: string, callType: 'video' | 'audio' = 'video'): string {
        const callId = this.generateCallId();

        this.send({
            type: 'call_initiate',
            to: calleeId,
            call_id: callId,
            call_type: callType
        });

        return callId;
    }

    /**
     * Accept an incoming call
     */
    acceptCall(callId: string): void {
        this.send({
            type: 'call_accept',
            call_id: callId
        });
    }

    /**
     * Reject an incoming call
     */
    rejectCall(callId: string): void {
        this.send({
            type: 'call_reject',
            call_id: callId
        });
    }

    /**
     * End an active call
     */
    endCall(callId: string): void {
        this.send({
            type: 'call_end',
            call_id: callId
        });
    }

    /**
     * Send SDP offer
     */
    sendOffer(callId: string, sdp: RTCSessionDescriptionInit): void {
        this.send({
            type: 'sdp_offer',
            call_id: callId,
            sdp
        });
    }

    /**
     * Send SDP answer
     */
    sendAnswer(callId: string, sdp: RTCSessionDescriptionInit): void {
        this.send({
            type: 'sdp_answer',
            call_id: callId,
            sdp
        });
    }

    /**
     * Send ICE candidate
     */
    sendIceCandidate(callId: string, candidate: RTCIceCandidateInit): void {
        this.send({
            type: 'ice_candidate',
            call_id: callId,
            candidate
        });
    }

    /**
     * Update presence status
     */
    updatePresence(status: 'online' | 'away' | 'offline'): void {
        this.send({
            type: 'presence',
            status
        });
    }

    /**
     * Register event handler
     */
    on(type: XMPPMessageType, handler: MessageHandler): void {
        if (!this.messageHandlers.has(type)) {
            this.messageHandlers.set(type, new Set());
        }
        this.messageHandlers.get(type)!.add(handler);
    }

    /**
     * Remove event handler
     */
    off(type: XMPPMessageType, handler: MessageHandler): void {
        const handlers = this.messageHandlers.get(type);
        if (handlers) {
            handlers.delete(handler);
        }
    }

    /**
     * Send a message to the server
     */
    private send(message: XMPPMessage | Record<string, any>): void {
        if (this.connectionState !== 'authenticated' && message.type !== 'auth') {
            // Queue message for later
            this.pendingMessages.push(message as XMPPMessage);
            return;
        }

        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(JSON.stringify(message));
        } else {
            this.pendingMessages.push(message as XMPPMessage);
        }
    }

    /**
     * Handle incoming message
     */
    private handleMessage(data: string): void {
        try {
            const message: XMPPMessage = JSON.parse(data);

            // Handle ping/pong
            if (message.type === 'ping') {
                this.send({ type: 'pong' });
                return;
            }

            // Dispatch to handlers
            const handlers = this.messageHandlers.get(message.type);
            if (handlers) {
                handlers.forEach(handler => handler(message));
            }

            // Also dispatch to wildcard handlers
            const allHandlers = this.messageHandlers.get('*' as XMPPMessageType);
            if (allHandlers) {
                allHandlers.forEach(handler => handler(message));
            }

        } catch (error) {
            console.error('[XMPP] Failed to parse message:', error);
        }
    }

    /**
     * Handle disconnect
     */
    private handleDisconnect(): void {
        this.stopHeartbeat();
        this.connectionState = 'disconnected';

        // Attempt reconnection
        if (this.reconnectAttempts < this.maxReconnectAttempts && this.token) {
            this.reconnectAttempts++;
            const delay = this.reconnectDelay * Math.pow(2, this.reconnectAttempts - 1);

            console.log(`[XMPP] Reconnecting in ${delay}ms (attempt ${this.reconnectAttempts})`);

            setTimeout(() => {
                if (this.token) {
                    this.connect(this.token).catch(console.error);
                }
            }, delay);
        }
    }

    /**
     * Start heartbeat
     */
    private startHeartbeat(): void {
        this.heartbeatInterval = setInterval(() => {
            this.send({ type: 'ping' });
        }, 25000);
    }

    /**
     * Stop heartbeat
     */
    private stopHeartbeat(): void {
        if (this.heartbeatInterval) {
            clearInterval(this.heartbeatInterval);
            this.heartbeatInterval = null;
        }
    }

    /**
     * Flush pending messages
     */
    private flushPendingMessages(): void {
        while (this.pendingMessages.length > 0) {
            const msg = this.pendingMessages.shift()!;
            this.send(msg);
        }
    }

    /**
     * Generate unique call ID
     */
    private generateCallId(): string {
        return `call_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
    }

    /**
     * Get current connection state
     */
    getConnectionState(): ConnectionState {
        return this.connectionState;
    }

    /**
     * Get current user ID
     */
    getUserId(): string | null {
        return this.userId;
    }
}

// Singleton instance
let xmppClient: XMPPClient | null = null;

export function getXMPPClient(serverUrl?: string): XMPPClient {
    if (!xmppClient) {
        xmppClient = new XMPPClient(serverUrl);
    }
    return xmppClient;
}

export default XMPPClient;
