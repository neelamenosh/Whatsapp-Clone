/**
 * Stable WebRTC Service
 * 
 * Enhanced for WhatsApp-like stability with:
 * - ICE restart for connection recovery
 * - Multiple STUN/TURN servers
 * - Automatic reconnection
 * - Connection quality monitoring
 * - Proper error handling
 */

import {
    sendSignalingMessage,
    subscribeToCallSignaling,
    cleanupSignaling,
    unsubscribeFromChannel,
    type SignalingMessage,
} from './supabase/call-signaling';
import {
    createCallLog,
    updateCallLogByCallId,
} from './supabase/call-logs';
import type { RealtimeChannel } from '@supabase/supabase-js';

// Multiple ICE servers for better connectivity
const ICE_SERVERS: RTCIceServer[] = [
    // Google STUN servers (free, reliable)
    { urls: 'stun:stun.l.google.com:19302' },
    { urls: 'stun:stun1.l.google.com:19302' },
    { urls: 'stun:stun2.l.google.com:19302' },
    { urls: 'stun:stun3.l.google.com:19302' },
    { urls: 'stun:stun4.l.google.com:19302' },
    // Twilio STUN (backup)
    { urls: 'stun:global.stun.twilio.com:3478' },
    // Open Relay TURN servers (free, for NAT traversal)
    {
        urls: 'turn:openrelay.metered.ca:80',
        username: 'openrelayproject',
        credential: 'openrelayproject',
    },
    {
        urls: 'turn:openrelay.metered.ca:443',
        username: 'openrelayproject',
        credential: 'openrelayproject',
    },
    {
        urls: 'turn:openrelay.metered.ca:443?transport=tcp',
        username: 'openrelayproject',
        credential: 'openrelayproject',
    },
];

export type CallState = 'idle' | 'calling' | 'ringing' | 'connecting' | 'connected' | 'reconnecting' | 'ended' | 'failed';

export interface CallInfo {
    callId: string;
    callerId: string;
    calleeId: string;
    callType: 'video' | 'audio';
    callerName?: string;
    callerAvatar?: string;
}

export interface ConnectionQuality {
    state: 'excellent' | 'good' | 'poor' | 'disconnected';
    rtt?: number;
    packetLoss?: number;
}

export type CallEventCallback = (state: CallState, info?: CallInfo) => void;
export type RemoteStreamCallback = (stream: MediaStream | null) => void;
export type QualityCallback = (quality: ConnectionQuality) => void;

export class WebRTCService {
    private peerConnection: RTCPeerConnection | null = null;
    private localStream: MediaStream | null = null;
    private remoteStream: MediaStream | null = null;
    private signalingChannel: RealtimeChannel | null = null;
    private pendingIceCandidates: RTCIceCandidateInit[] = [];

    private currentCallId: string | null = null;
    private currentUserId: string = '';
    private isInitiator: boolean = false;
    private callerId: string = '';
    private calleeId: string = '';
    private currentCallType: 'video' | 'audio' = 'video';

    // Call logging
    private callLogId: string | null = null;
    private callStartTime: Date | null = null;

    // Stability features
    private reconnectAttempts: number = 0;
    private maxReconnectAttempts: number = 3;
    private iceRestartAttempts: number = 0;
    private maxIceRestartAttempts: number = 3;
    private qualityCheckInterval: NodeJS.Timeout | null = null;
    private connectionTimeout: NodeJS.Timeout | null = null;
    private lastOffer: RTCSessionDescriptionInit | null = null;

    // Callbacks
    private onCallStateChange: CallEventCallback | null = null;
    private onRemoteStream: RemoteStreamCallback | null = null;
    private onQualityChange: QualityCallback | null = null;

    // Generate unique call ID
    private generateCallId(): string {
        return `call-${Date.now()}-${Math.random().toString(36).substring(2, 9)}`;
    }

    // Set callbacks
    setCallbacks(
        onStateChange: CallEventCallback,
        onRemote: RemoteStreamCallback,
        onQuality?: QualityCallback
    ) {
        this.onCallStateChange = onStateChange;
        this.onRemoteStream = onRemote;
        this.onQualityChange = onQuality || null;
    }

    // Get local media stream with retry
    async getLocalStream(video: boolean = true, audio: boolean = true): Promise<MediaStream> {
        if (this.localStream) {
            return this.localStream;
        }

        const attempts = [
            // Try with ideal constraints first
            { video: video ? { facingMode: 'user', width: { ideal: 1280 }, height: { ideal: 720 } } : false, audio: audio ? { echoCancellation: true, noiseSuppression: true, autoGainControl: true } : false },
            // Fallback to simpler constraints
            { video: video ? { facingMode: 'user' } : false, audio: audio },
            // Last resort - basic constraints
            { video: video, audio: audio }
        ];

        for (const constraints of attempts) {
            try {
                this.localStream = await navigator.mediaDevices.getUserMedia(constraints);
                console.log('[WebRTC] Got local stream with constraints:', constraints);
                return this.localStream;
            } catch (error) {
                console.warn('[WebRTC] Failed with constraints, trying fallback:', error);
            }
        }

        throw new Error('Failed to get local media stream after all attempts');
    }

    // Create peer connection with enhanced configuration
    private createPeerConnection(): RTCPeerConnection {
        const config: RTCConfiguration = {
            iceServers: ICE_SERVERS,
            iceTransportPolicy: 'all',
            bundlePolicy: 'max-bundle',
            rtcpMuxPolicy: 'require',
            iceCandidatePoolSize: 10,
        };

        const pc = new RTCPeerConnection(config);

        // ICE candidate handler
        pc.onicecandidate = (event) => {
            if (event.candidate && this.currentCallId) {
                const targetId = this.isInitiator ? this.calleeId : this.callerId;
                if (targetId) {
                    sendSignalingMessage(
                        this.currentUserId,
                        targetId,
                        'ice-candidate',
                        this.currentCallId,
                        event.candidate.toJSON()
                    );
                }
            }
        };

        // ICE gathering state
        pc.onicegatheringstatechange = () => {
            console.log('[WebRTC] ICE gathering state:', pc.iceGatheringState);
        };

        // Remote track handler
        pc.ontrack = (event) => {
            console.log('[WebRTC] Remote track received:', event.track.kind);
            if (event.streams[0]) {
                this.remoteStream = event.streams[0];
                this.onRemoteStream?.(this.remoteStream);
            }
        };

        // Connection state handler with recovery logic
        pc.onconnectionstatechange = () => {
            console.log('[WebRTC] Connection state:', pc.connectionState);
            this.handleConnectionStateChange(pc.connectionState);
        };

        // ICE connection state handler with recovery logic
        pc.oniceconnectionstatechange = () => {
            console.log('[WebRTC] ICE connection state:', pc.iceConnectionState);
            this.handleIceConnectionStateChange(pc.iceConnectionState);
        };

        // Start connection timeout
        this.startConnectionTimeout();

        return pc;
    }

    // Handle connection state changes
    private handleConnectionStateChange(state: RTCPeerConnectionState): void {
        switch (state) {
            case 'connected':
                this.clearConnectionTimeout();
                this.reconnectAttempts = 0;
                this.iceRestartAttempts = 0;
                this.onCallStateChange?.('connected');
                this.startQualityMonitoring();
                break;

            case 'disconnected':
                this.onCallStateChange?.('reconnecting');
                // Don't cleanup immediately - try to recover
                this.attemptReconnection();
                break;

            case 'failed':
                if (this.iceRestartAttempts < this.maxIceRestartAttempts) {
                    this.attemptIceRestart();
                } else {
                    this.onCallStateChange?.('failed');
                    this.cleanup();
                }
                break;

            case 'closed':
                this.onCallStateChange?.('ended');
                this.cleanup();
                break;
        }
    }

    // Handle ICE connection state changes
    private handleIceConnectionStateChange(state: RTCIceConnectionState): void {
        switch (state) {
            case 'connected':
            case 'completed':
                this.clearConnectionTimeout();
                this.updateQuality({ state: 'excellent' });
                break;

            case 'checking':
                this.updateQuality({ state: 'good' });
                break;

            case 'disconnected':
                this.updateQuality({ state: 'poor' });
                this.attemptReconnection();
                break;

            case 'failed':
                this.updateQuality({ state: 'disconnected' });
                if (this.iceRestartAttempts < this.maxIceRestartAttempts) {
                    this.attemptIceRestart();
                }
                break;
        }
    }

    // Attempt ICE restart
    private async attemptIceRestart(): Promise<void> {
        if (!this.peerConnection || !this.currentCallId || !this.isInitiator) {
            return;
        }

        this.iceRestartAttempts++;
        console.log(`[WebRTC] Attempting ICE restart (${this.iceRestartAttempts}/${this.maxIceRestartAttempts})`);

        try {
            this.onCallStateChange?.('reconnecting');

            const offer = await this.peerConnection.createOffer({ iceRestart: true });
            await this.peerConnection.setLocalDescription(offer);

            await sendSignalingMessage(
                this.currentUserId,
                this.calleeId,
                'offer',
                this.currentCallId,
                offer,
                this.currentCallType
            );

            this.lastOffer = offer;
        } catch (error) {
            console.error('[WebRTC] ICE restart failed:', error);
            if (this.iceRestartAttempts >= this.maxIceRestartAttempts) {
                this.onCallStateChange?.('failed');
                this.cleanup();
            }
        }
    }

    // Attempt reconnection
    private async attemptReconnection(): Promise<void> {
        if (this.reconnectAttempts >= this.maxReconnectAttempts) {
            console.log('[WebRTC] Max reconnection attempts reached');
            this.onCallStateChange?.('failed');
            this.cleanup();
            return;
        }

        this.reconnectAttempts++;
        console.log(`[WebRTC] Attempting reconnection (${this.reconnectAttempts}/${this.maxReconnectAttempts})`);

        // Wait a bit before trying
        await new Promise(resolve => setTimeout(resolve, 2000));

        // Check if connection recovered
        if (this.peerConnection?.connectionState === 'connected') {
            console.log('[WebRTC] Connection recovered');
            this.reconnectAttempts = 0;
            return;
        }

        // Try ICE restart
        this.attemptIceRestart();
    }

    // Start connection timeout
    private startConnectionTimeout(): void {
        this.clearConnectionTimeout();
        this.connectionTimeout = setTimeout(() => {
            if (this.peerConnection?.connectionState !== 'connected') {
                console.log('[WebRTC] Connection timeout');
                this.attemptIceRestart();
            }
        }, 30000); // 30 second timeout
    }

    // Clear connection timeout
    private clearConnectionTimeout(): void {
        if (this.connectionTimeout) {
            clearTimeout(this.connectionTimeout);
            this.connectionTimeout = null;
        }
    }

    // Start quality monitoring
    private startQualityMonitoring(): void {
        this.stopQualityMonitoring();

        this.qualityCheckInterval = setInterval(async () => {
            if (!this.peerConnection) return;

            try {
                const stats = await this.peerConnection.getStats();
                let rtt: number | undefined;
                let packetsLost = 0;
                let packetsReceived = 0;

                stats.forEach((report) => {
                    if (report.type === 'candidate-pair' && report.state === 'succeeded') {
                        rtt = report.currentRoundTripTime * 1000; // Convert to ms
                    }
                    if (report.type === 'inbound-rtp') {
                        packetsLost += report.packetsLost || 0;
                        packetsReceived += report.packetsReceived || 0;
                    }
                });

                const packetLoss = packetsReceived > 0
                    ? (packetsLost / (packetsLost + packetsReceived)) * 100
                    : 0;

                let qualityState: ConnectionQuality['state'] = 'excellent';
                if (packetLoss > 5 || (rtt && rtt > 300)) {
                    qualityState = 'poor';
                } else if (packetLoss > 1 || (rtt && rtt > 150)) {
                    qualityState = 'good';
                }

                this.updateQuality({ state: qualityState, rtt, packetLoss });
            } catch (error) {
                console.warn('[WebRTC] Quality check failed:', error);
            }
        }, 5000);
    }

    // Stop quality monitoring
    private stopQualityMonitoring(): void {
        if (this.qualityCheckInterval) {
            clearInterval(this.qualityCheckInterval);
            this.qualityCheckInterval = null;
        }
    }

    // Update quality callback
    private updateQuality(quality: ConnectionQuality): void {
        this.onQualityChange?.(quality);
    }

    private getCallerId(): string {
        return this.callerId;
    }

    private getCalleeId(): string {
        return this.calleeId;
    }

    // Initiate a call
    async initiateCall(
        userId: string,
        targetUserId: string,
        callType: 'video' | 'audio' = 'video'
    ): Promise<string> {
        // Reset state
        this.resetState();

        this.currentUserId = userId;
        this.isInitiator = true;
        this.callerId = userId;
        this.calleeId = targetUserId;
        this.currentCallId = this.generateCallId();
        this.currentCallType = callType;

        this.onCallStateChange?.('calling');

        try {
            // Get local stream
            await this.getLocalStream(callType === 'video', true);

            // Create peer connection
            this.peerConnection = this.createPeerConnection();

            // Add local tracks
            this.localStream?.getTracks().forEach((track) => {
                this.peerConnection!.addTrack(track, this.localStream!);
            });

            // Subscribe to signaling for this call
            this.signalingChannel = subscribeToCallSignaling(
                this.currentCallId,
                userId,
                this.handleSignalingMessage.bind(this)
            );

            // Create and send offer
            const offer = await this.peerConnection.createOffer({
                offerToReceiveAudio: true,
                offerToReceiveVideo: callType === 'video',
            });
            await this.peerConnection.setLocalDescription(offer);
            this.lastOffer = offer;

            await sendSignalingMessage(
                userId,
                targetUserId,
                'offer',
                this.currentCallId,
                offer,
                callType
            );

            // Create call log for outgoing call
            this.callStartTime = new Date();
            const { data: callLog } = await createCallLog(
                userId,
                targetUserId,
                callType,
                'outgoing',
                this.currentCallId
            );
            if (callLog) {
                this.callLogId = callLog.id;
                console.log('[WebRTC] Call log created for outgoing call:', callLog.id);
            }

            return this.currentCallId;
        } catch (error) {
            console.error('[WebRTC] Error initiating call:', error);
            this.onCallStateChange?.('failed');
            this.cleanup();
            throw error;
        }
    }

    // Answer an incoming call
    async answerCall(
        userId: string,
        callInfo: CallInfo,
        offer: RTCSessionDescriptionInit
    ): Promise<void> {
        // Reset state
        this.resetState();

        this.currentUserId = userId;
        this.isInitiator = false;
        this.callerId = callInfo.callerId;
        this.calleeId = userId;
        this.currentCallId = callInfo.callId;
        this.currentCallType = callInfo.callType;

        this.onCallStateChange?.('connecting');

        try {
            // Get local stream
            await this.getLocalStream(callInfo.callType === 'video', true);

            // Create peer connection
            this.peerConnection = this.createPeerConnection();

            // Add local tracks
            this.localStream?.getTracks().forEach((track) => {
                this.peerConnection!.addTrack(track, this.localStream!);
            });

            // Subscribe to signaling for this call
            this.signalingChannel = subscribeToCallSignaling(
                this.currentCallId,
                userId,
                this.handleSignalingMessage.bind(this)
            );

            // Set remote description
            await this.peerConnection.setRemoteDescription(new RTCSessionDescription(offer));

            // Add any pending ICE candidates
            for (const candidate of this.pendingIceCandidates) {
                try {
                    await this.peerConnection.addIceCandidate(new RTCIceCandidate(candidate));
                } catch (e) {
                    console.warn('[WebRTC] Failed to add pending ICE candidate:', e);
                }
            }
            this.pendingIceCandidates = [];

            // Create and send answer
            const answer = await this.peerConnection.createAnswer();
            await this.peerConnection.setLocalDescription(answer);

            await sendSignalingMessage(
                userId,
                callInfo.callerId,
                'answer',
                this.currentCallId,
                answer
            );

            // Create call log for incoming call
            this.callStartTime = new Date();
            const { data: callLog } = await createCallLog(
                callInfo.callerId,
                userId,
                callInfo.callType,
                'incoming',
                this.currentCallId
            );
            if (callLog) {
                this.callLogId = callLog.id;
                console.log('[WebRTC] Call log created for incoming call:', callLog.id);
            }
        } catch (error) {
            console.error('[WebRTC] Error answering call:', error);
            this.onCallStateChange?.('failed');
            this.cleanup();
            throw error;
        }
    }

    // Handle incoming signaling messages
    private async handleSignalingMessage(message: SignalingMessage): Promise<void> {
        console.log('[WebRTC] handleSignalingMessage received:', message.type, {
            hasPayload: !!message.payload,
            hasPeerConnection: !!this.peerConnection,
            pcState: this.peerConnection?.connectionState,
            isInitiator: this.isInitiator
        });

        try {
            switch (message.type) {
                case 'offer':
                    // Handle ICE restart offer
                    if (this.peerConnection && !this.isInitiator && message.payload) {
                        await this.peerConnection.setRemoteDescription(
                            new RTCSessionDescription(message.payload as RTCSessionDescriptionInit)
                        );
                        const answer = await this.peerConnection.createAnswer();
                        await this.peerConnection.setLocalDescription(answer);
                        await sendSignalingMessage(
                            this.currentUserId,
                            this.callerId,
                            'answer',
                            this.currentCallId!,
                            answer
                        );
                    }
                    break;

                case 'answer':
                    if (this.peerConnection && message.payload) {
                        await this.peerConnection.setRemoteDescription(
                            new RTCSessionDescription(message.payload as RTCSessionDescriptionInit)
                        );
                        // Add any pending ICE candidates
                        for (const candidate of this.pendingIceCandidates) {
                            try {
                                await this.peerConnection.addIceCandidate(new RTCIceCandidate(candidate));
                            } catch (e) {
                                console.warn('[WebRTC] Failed to add pending ICE candidate:', e);
                            }
                        }
                        this.pendingIceCandidates = [];
                        this.onCallStateChange?.('connecting');
                    }
                    break;

                case 'ice-candidate':
                    if (message.payload) {
                        const candidate = message.payload as RTCIceCandidateInit;
                        if (this.peerConnection?.remoteDescription) {
                            try {
                                await this.peerConnection.addIceCandidate(new RTCIceCandidate(candidate));
                            } catch (e) {
                                console.warn('[WebRTC] Failed to add ICE candidate:', e);
                            }
                        } else {
                            this.pendingIceCandidates.push(candidate);
                        }
                    }
                    break;

                case 'end':
                case 'reject':
                    // Update call log when call is ended/rejected by other party
                    if (this.currentCallId && this.callStartTime) {
                        const duration = Math.floor((Date.now() - this.callStartTime.getTime()) / 1000);
                        await updateCallLogByCallId(this.currentCallId, {
                            status: 'completed',
                            endedAt: new Date(),
                            duration
                        });
                    }
                    this.onCallStateChange?.('ended');
                    this.cleanup();
                    break;

                case 'busy':
                    this.onCallStateChange?.('failed');
                    this.cleanup();
                    break;
            }
        } catch (error) {
            console.error('[WebRTC] Error handling signaling message:', error);
        }
    }

    // Reject an incoming call
    async rejectCall(userId: string, callInfo: CallInfo): Promise<void> {
        // Create a call log for the missed/declined call
        await createCallLog(
            callInfo.callerId,
            userId,
            callInfo.callType,
            'declined',
            callInfo.callId
        );

        await sendSignalingMessage(
            userId,
            callInfo.callerId,
            'reject',
            callInfo.callId
        );
    }

    // End current call
    async endCall(): Promise<void> {
        // Update call log with duration and completed status
        if (this.currentCallId && this.callStartTime) {
            const duration = Math.floor((Date.now() - this.callStartTime.getTime()) / 1000);
            await updateCallLogByCallId(this.currentCallId, {
                status: 'completed',
                endedAt: new Date(),
                duration
            });
            console.log('[WebRTC] Call log updated with duration:', duration, 'seconds');
        }

        if (this.currentCallId) {
            const targetId = this.isInitiator ? this.calleeId : this.callerId;
            if (targetId) {
                await sendSignalingMessage(
                    this.currentUserId,
                    targetId,
                    'end',
                    this.currentCallId
                );
            }
        }
        this.onCallStateChange?.('ended');
        this.cleanup();
    }

    // Toggle audio mute
    toggleAudio(muted: boolean): void {
        this.localStream?.getAudioTracks().forEach((track) => {
            track.enabled = !muted;
        });
    }

    // Toggle video
    toggleVideo(disabled: boolean): void {
        this.localStream?.getVideoTracks().forEach((track) => {
            track.enabled = !disabled;
        });
    }

    // Get local stream for display
    getLocalMediaStream(): MediaStream | null {
        return this.localStream;
    }

    // Get remote stream for display
    getRemoteMediaStream(): MediaStream | null {
        return this.remoteStream;
    }

    // Reset state without cleanup
    private resetState(): void {
        this.reconnectAttempts = 0;
        this.iceRestartAttempts = 0;
        this.pendingIceCandidates = [];
        this.lastOffer = null;
    }

    // Cleanup resources
    cleanup(): void {
        console.log('[WebRTC] Cleaning up resources');

        // Stop timers
        this.clearConnectionTimeout();
        this.stopQualityMonitoring();

        // Stop local tracks
        this.localStream?.getTracks().forEach((track) => track.stop());
        this.localStream = null;

        // Close peer connection
        if (this.peerConnection) {
            this.peerConnection.onicecandidate = null;
            this.peerConnection.ontrack = null;
            this.peerConnection.onconnectionstatechange = null;
            this.peerConnection.oniceconnectionstatechange = null;
            this.peerConnection.close();
            this.peerConnection = null;
        }

        // Clean up remote stream
        this.remoteStream = null;
        this.onRemoteStream?.(null);

        // Unsubscribe from signaling
        unsubscribeFromChannel(this.signalingChannel);
        this.signalingChannel = null;

        // Clean up signaling messages
        if (this.currentCallId) {
            cleanupSignaling(this.currentCallId);
        }

        // Reset state
        this.currentCallId = null;
        this.callLogId = null;
        this.callStartTime = null;
        this.resetState();
    }
}

// Singleton instance
let webrtcServiceInstance: WebRTCService | null = null;

export function getWebRTCService(): WebRTCService {
    if (!webrtcServiceInstance) {
        webrtcServiceInstance = new WebRTCService();
    }
    return webrtcServiceInstance;
}
