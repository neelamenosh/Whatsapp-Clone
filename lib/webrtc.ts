import {
    sendSignalingMessage,
    subscribeToCallSignaling,
    cleanupSignaling,
    unsubscribeFromChannel,
    type SignalingMessage,
} from './supabase/call-signaling';
import type { RealtimeChannel } from '@supabase/supabase-js';

// ICE Server configuration - using free public STUN servers
const ICE_SERVERS: RTCIceServer[] = [
    { urls: 'stun:stun.l.google.com:19302' },
    { urls: 'stun:stun1.l.google.com:19302' },
    { urls: 'stun:stun2.l.google.com:19302' },
];

export type CallState = 'idle' | 'calling' | 'ringing' | 'connecting' | 'connected' | 'ended' | 'failed';

export interface CallInfo {
    callId: string;
    callerId: string;
    calleeId: string;
    callType: 'video' | 'audio';
    callerName?: string;
    callerAvatar?: string;
}

export type CallEventCallback = (state: CallState, info?: CallInfo) => void;
export type RemoteStreamCallback = (stream: MediaStream | null) => void;

export class WebRTCService {
    private peerConnection: RTCPeerConnection | null = null;
    private localStream: MediaStream | null = null;
    private remoteStream: MediaStream | null = null;
    private signalingChannel: RealtimeChannel | null = null;
    private pendingIceCandidates: RTCIceCandidateInit[] = [];

    private currentCallId: string | null = null;
    private currentUserId: string = '';
    private isInitiator: boolean = false;

    private onCallStateChange: CallEventCallback | null = null;
    private onRemoteStream: RemoteStreamCallback | null = null;

    // Generate unique call ID
    private generateCallId(): string {
        return `call-${Date.now()}-${Math.random().toString(36).substring(2, 9)}`;
    }

    // Set callbacks
    setCallbacks(onStateChange: CallEventCallback, onRemote: RemoteStreamCallback) {
        this.onCallStateChange = onStateChange;
        this.onRemoteStream = onRemote;
    }

    // Get local media stream
    async getLocalStream(video: boolean = true, audio: boolean = true): Promise<MediaStream> {
        if (this.localStream) {
            return this.localStream;
        }

        try {
            this.localStream = await navigator.mediaDevices.getUserMedia({
                video: video ? { facingMode: 'user', width: { ideal: 1280 }, height: { ideal: 720 } } : false,
                audio: audio ? { echoCancellation: true, noiseSuppression: true } : false,
            });
            return this.localStream;
        } catch (error) {
            console.error('Error getting local stream:', error);
            throw error;
        }
    }

    // Create peer connection
    private createPeerConnection(): RTCPeerConnection {
        const pc = new RTCPeerConnection({ iceServers: ICE_SERVERS });

        pc.onicecandidate = (event) => {
            if (event.candidate && this.currentCallId) {
                const targetId = this.isInitiator ? this.getCalleeId() : this.getCallerId();
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

        pc.ontrack = (event) => {
            if (event.streams[0]) {
                this.remoteStream = event.streams[0];
                this.onRemoteStream?.(this.remoteStream);
            }
        };

        pc.onconnectionstatechange = () => {
            console.log('Connection state:', pc.connectionState);
            switch (pc.connectionState) {
                case 'connected':
                    this.onCallStateChange?.('connected');
                    break;
                case 'disconnected':
                case 'failed':
                    this.onCallStateChange?.('failed');
                    this.cleanup();
                    break;
                case 'closed':
                    this.onCallStateChange?.('ended');
                    break;
            }
        };

        pc.oniceconnectionstatechange = () => {
            console.log('ICE connection state:', pc.iceConnectionState);
        };

        return pc;
    }

    private callerId: string = '';
    private calleeId: string = '';

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
        this.currentUserId = userId;
        this.isInitiator = true;
        this.callerId = userId;
        this.calleeId = targetUserId;
        this.currentCallId = this.generateCallId();

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
            const offer = await this.peerConnection.createOffer();
            await this.peerConnection.setLocalDescription(offer);

            await sendSignalingMessage(
                userId,
                targetUserId,
                'offer',
                this.currentCallId,
                offer,
                callType
            );

            return this.currentCallId;
        } catch (error) {
            console.error('Error initiating call:', error);
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
        this.currentUserId = userId;
        this.isInitiator = false;
        this.callerId = callInfo.callerId;
        this.calleeId = userId;
        this.currentCallId = callInfo.callId;

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
                await this.peerConnection.addIceCandidate(new RTCIceCandidate(candidate));
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
        } catch (error) {
            console.error('Error answering call:', error);
            this.onCallStateChange?.('failed');
            this.cleanup();
            throw error;
        }
    }

    // Handle incoming signaling messages
    private async handleSignalingMessage(message: SignalingMessage): Promise<void> {
        console.log('Received signaling message:', message.type);

        switch (message.type) {
            case 'answer':
                if (this.peerConnection && message.payload) {
                    await this.peerConnection.setRemoteDescription(
                        new RTCSessionDescription(message.payload as RTCSessionDescriptionInit)
                    );
                    // Add any pending ICE candidates
                    for (const candidate of this.pendingIceCandidates) {
                        await this.peerConnection.addIceCandidate(new RTCIceCandidate(candidate));
                    }
                    this.pendingIceCandidates = [];
                    this.onCallStateChange?.('connecting');
                }
                break;

            case 'ice-candidate':
                if (message.payload) {
                    const candidate = message.payload as RTCIceCandidateInit;
                    if (this.peerConnection?.remoteDescription) {
                        await this.peerConnection.addIceCandidate(new RTCIceCandidate(candidate));
                    } else {
                        this.pendingIceCandidates.push(candidate);
                    }
                }
                break;

            case 'end':
            case 'reject':
                this.onCallStateChange?.('ended');
                this.cleanup();
                break;

            case 'busy':
                this.onCallStateChange?.('failed');
                this.cleanup();
                break;
        }
    }

    // Reject an incoming call
    async rejectCall(userId: string, callInfo: CallInfo): Promise<void> {
        await sendSignalingMessage(
            userId,
            callInfo.callerId,
            'reject',
            callInfo.callId
        );
    }

    // End current call
    async endCall(): Promise<void> {
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

    // Cleanup resources
    cleanup(): void {
        // Stop local tracks
        this.localStream?.getTracks().forEach((track) => track.stop());
        this.localStream = null;

        // Close peer connection
        this.peerConnection?.close();
        this.peerConnection = null;

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
        this.pendingIceCandidates = [];
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
