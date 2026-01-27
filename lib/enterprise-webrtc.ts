/**
 * Enterprise WebRTC Service
 * 
 * WebRTC implementation integrated with XMPP signaling
 * for carrier-grade video calling
 */

import { getXMPPClient, type XMPPMessage } from './xmpp-client';

export type CallState =
    | 'idle'
    | 'initiating'
    | 'ringing'
    | 'connecting'
    | 'connected'
    | 'reconnecting'
    | 'ended'
    | 'failed';

export interface EnterpriseCallInfo {
    callId: string;
    callerId: string;
    calleeId: string;
    callType: 'video' | 'audio';
    state: CallState;
    startTime?: Date;
    endTime?: Date;
}

interface TURNCredentials {
    iceServers: RTCIceServer[];
    ttl: number;
}

type StateChangeHandler = (state: CallState, callInfo: EnterpriseCallInfo | null) => void;
type StreamHandler = (stream: MediaStream) => void;

export class EnterpriseWebRTCService {
    private peerConnection: RTCPeerConnection | null = null;
    private localStream: MediaStream | null = null;
    private remoteStream: MediaStream | null = null;
    private currentCall: EnterpriseCallInfo | null = null;
    private turnCredentials: TURNCredentials | null = null;
    private credentialsExpiry: number = 0;

    private stateChangeHandlers: Set<StateChangeHandler> = new Set();
    private localStreamHandlers: Set<StreamHandler> = new Set();
    private remoteStreamHandlers: Set<StreamHandler> = new Set();

    private apiBaseUrl: string;
    private authToken: string | null = null;

    constructor(apiBaseUrl: string = 'http://localhost:8080') {
        this.apiBaseUrl = apiBaseUrl;
        this.setupXMPPListeners();
    }

    /**
     * Initialize with authentication token
     */
    async initialize(token: string): Promise<void> {
        this.authToken = token;

        // Connect XMPP client
        const xmpp = getXMPPClient(`${this.apiBaseUrl.replace('http', 'ws')}/ws`);
        await xmpp.connect(token);

        // Pre-fetch TURN credentials
        await this.fetchTURNCredentials();
    }

    /**
     * Setup XMPP message listeners
     */
    private setupXMPPListeners(): void {
        const xmpp = getXMPPClient();

        // Incoming call
        xmpp.on('call_initiate', (msg) => {
            this.handleIncomingCall(msg);
        });

        // Call accepted by remote
        xmpp.on('call_accepted', (msg) => {
            this.handleCallAccepted(msg);
        });

        // Call rejected by remote
        xmpp.on('call_rejected', (msg) => {
            this.handleCallRejected(msg);
        });

        // Call ended
        xmpp.on('call_ended', (msg) => {
            this.handleCallEnded(msg);
        });

        // Call failed
        xmpp.on('call_failed', (msg) => {
            this.handleCallFailed(msg);
        });

        // SDP offer received
        xmpp.on('sdp_offer', (msg) => {
            this.handleRemoteOffer(msg);
        });

        // SDP answer received  
        xmpp.on('sdp_answer', (msg) => {
            this.handleRemoteAnswer(msg);
        });

        // ICE candidate received
        xmpp.on('ice_candidate', (msg) => {
            this.handleRemoteIceCandidate(msg);
        });
    }

    /**
     * Fetch TURN credentials from server
     */
    private async fetchTURNCredentials(): Promise<TURNCredentials> {
        const now = Date.now();

        // Return cached credentials if still valid
        if (this.turnCredentials && this.credentialsExpiry > now + 60000) {
            return this.turnCredentials;
        }

        const response = await fetch(`${this.apiBaseUrl}/api/turn/credentials`, {
            headers: {
                'Authorization': `Bearer ${this.authToken}`
            }
        });

        if (!response.ok) {
            throw new Error('Failed to fetch TURN credentials');
        }

        this.turnCredentials = await response.json();
        this.credentialsExpiry = now + (this.turnCredentials!.ttl * 1000);

        return this.turnCredentials!;
    }

    /**
     * Create RTCPeerConnection with TURN credentials
     */
    private async createPeerConnection(): Promise<RTCPeerConnection> {
        const credentials = await this.fetchTURNCredentials();

        const config: RTCConfiguration = {
            iceServers: credentials.iceServers,
            bundlePolicy: 'max-bundle',
            rtcpMuxPolicy: 'require',
            iceTransportPolicy: 'all'
        };

        const pc = new RTCPeerConnection(config);

        // ICE candidate handler
        pc.onicecandidate = (event) => {
            if (event.candidate && this.currentCall) {
                getXMPPClient().sendIceCandidate(
                    this.currentCall.callId,
                    event.candidate.toJSON()
                );
            }
        };

        // ICE connection state
        pc.oniceconnectionstatechange = () => {
            console.log('[WebRTC] ICE state:', pc.iceConnectionState);

            switch (pc.iceConnectionState) {
                case 'connected':
                case 'completed':
                    this.updateState('connected');
                    break;
                case 'disconnected':
                    this.updateState('reconnecting');
                    break;
                case 'failed':
                    this.handleConnectionFailed();
                    break;
            }
        };

        // Remote track handler
        pc.ontrack = (event) => {
            console.log('[WebRTC] Remote track received');

            if (!this.remoteStream) {
                this.remoteStream = new MediaStream();
            }

            this.remoteStream.addTrack(event.track);
            this.notifyRemoteStream(this.remoteStream);
        };

        this.peerConnection = pc;
        return pc;
    }

    /**
     * Get local media stream
     */
    async getLocalStream(video: boolean = true, audio: boolean = true): Promise<MediaStream> {
        if (this.localStream) {
            return this.localStream;
        }

        const constraints: MediaStreamConstraints = {
            video: video ? {
                width: { ideal: 1280 },
                height: { ideal: 720 },
                frameRate: { ideal: 30 }
            } : false,
            audio: audio ? {
                echoCancellation: true,
                noiseSuppression: true,
                autoGainControl: true
            } : false
        };

        this.localStream = await navigator.mediaDevices.getUserMedia(constraints);
        this.notifyLocalStream(this.localStream);

        return this.localStream;
    }

    /**
     * Initiate an outgoing call
     */
    async initiateCall(
        calleeId: string,
        callType: 'video' | 'audio' = 'video'
    ): Promise<string> {
        if (this.currentCall) {
            throw new Error('Already in a call');
        }

        // Get local media first
        await this.getLocalStream(callType === 'video', true);

        // Create peer connection
        const pc = await this.createPeerConnection();

        // Add local tracks
        this.localStream!.getTracks().forEach(track => {
            pc.addTrack(track, this.localStream!);
        });

        // Initiate call via XMPP
        const callId = getXMPPClient().initiateCall(calleeId, callType);

        this.currentCall = {
            callId,
            callerId: getXMPPClient().getUserId()!,
            calleeId,
            callType,
            state: 'initiating'
        };

        this.updateState('initiating');

        // Create and send offer
        const offer = await pc.createOffer({
            offerToReceiveAudio: true,
            offerToReceiveVideo: callType === 'video'
        });

        await pc.setLocalDescription(offer);
        getXMPPClient().sendOffer(callId, offer);

        return callId;
    }

    /**
     * Answer an incoming call
     */
    async answerCall(callId: string, sdpOffer: RTCSessionDescriptionInit): Promise<void> {
        if (!this.currentCall || this.currentCall.callId !== callId) {
            throw new Error('No matching incoming call');
        }

        // Get local media
        await this.getLocalStream(this.currentCall.callType === 'video', true);

        // Create peer connection
        const pc = await this.createPeerConnection();

        // Add local tracks
        this.localStream!.getTracks().forEach(track => {
            pc.addTrack(track, this.localStream!);
        });

        // Set remote description (offer)
        await pc.setRemoteDescription(new RTCSessionDescription(sdpOffer));

        // Create and send answer
        const answer = await pc.createAnswer();
        await pc.setLocalDescription(answer);

        getXMPPClient().acceptCall(callId);
        getXMPPClient().sendAnswer(callId, answer);

        this.updateState('connecting');
    }

    /**
     * Reject an incoming call
     */
    rejectCall(callId: string): void {
        getXMPPClient().rejectCall(callId);
        this.cleanup();
    }

    /**
     * End current call
     */
    endCall(): void {
        if (this.currentCall) {
            getXMPPClient().endCall(this.currentCall.callId);
        }
        this.cleanup();
    }

    /**
     * Toggle audio mute
     */
    toggleAudio(): boolean {
        if (!this.localStream) return false;

        const audioTrack = this.localStream.getAudioTracks()[0];
        if (audioTrack) {
            audioTrack.enabled = !audioTrack.enabled;
            return audioTrack.enabled;
        }
        return false;
    }

    /**
     * Toggle video
     */
    toggleVideo(): boolean {
        if (!this.localStream) return false;

        const videoTrack = this.localStream.getVideoTracks()[0];
        if (videoTrack) {
            videoTrack.enabled = !videoTrack.enabled;
            return videoTrack.enabled;
        }
        return false;
    }

    /**
     * Handle incoming call
     */
    private handleIncomingCall(msg: XMPPMessage): void {
        this.currentCall = {
            callId: msg.call_id!,
            callerId: msg.from!,
            calleeId: getXMPPClient().getUserId()!,
            callType: msg.call_type || 'video',
            state: 'ringing'
        };

        this.updateState('ringing');
    }

    /**
     * Handle call accepted
     */
    private handleCallAccepted(_msg: XMPPMessage): void {
        this.updateState('connecting');
    }

    /**
     * Handle call rejected
     */
    private handleCallRejected(_msg: XMPPMessage): void {
        this.updateState('ended');
        this.cleanup();
    }

    /**
     * Handle call ended
     */
    private handleCallEnded(_msg: XMPPMessage): void {
        this.updateState('ended');
        this.cleanup();
    }

    /**
     * Handle call failed
     */
    private handleCallFailed(msg: XMPPMessage): void {
        console.error('[WebRTC] Call failed:', msg.reason);
        this.updateState('failed');
        this.cleanup();
    }

    /**
     * Handle remote SDP offer
     */
    private async handleRemoteOffer(msg: XMPPMessage): Promise<void> {
        // Store offer for when user answers
        if (this.currentCall && msg.sdp) {
            (this.currentCall as any).pendingOffer = msg.sdp;
        }
    }

    /**
     * Handle remote SDP answer
     */
    private async handleRemoteAnswer(msg: XMPPMessage): Promise<void> {
        if (this.peerConnection && msg.sdp) {
            await this.peerConnection.setRemoteDescription(
                new RTCSessionDescription(msg.sdp)
            );
        }
    }

    /**
     * Handle remote ICE candidate
     */
    private async handleRemoteIceCandidate(msg: XMPPMessage): Promise<void> {
        if (this.peerConnection && msg.candidate) {
            await this.peerConnection.addIceCandidate(
                new RTCIceCandidate(msg.candidate)
            );
        }
    }

    /**
     * Handle connection failed - attempt ICE restart
     */
    private async handleConnectionFailed(): Promise<void> {
        if (!this.peerConnection || !this.currentCall) {
            this.updateState('failed');
            return;
        }

        // Attempt ICE restart
        try {
            const offer = await this.peerConnection.createOffer({ iceRestart: true });
            await this.peerConnection.setLocalDescription(offer);
            getXMPPClient().sendOffer(this.currentCall.callId, offer);
            this.updateState('reconnecting');
        } catch (error) {
            console.error('[WebRTC] ICE restart failed:', error);
            this.updateState('failed');
            this.cleanup();
        }
    }

    /**
     * Cleanup call resources
     */
    private cleanup(): void {
        if (this.localStream) {
            this.localStream.getTracks().forEach(track => track.stop());
            this.localStream = null;
        }

        if (this.peerConnection) {
            this.peerConnection.close();
            this.peerConnection = null;
        }

        this.remoteStream = null;
        this.currentCall = null;
    }

    /**
     * Update call state
     */
    private updateState(state: CallState): void {
        if (this.currentCall) {
            this.currentCall.state = state;

            if (state === 'connected' && !this.currentCall.startTime) {
                this.currentCall.startTime = new Date();
            } else if (state === 'ended' || state === 'failed') {
                this.currentCall.endTime = new Date();
            }
        }

        this.stateChangeHandlers.forEach(handler => {
            handler(state, this.currentCall);
        });
    }

    /**
     * Notify local stream handlers
     */
    private notifyLocalStream(stream: MediaStream): void {
        this.localStreamHandlers.forEach(handler => handler(stream));
    }

    /**
     * Notify remote stream handlers  
     */
    private notifyRemoteStream(stream: MediaStream): void {
        this.remoteStreamHandlers.forEach(handler => handler(stream));
    }

    // Event registration
    onStateChange(handler: StateChangeHandler): () => void {
        this.stateChangeHandlers.add(handler);
        return () => this.stateChangeHandlers.delete(handler);
    }

    onLocalStream(handler: StreamHandler): () => void {
        this.localStreamHandlers.add(handler);
        return () => this.localStreamHandlers.delete(handler);
    }

    onRemoteStream(handler: StreamHandler): () => void {
        this.remoteStreamHandlers.add(handler);
        return () => this.remoteStreamHandlers.delete(handler);
    }

    // Getters
    getCurrentCall(): EnterpriseCallInfo | null {
        return this.currentCall;
    }

    getLocalStream(): MediaStream | null {
        return this.localStream;
    }

    getRemoteStream(): MediaStream | null {
        return this.remoteStream;
    }
}

// Singleton instance
let enterpriseService: EnterpriseWebRTCService | null = null;

export function getEnterpriseWebRTCService(apiUrl?: string): EnterpriseWebRTCService {
    if (!enterpriseService) {
        enterpriseService = new EnterpriseWebRTCService(apiUrl);
    }
    return enterpriseService;
}

export default EnterpriseWebRTCService;
