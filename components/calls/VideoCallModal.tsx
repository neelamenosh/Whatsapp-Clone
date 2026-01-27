'use client';

import { useState, useEffect, useRef, useCallback } from 'react';
import { cn } from '@/lib/utils';
import {
    getWebRTCService,
    type CallState,
    type CallInfo,
    type ConnectionQuality
} from '@/lib/webrtc';
import {
    Mic,
    MicOff,
    Video,
    VideoOff,
    Phone,
    PhoneOff,
    Maximize2,
    Minimize2,
    User as UserIcon,
    Wifi,
    WifiOff,
} from 'lucide-react';

interface VideoCallModalProps {
    isOpen: boolean;
    onClose: () => void;
    callInfo: CallInfo | null;
    isIncoming?: boolean;
    incomingOffer?: RTCSessionDescriptionInit | null;
    userId: string;
}

export function VideoCallModal({
    isOpen,
    onClose,
    callInfo,
    isIncoming = false,
    incomingOffer = null,
    userId,
}: VideoCallModalProps) {
    const [callState, setCallState] = useState<CallState>('idle');
    const [callDuration, setCallDuration] = useState(0);
    const [isAudioMuted, setIsAudioMuted] = useState(false);
    const [isVideoDisabled, setIsVideoDisabled] = useState(false);
    const [isFullscreen, setIsFullscreen] = useState(false);
    const [localStream, setLocalStream] = useState<MediaStream | null>(null);
    const [remoteStream, setRemoteStream] = useState<MediaStream | null>(null);
    const [connectionQuality, setConnectionQuality] = useState<ConnectionQuality>({ state: 'good' });

    const localVideoRef = useRef<HTMLVideoElement>(null);
    const remoteVideoRef = useRef<HTMLVideoElement>(null);
    const containerRef = useRef<HTMLDivElement>(null);
    const durationIntervalRef = useRef<NodeJS.Timeout | null>(null);

    const webrtcService = getWebRTCService();

    // Setup WebRTC callbacks
    useEffect(() => {
        webrtcService.setCallbacks(
            (state: CallState) => {
                setCallState(state);
                if (state === 'connected') {
                    // Start duration timer
                    durationIntervalRef.current = setInterval(() => {
                        setCallDuration((prev) => prev + 1);
                    }, 1000);
                }
                if (state === 'ended' || state === 'failed') {
                    onClose();
                }
            },
            (stream: MediaStream | null) => {
                setRemoteStream(stream);
            },
            (quality: ConnectionQuality) => {
                setConnectionQuality(quality);
            }
        );

        return () => {
            if (durationIntervalRef.current) {
                clearInterval(durationIntervalRef.current);
            }
        };
    }, [webrtcService, onClose]);

    // Initialize call
    useEffect(() => {
        if (!isOpen || !callInfo) return;

        const initCall = async () => {
            try {
                if (isIncoming && incomingOffer) {
                    // Answer incoming call
                    await webrtcService.answerCall(userId, callInfo, incomingOffer);
                } else if (!isIncoming) {
                    // Initiate outgoing call
                    await webrtcService.initiateCall(userId, callInfo.calleeId, callInfo.callType);
                }

                // Get local stream for display
                const stream = await webrtcService.getLocalStream(callInfo.callType === 'video', true);
                setLocalStream(stream);
            } catch (error) {
                console.error('Error initializing call:', error);
                setCallState('failed');
            }
        };

        initCall();
    }, [isOpen, callInfo, isIncoming, incomingOffer, userId, webrtcService]);

    // Attach streams to video elements
    useEffect(() => {
        if (localVideoRef.current && localStream) {
            localVideoRef.current.srcObject = localStream;
        }
    }, [localStream]);

    useEffect(() => {
        if (remoteVideoRef.current && remoteStream) {
            remoteVideoRef.current.srcObject = remoteStream;
        }
    }, [remoteStream]);

    // Handle end call
    const handleEndCall = useCallback(async () => {
        await webrtcService.endCall();
        onClose();
    }, [webrtcService, onClose]);

    // Toggle audio mute
    const toggleAudio = useCallback(() => {
        const newMuted = !isAudioMuted;
        setIsAudioMuted(newMuted);
        webrtcService.toggleAudio(newMuted);
    }, [isAudioMuted, webrtcService]);

    // Toggle video
    const toggleVideo = useCallback(() => {
        const newDisabled = !isVideoDisabled;
        setIsVideoDisabled(newDisabled);
        webrtcService.toggleVideo(newDisabled);
    }, [isVideoDisabled, webrtcService]);

    // Toggle fullscreen
    const toggleFullscreen = useCallback(() => {
        if (!containerRef.current) return;

        if (!isFullscreen) {
            containerRef.current.requestFullscreen?.();
        } else {
            document.exitFullscreen?.();
        }
        setIsFullscreen(!isFullscreen);
    }, [isFullscreen]);

    // Format duration
    const formatDuration = (seconds: number): string => {
        const mins = Math.floor(seconds / 60);
        const secs = seconds % 60;
        return `${mins.toString().padStart(2, '0')}:${secs.toString().padStart(2, '0')}`;
    };

    // Get status text
    const getStatusText = (): string => {
        switch (callState) {
            case 'calling':
                return 'Calling...';
            case 'ringing':
                return 'Ringing...';
            case 'connecting':
                return 'Connecting...';
            case 'reconnecting':
                return 'Reconnecting...';
            case 'connected':
                return formatDuration(callDuration);
            case 'failed':
                return 'Call failed';
            case 'ended':
                return 'Call ended';
            default:
                return '';
        }
    };

    // Get quality indicator color
    const getQualityColor = (): string => {
        switch (connectionQuality.state) {
            case 'excellent': return 'text-green-400';
            case 'good': return 'text-green-300';
            case 'poor': return 'text-yellow-400';
            case 'disconnected': return 'text-red-400';
            default: return 'text-gray-400';
        }
    };

    if (!isOpen) return null;

    return (
        <div
            ref={containerRef}
            className="fixed inset-0 z-50 bg-black flex flex-col"
        >
            {/* Remote video (full screen) */}
            <div className="flex-1 relative bg-black flex items-center justify-center">
                {remoteStream ? (
                    <video
                        ref={remoteVideoRef}
                        autoPlay
                        playsInline
                        className="w-full h-full object-contain"
                    />
                ) : (
                    <div className="w-full h-full flex items-center justify-center bg-gradient-to-br from-gray-900 to-gray-800">
                        <div className="flex flex-col items-center gap-4">
                            <div className="w-24 h-24 rounded-full bg-gray-700 flex items-center justify-center">
                                {callInfo?.callerAvatar ? (
                                    <img
                                        src={callInfo.callerAvatar}
                                        alt=""
                                        className="w-full h-full rounded-full object-cover"
                                    />
                                ) : (
                                    <UserIcon className="w-12 h-12 text-gray-400" />
                                )}
                            </div>
                            <p className="text-white text-lg font-medium">
                                {callInfo?.callerName || 'Unknown'}
                            </p>
                            <p className="text-gray-400 text-sm animate-pulse">
                                {getStatusText()}
                            </p>
                        </div>
                    </div>
                )}

                {/* Status overlay */}
                <div className="absolute top-4 left-0 right-0 flex justify-center">
                    <div className="bg-black/50 backdrop-blur-sm px-4 py-2 rounded-full flex items-center gap-2">
                        {/* Connection quality indicator */}
                        {callState === 'connected' && (
                            <Wifi className={cn("w-4 h-4", getQualityColor())} />
                        )}
                        {callState === 'reconnecting' && (
                            <WifiOff className="w-4 h-4 text-yellow-400 animate-pulse" />
                        )}
                        <p className="text-white text-sm font-medium">{getStatusText()}</p>
                    </div>
                </div>

                {/* Reconnecting overlay */}
                {callState === 'reconnecting' && (
                    <div className="absolute inset-0 bg-black/60 flex items-center justify-center">
                        <div className="bg-black/80 backdrop-blur-sm px-6 py-4 rounded-2xl flex flex-col items-center gap-3">
                            <div className="w-10 h-10 border-3 border-white/20 border-t-white rounded-full animate-spin" />
                            <p className="text-white text-sm">Reconnecting...</p>
                            <p className="text-gray-400 text-xs">Poor connection detected</p>
                        </div>
                    </div>
                )}

                {/* Local video (picture-in-picture) */}
                <div
                    className={cn(
                        'absolute bottom-24 right-4 rounded-2xl overflow-hidden shadow-2xl border-2 border-white/20',
                        'w-24 h-32 sm:w-32 sm:h-44 md:w-40 md:h-56',
                        isVideoDisabled && 'bg-gray-800'
                    )}
                >
                    {localStream && !isVideoDisabled ? (
                        <video
                            ref={localVideoRef}
                            autoPlay
                            playsInline
                            muted
                            className="w-full h-full object-cover mirror"
                        />
                    ) : (
                        <div className="w-full h-full flex items-center justify-center bg-gray-800">
                            <VideoOff className="w-8 h-8 text-gray-400" />
                        </div>
                    )}
                </div>

                {/* Fullscreen toggle */}
                <button
                    onClick={toggleFullscreen}
                    className="absolute top-4 right-4 w-10 h-10 rounded-full bg-black/50 backdrop-blur-sm flex items-center justify-center text-white hover:bg-black/70 transition-colors"
                >
                    {isFullscreen ? (
                        <Minimize2 className="w-5 h-5" />
                    ) : (
                        <Maximize2 className="w-5 h-5" />
                    )}
                </button>
            </div>

            {/* Controls */}
            <div className="bg-gradient-to-t from-black via-black/80 to-transparent py-8 px-6">
                <div className="flex items-center justify-center gap-6">
                    {/* Mute audio */}
                    <button
                        onClick={toggleAudio}
                        className={cn(
                            'w-14 h-14 rounded-full flex items-center justify-center transition-all duration-200',
                            isAudioMuted
                                ? 'bg-white text-black'
                                : 'bg-white/20 text-white hover:bg-white/30'
                        )}
                    >
                        {isAudioMuted ? (
                            <MicOff className="w-6 h-6" />
                        ) : (
                            <Mic className="w-6 h-6" />
                        )}
                    </button>

                    {/* Disable video */}
                    <button
                        onClick={toggleVideo}
                        className={cn(
                            'w-14 h-14 rounded-full flex items-center justify-center transition-all duration-200',
                            isVideoDisabled
                                ? 'bg-white text-black'
                                : 'bg-white/20 text-white hover:bg-white/30'
                        )}
                    >
                        {isVideoDisabled ? (
                            <VideoOff className="w-6 h-6" />
                        ) : (
                            <Video className="w-6 h-6" />
                        )}
                    </button>

                    {/* End call */}
                    <button
                        onClick={handleEndCall}
                        className="w-16 h-16 rounded-full bg-red-500 flex items-center justify-center text-white hover:bg-red-600 transition-colors shadow-lg shadow-red-500/30"
                    >
                        <PhoneOff className="w-7 h-7" />
                    </button>
                </div>
            </div>

            <style jsx>{`
        .mirror {
          transform: scaleX(-1);
        }
      `}</style>
        </div>
    );
}
