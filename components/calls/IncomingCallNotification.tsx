'use client';

import { useEffect, useRef } from 'react';
import { cn } from '@/lib/utils';
import type { CallInfo } from '@/lib/webrtc';
import { Phone, PhoneOff, Video, User as UserIcon } from 'lucide-react';

interface IncomingCallNotificationProps {
    isOpen: boolean;
    callInfo: CallInfo | null;
    onAccept: () => void;
    onDecline: () => void;
}

export function IncomingCallNotification({
    isOpen,
    callInfo,
    onAccept,
    onDecline,
}: IncomingCallNotificationProps) {
    const audioRef = useRef<HTMLAudioElement | null>(null);

    // Play ringtone
    useEffect(() => {
        if (isOpen) {
            // Create ringtone (using Web Audio API for a simple ring)
            try {
                const audioContext = new (window.AudioContext || (window as any).webkitAudioContext)();
                const oscillator = audioContext.createOscillator();
                const gainNode = audioContext.createGain();

                oscillator.connect(gainNode);
                gainNode.connect(audioContext.destination);

                oscillator.frequency.value = 440;
                oscillator.type = 'sine';
                gainNode.gain.value = 0.1;

                // Ring pattern
                const ringPattern = () => {
                    oscillator.start();

                    // Ring for 1 second, pause for 2 seconds
                    const ring = () => {
                        gainNode.gain.setValueAtTime(0.1, audioContext.currentTime);
                        gainNode.gain.setValueAtTime(0, audioContext.currentTime + 1);
                    };

                    ring();
                    const interval = setInterval(ring, 3000);

                    return () => {
                        clearInterval(interval);
                        oscillator.stop();
                        audioContext.close();
                    };
                };

                const cleanup = ringPattern();

                return () => {
                    cleanup();
                };
            } catch (e) {
                // Audio not supported
                console.warn('Audio ringtone not supported');
            }
        }
    }, [isOpen]);

    if (!isOpen || !callInfo) return null;

    const isVideoCall = callInfo.callType === 'video';

    return (
        <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 backdrop-blur-sm animate-in fade-in duration-200">
            <div className="bg-gradient-to-br from-gray-900 to-gray-800 rounded-3xl p-8 max-w-sm w-full mx-4 shadow-2xl border border-white/10">
                {/* Caller info */}
                <div className="flex flex-col items-center mb-8">
                    {/* Avatar with ring animation */}
                    <div className="relative mb-4">
                        <div className="absolute inset-0 rounded-full bg-green-500/30 animate-ping" />
                        <div className="absolute inset-0 rounded-full bg-green-500/20 animate-pulse" style={{ animationDelay: '0.5s' }} />
                        <div className="relative w-24 h-24 rounded-full bg-gray-700 flex items-center justify-center overflow-hidden ring-4 ring-green-500/40">
                            {callInfo.callerAvatar ? (
                                <img
                                    src={callInfo.callerAvatar}
                                    alt={callInfo.callerName || ''}
                                    className="w-full h-full object-cover"
                                />
                            ) : (
                                <UserIcon className="w-12 h-12 text-gray-400" />
                            )}
                        </div>
                    </div>

                    {/* Caller name */}
                    <h2 className="text-xl font-semibold text-white mb-1">
                        {callInfo.callerName || 'Unknown'}
                    </h2>

                    {/* Call type */}
                    <div className="flex items-center gap-2 text-gray-400">
                        {isVideoCall ? (
                            <Video className="w-4 h-4" />
                        ) : (
                            <Phone className="w-4 h-4" />
                        )}
                        <span className="text-sm">
                            Incoming {isVideoCall ? 'video' : 'voice'} call...
                        </span>
                    </div>
                </div>

                {/* Action buttons */}
                <div className="flex items-center justify-center gap-8">
                    {/* Decline */}
                    <button
                        onClick={onDecline}
                        className="flex flex-col items-center gap-2 group"
                    >
                        <div className="w-16 h-16 rounded-full bg-red-500 flex items-center justify-center text-white shadow-lg shadow-red-500/30 group-hover:bg-red-600 transition-colors group-active:scale-95">
                            <PhoneOff className="w-7 h-7" />
                        </div>
                        <span className="text-sm text-gray-400 group-hover:text-white transition-colors">
                            Decline
                        </span>
                    </button>

                    {/* Accept */}
                    <button
                        onClick={onAccept}
                        className="flex flex-col items-center gap-2 group"
                    >
                        <div className="w-16 h-16 rounded-full bg-green-500 flex items-center justify-center text-white shadow-lg shadow-green-500/30 group-hover:bg-green-600 transition-colors group-active:scale-95 animate-bounce">
                            {isVideoCall ? (
                                <Video className="w-7 h-7" />
                            ) : (
                                <Phone className="w-7 h-7" />
                            )}
                        </div>
                        <span className="text-sm text-gray-400 group-hover:text-white transition-colors">
                            Accept
                        </span>
                    </button>
                </div>
            </div>
        </div>
    );
}
