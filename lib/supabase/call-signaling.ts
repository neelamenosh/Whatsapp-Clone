import { supabase, isSupabaseConfigured } from './client';
import type { RealtimeChannel } from '@supabase/supabase-js';

export interface SignalingMessage {
    id: string;
    callerId: string;
    calleeId: string;
    type: 'offer' | 'answer' | 'ice-candidate' | 'end' | 'reject' | 'busy';
    payload: RTCSessionDescriptionInit | RTCIceCandidateInit | null;
    callType: 'video' | 'audio';
    callId: string;
    createdAt: string;
}

// Send a signaling message
export async function sendSignalingMessage(
    callerId: string,
    calleeId: string,
    type: SignalingMessage['type'],
    callId: string,
    payload: SignalingMessage['payload'] = null,
    callType: 'video' | 'audio' = 'video'
): Promise<{ error: Error | null }> {
    if (!isSupabaseConfigured()) {
        console.error('[CallSignaling] Cannot send - Supabase not configured');
        return { error: new Error('Supabase not configured') };
    }

    console.log('[CallSignaling] Sending signaling message:', { type, callerId, calleeId, callId, callType });

    const { error, data } = await supabase!.from('call_signaling').insert({
        caller_id: callerId,
        callee_id: calleeId,
        type,
        payload,
        call_type: callType,
        call_id: callId,
    }).select();

    if (error) {
        console.error('[CallSignaling] Failed to send:', error);
        return { error: new Error(error.message) };
    }

    console.log('[CallSignaling] Message sent successfully:', data);
    return { error: null };
}

// Subscribe to incoming signaling messages for a user
export function subscribeToSignaling(
    userId: string,
    onMessage: (message: SignalingMessage) => void
): RealtimeChannel | null {
    if (!isSupabaseConfigured()) {
        console.warn('[CallSignaling] Supabase not configured');
        return null;
    }

    console.log('[CallSignaling] Subscribing to incoming calls for user:', userId);

    const channel = supabase!
        .channel(`signaling:${userId}`)
        .on(
            'postgres_changes',
            {
                event: 'INSERT',
                schema: 'public',
                table: 'call_signaling',
                filter: `callee_id=eq.${userId}`,
            },
            (payload) => {
                console.log('[CallSignaling] Received signaling message:', payload);
                const row = payload.new as any;
                onMessage({
                    id: row.id,
                    callerId: row.caller_id,
                    calleeId: row.callee_id,
                    type: row.type,
                    payload: row.payload,
                    callType: row.call_type,
                    callId: row.call_id,
                    createdAt: row.created_at,
                });
            }
        )
        .subscribe((status) => {
            console.log('[CallSignaling] Subscription status:', status);
            if (status === 'SUBSCRIBED') {
                console.log('[CallSignaling] Successfully subscribed to call signals');
            } else if (status === 'CHANNEL_ERROR') {
                console.error('[CallSignaling] Failed to subscribe - check if call_signaling table exists and Realtime is enabled');
            }
        });

    return channel;
}

// Subscribe to signaling messages for a specific call (for caller to receive answers)
export function subscribeToCallSignaling(
    callId: string,
    userId: string,
    onMessage: (message: SignalingMessage) => void
): RealtimeChannel | null {
    if (!isSupabaseConfigured()) {
        return null;
    }

    const channel = supabase!
        .channel(`call:${callId}`)
        .on(
            'postgres_changes',
            {
                event: 'INSERT',
                schema: 'public',
                table: 'call_signaling',
                filter: `call_id=eq.${callId}`,
            },
            (payload) => {
                const row = payload.new as any;
                // Only process messages intended for this user
                if (row.callee_id === userId) {
                    onMessage({
                        id: row.id,
                        callerId: row.caller_id,
                        calleeId: row.callee_id,
                        type: row.type,
                        payload: row.payload,
                        callType: row.call_type,
                        callId: row.call_id,
                        createdAt: row.created_at,
                    });
                }
            }
        )
        .subscribe();

    return channel;
}

// Clean up signaling messages for a call
export async function cleanupSignaling(callId: string): Promise<void> {
    if (!isSupabaseConfigured()) return;

    await supabase!.from('call_signaling').delete().eq('call_id', callId);
}

// Unsubscribe from a channel
export function unsubscribeFromChannel(channel: RealtimeChannel | null): void {
    if (channel && isSupabaseConfigured()) {
        supabase!.removeChannel(channel);
    }
}
