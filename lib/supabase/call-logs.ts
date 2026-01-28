import { supabase, isSupabaseConfigured } from './client';
import type { RealtimeChannel } from '@supabase/supabase-js';

export interface CallLog {
    id: string;
    callerId: string;
    calleeId: string;
    callType: 'video' | 'audio';
    status: 'incoming' | 'outgoing' | 'missed' | 'declined' | 'completed';
    startedAt: Date;
    endedAt?: Date;
    duration?: number;
    callId?: string;
    createdAt: Date;
}

interface DbCallLog {
    id: string;
    caller_id: string;
    callee_id: string;
    call_type: 'video' | 'audio';
    status: 'incoming' | 'outgoing' | 'missed' | 'declined' | 'completed';
    started_at: string;
    ended_at: string | null;
    duration: number | null;
    call_id: string | null;
    created_at: string;
}

// Convert database row to CallLog
function dbToCallLog(row: DbCallLog): CallLog {
    return {
        id: row.id,
        callerId: row.caller_id,
        calleeId: row.callee_id,
        callType: row.call_type,
        status: row.status,
        startedAt: new Date(row.started_at),
        endedAt: row.ended_at ? new Date(row.ended_at) : undefined,
        duration: row.duration ?? undefined,
        callId: row.call_id ?? undefined,
        createdAt: new Date(row.created_at),
    };
}

// Create a new call log entry
export async function createCallLog(
    callerId: string,
    calleeId: string,
    callType: 'video' | 'audio',
    status: 'incoming' | 'outgoing' | 'missed' | 'declined' | 'completed',
    callId?: string
): Promise<{ data: CallLog | null; error: Error | null }> {
    if (!isSupabaseConfigured()) {
        console.error('[CallLogs] Cannot create - Supabase not configured');
        return { data: null, error: new Error('Supabase not configured') };
    }

    console.log('[CallLogs] Creating call log:', { callerId, calleeId, callType, status, callId });

    const { data, error } = await supabase!.from('call_logs').insert({
        caller_id: callerId,
        callee_id: calleeId,
        call_type: callType,
        status,
        call_id: callId,
    }).select().single();

    if (error) {
        console.error('[CallLogs] Failed to create:', error);
        return { data: null, error: new Error(error.message) };
    }

    console.log('[CallLogs] Call log created:', data);
    return { data: dbToCallLog(data as DbCallLog), error: null };
}

// Update an existing call log (e.g., when call ends)
export async function updateCallLog(
    callLogId: string,
    updates: {
        status?: 'incoming' | 'outgoing' | 'missed' | 'declined' | 'completed';
        endedAt?: Date;
        duration?: number;
    }
): Promise<{ error: Error | null }> {
    if (!isSupabaseConfigured()) {
        console.error('[CallLogs] Cannot update - Supabase not configured');
        return { error: new Error('Supabase not configured') };
    }

    console.log('[CallLogs] Updating call log:', { callLogId, updates });

    const updateData: Record<string, unknown> = {};
    if (updates.status) updateData.status = updates.status;
    if (updates.endedAt) updateData.ended_at = updates.endedAt.toISOString();
    if (updates.duration !== undefined) updateData.duration = updates.duration;

    const { error } = await supabase!.from('call_logs').update(updateData).eq('id', callLogId);

    if (error) {
        console.error('[CallLogs] Failed to update:', error);
        return { error: new Error(error.message) };
    }

    console.log('[CallLogs] Call log updated successfully');
    return { error: null };
}

// Update call log by call_id (for WebRTC integration)
export async function updateCallLogByCallId(
    callId: string,
    updates: {
        status?: 'incoming' | 'outgoing' | 'missed' | 'declined' | 'completed';
        endedAt?: Date;
        duration?: number;
    }
): Promise<{ error: Error | null }> {
    if (!isSupabaseConfigured()) {
        console.error('[CallLogs] Cannot update - Supabase not configured');
        return { error: new Error('Supabase not configured') };
    }

    console.log('[CallLogs] Updating call log by callId:', { callId, updates });

    const updateData: Record<string, unknown> = {};
    if (updates.status) updateData.status = updates.status;
    if (updates.endedAt) updateData.ended_at = updates.endedAt.toISOString();
    if (updates.duration !== undefined) updateData.duration = updates.duration;

    const { error } = await supabase!.from('call_logs').update(updateData).eq('call_id', callId);

    if (error) {
        console.error('[CallLogs] Failed to update by callId:', error);
        return { error: new Error(error.message) };
    }

    console.log('[CallLogs] Call log updated by callId successfully');
    return { error: null };
}

// Get call logs for a user (both as caller and callee)
export async function getCallLogsForUser(
    userId: string,
    limit: number = 50
): Promise<{ data: CallLog[]; error: Error | null }> {
    if (!isSupabaseConfigured()) {
        console.error('[CallLogs] Cannot fetch - Supabase not configured');
        return { data: [], error: new Error('Supabase not configured') };
    }

    console.log('[CallLogs] Fetching call logs for user:', userId);

    const { data, error } = await supabase!
        .from('call_logs')
        .select('*')
        .or(`caller_id.eq.${userId},callee_id.eq.${userId}`)
        .order('started_at', { ascending: false })
        .limit(limit);

    if (error) {
        console.error('[CallLogs] Failed to fetch:', error);
        return { data: [], error: new Error(error.message) };
    }

    const callLogs = (data as DbCallLog[]).map(dbToCallLog);
    console.log('[CallLogs] Fetched call logs:', callLogs.length);
    return { data: callLogs, error: null };
}

// Subscribe to real-time call log updates for a user
export function subscribeToCallLogs(
    userId: string,
    onNewCall: (callLog: CallLog) => void,
    onUpdateCall?: (callLog: CallLog) => void
): RealtimeChannel | null {
    if (!isSupabaseConfigured()) {
        console.warn('[CallLogs] Supabase not configured');
        return null;
    }

    console.log('[CallLogs] Subscribing to call logs for user:', userId);

    const channel = supabase!
        .channel(`call_logs:${userId}`)
        .on(
            'postgres_changes',
            {
                event: 'INSERT',
                schema: 'public',
                table: 'call_logs',
            },
            (payload) => {
                const row = payload.new as DbCallLog;
                // Only process if user is involved
                if (row.caller_id === userId || row.callee_id === userId) {
                    console.log('[CallLogs] New call log received:', row);
                    onNewCall(dbToCallLog(row));
                }
            }
        )
        .on(
            'postgres_changes',
            {
                event: 'UPDATE',
                schema: 'public',
                table: 'call_logs',
            },
            (payload) => {
                const row = payload.new as DbCallLog;
                if ((row.caller_id === userId || row.callee_id === userId) && onUpdateCall) {
                    console.log('[CallLogs] Call log updated:', row);
                    onUpdateCall(dbToCallLog(row));
                }
            }
        )
        .subscribe((status) => {
            console.log('[CallLogs] Subscription status:', status);
        });

    return channel;
}

// Unsubscribe from call logs channel
export function unsubscribeFromCallLogs(channel: RealtimeChannel | null): void {
    if (channel && isSupabaseConfigured()) {
        supabase!.removeChannel(channel);
    }
}

// Clear all call logs for a user
export async function clearCallLogsForUser(userId: string): Promise<{ error: Error | null }> {
    if (!isSupabaseConfigured()) {
        console.error('[CallLogs] Cannot clear - Supabase not configured');
        return { error: new Error('Supabase not configured') };
    }

    console.log('[CallLogs] Clearing all call logs for user:', userId);

    const { error } = await supabase!
        .from('call_logs')
        .delete()
        .or(`caller_id.eq.${userId},callee_id.eq.${userId}`);

    if (error) {
        console.error('[CallLogs] Failed to clear:', error);
        return { error: new Error(error.message) };
    }

    console.log('[CallLogs] Call logs cleared successfully');
    return { error: null };
}
