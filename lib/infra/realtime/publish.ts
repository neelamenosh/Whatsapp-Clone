// Server-side realtime publish helper.
// In dev, calls the local WS server's /publish endpoint.
// In production, this would call a managed pub/sub service.

const WS_INTERNAL_URL = process.env.WS_INTERNAL_URL ?? 'http://localhost:3001';

export async function publishToRealtime(chatId: string, payload: unknown): Promise<void> {
  try {
    const res = await fetch(`${WS_INTERNAL_URL}/publish`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ chatId, payload }),
    });
    if (!res.ok) {
      console.error('[realtime] publish failed:', res.status, await res.text());
    }
  } catch (err) {
    // WS server may not be running (e.g., in serverless); log and continue.
    console.error('[realtime] publish error:', err);
  }
}
