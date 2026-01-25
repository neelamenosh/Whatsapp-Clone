/*
  Local WebSocket server for realtime messaging.
  Runs with plain Node.js (no TS runtime) to avoid build-script issues.

  Start via: pnpm dev (runs next dev + this) or: node server/ws-server.mjs

  Enterprise features:
  - Server-only publishing (no client-to-client broadcast)
  - Cursor-based reconnect/catch-up support
  - Event log for replay on reconnection
*/

import http from 'node:http';
import { WebSocketServer } from 'ws';

const port = Number(process.env.WS_PORT ?? 3001);

// Event log for catch-up on reconnection (in-memory, limited size)
const MAX_EVENT_LOG = 10000;
const eventLog = []; // { chatId, payload, timestamp }

function addToEventLog(chatId, payload) {
  const entry = {
    chatId,
    payload,
    timestamp: new Date().toISOString(),
    eventId: payload.eventId || `evt-${Date.now()}-${Math.random().toString(36).slice(2)}`,
  };
  eventLog.push(entry);
  if (eventLog.length > MAX_EVENT_LOG) {
    eventLog.splice(0, eventLog.length - MAX_EVENT_LOG / 2);
  }
  return entry;
}

function getEventsSince(chatId, cursor) {
  const cursorTime = cursor ? new Date(cursor).getTime() : 0;
  return eventLog.filter(
    (e) => e.chatId === chatId && new Date(e.timestamp).getTime() > cursorTime
  );
}

// Collect body from incoming request
function collectBody(req) {
  return new Promise((resolve, reject) => {
    const chunks = [];
    req.on('data', (c) => chunks.push(c));
    req.on('end', () => resolve(Buffer.concat(chunks).toString('utf8')));
    req.on('error', reject);
  });
}

const server = http.createServer(async (req, res) => {
  // CORS headers for cross-origin requests
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type');

  if (req.method === 'OPTIONS') {
    res.writeHead(204);
    res.end();
    return;
  }

  // Health check
  if (req.method === 'GET' && req.url === '/') {
    res.writeHead(200, { 'content-type': 'text/plain' });
    res.end('ok');
    return;
  }

  // Server-only publish endpoint
  if (req.method === 'POST' && req.url === '/publish') {
    try {
      const body = await collectBody(req);
      const { chatId, payload } = JSON.parse(body);
      if (!chatId || payload === undefined) {
        res.writeHead(400, { 'content-type': 'application/json' });
        res.end(JSON.stringify({ error: 'chatId and payload required' }));
        return;
      }
      // Add to event log for catch-up support
      const entry = addToEventLog(chatId, payload);
      broadcast(chatId, { ...payload, _cursor: entry.timestamp });
      res.writeHead(200, { 'content-type': 'application/json' });
      res.end(JSON.stringify({ ok: true, cursor: entry.timestamp }));
    } catch (err) {
      res.writeHead(400, { 'content-type': 'application/json' });
      res.end(JSON.stringify({ error: 'Invalid JSON' }));
    }
    return;
  }

  // Catch-up endpoint for reconnecting clients
  if (req.method === 'GET' && req.url?.startsWith('/catchup')) {
    try {
      const url = new URL(req.url, `http://${req.headers.host}`);
      const chatId = url.searchParams.get('chatId');
      const cursor = url.searchParams.get('cursor');
      
      if (!chatId) {
        res.writeHead(400, { 'content-type': 'application/json' });
        res.end(JSON.stringify({ error: 'chatId required' }));
        return;
      }

      const events = getEventsSince(chatId, cursor);
      const nextCursor = events.length > 0 
        ? events[events.length - 1].timestamp 
        : cursor || new Date().toISOString();

      res.writeHead(200, { 'content-type': 'application/json' });
      res.end(JSON.stringify({ events: events.map(e => e.payload), nextCursor }));
    } catch (err) {
      res.writeHead(500, { 'content-type': 'application/json' });
      res.end(JSON.stringify({ error: 'Server error' }));
    }
    return;
  }

  res.writeHead(404, { 'content-type': 'text/plain' });
  res.end('not found');
});

const wss = new WebSocketServer({ server });

const rooms = new Map(); // chatId -> Set<{ ws, cursor }>

function send(ws, msg) {
  if (ws.readyState !== ws.OPEN) return;
  ws.send(JSON.stringify(msg));
}

function join(chatId, ws, cursor) {
  const set = rooms.get(chatId) ?? new Set();
  set.add({ ws, cursor });
  rooms.set(chatId, set);
  
  // Send catch-up events if cursor provided
  if (cursor) {
    const missedEvents = getEventsSince(chatId, cursor);
    for (const event of missedEvents) {
      send(ws, { type: 'message', chatId, payload: event.payload, _catchup: true });
    }
  }
  
  const latestCursor = eventLog.length > 0 
    ? eventLog[eventLog.length - 1].timestamp 
    : new Date().toISOString();
  
  send(ws, { type: 'subscribed', chatId, cursor: latestCursor });
}

function leave(chatId, ws) {
  const set = rooms.get(chatId);
  if (!set) return;
  for (const entry of set) {
    if (entry.ws === ws) {
      set.delete(entry);
      break;
    }
  }
  if (set.size === 0) rooms.delete(chatId);
  send(ws, { type: 'unsubscribed', chatId });
}

function broadcast(chatId, payload) {
  const set = rooms.get(chatId);
  if (!set) return;
  for (const { ws } of set) {
    send(ws, { type: 'message', chatId, payload });
  }
}

wss.on('connection', (ws) => {
  const subscriptions = new Map(); // chatId -> cursor

  ws.on('message', (data) => {
    try {
      const parsed = JSON.parse(data.toString());
      if (parsed?.type === 'subscribe' && typeof parsed.chatId === 'string') {
        const cursor = parsed.cursor || null; // Optional cursor for catch-up
        subscriptions.set(parsed.chatId, cursor);
        join(parsed.chatId, ws, cursor);
        return;
      }
      if (parsed?.type === 'unsubscribe' && typeof parsed.chatId === 'string') {
        subscriptions.delete(parsed.chatId);
        leave(parsed.chatId, ws);
        return;
      }
      // NOTE: Client 'message' broadcast is intentionally disabled.
      // All message fanout happens server-side via /publish endpoint.
    } catch {
      // ignore
    }
  });

  ws.on('close', () => {
    for (const chatId of subscriptions.keys()) {
      leave(chatId, ws);
    }
  });
});

server.listen(port, () => {
  // eslint-disable-next-line no-console
  console.log(`[ws] listening on ws://localhost:${port}`);
});
