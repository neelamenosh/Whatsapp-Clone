/*
  Local WebSocket server for realtime messaging.
  This is intended for development/demo. In production (Vercel), you typically need
  a dedicated WS host (Fly.io/Render) or a managed realtime provider.
*/

import http from 'node:http';
import { WebSocketServer, type WebSocket } from 'ws';

type ClientMessage =
  | { type: 'subscribe'; chatId: string }
  | { type: 'unsubscribe'; chatId: string }
  | { type: 'message'; chatId: string; payload: unknown };

type ServerMessage =
  | { type: 'subscribed'; chatId: string }
  | { type: 'unsubscribed'; chatId: string }
  | { type: 'message'; chatId: string; payload: unknown };

const port = Number(process.env.WS_PORT ?? 3001);

const server = http.createServer((_req, res) => {
  res.writeHead(200, { 'content-type': 'text/plain' });
  res.end('ok');
});

const wss = new WebSocketServer({ server });

const rooms = new Map<string, Set<WebSocket>>();

function send(ws: WebSocket, msg: ServerMessage) {
  if (ws.readyState !== ws.OPEN) return;
  ws.send(JSON.stringify(msg));
}

function join(chatId: string, ws: WebSocket) {
  const set = rooms.get(chatId) ?? new Set<WebSocket>();
  set.add(ws);
  rooms.set(chatId, set);
  send(ws, { type: 'subscribed', chatId });
}

function leave(chatId: string, ws: WebSocket) {
  const set = rooms.get(chatId);
  if (!set) return;
  set.delete(ws);
  if (set.size === 0) rooms.delete(chatId);
  send(ws, { type: 'unsubscribed', chatId });
}

function broadcast(chatId: string, payload: unknown, except?: WebSocket) {
  const set = rooms.get(chatId);
  if (!set) return;
  for (const client of set) {
    if (client === except) continue;
    send(client, { type: 'message', chatId, payload });
  }
}

wss.on('connection', (ws) => {
  const subscriptions = new Set<string>();

  ws.on('message', (data) => {
    try {
      const parsed = JSON.parse(data.toString()) as ClientMessage;
      if (parsed.type === 'subscribe') {
        subscriptions.add(parsed.chatId);
        join(parsed.chatId, ws);
        return;
      }
      if (parsed.type === 'unsubscribe') {
        subscriptions.delete(parsed.chatId);
        leave(parsed.chatId, ws);
        return;
      }
      if (parsed.type === 'message') {
        broadcast(parsed.chatId, parsed.payload, ws);
        return;
      }
    } catch {
      // ignore malformed messages
    }
  });

  ws.on('close', () => {
    for (const chatId of subscriptions) {
      leave(chatId, ws);
    }
  });
});

server.listen(port, () => {
  // eslint-disable-next-line no-console
  console.log(`[ws] listening on ws://localhost:${port}`);
});
