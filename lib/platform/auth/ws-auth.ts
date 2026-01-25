// Auth middleware for WebSocket connections.
// Validates auth token on connection upgrade.

import type { IncomingMessage } from 'node:http';
import type { AuthContext, UserClaims } from '@/lib/platform/auth/types';

// Demo user header for development
const DEMO_USER_HEADER = 'x-demo-user-id';

/**
 * Extract auth context from WebSocket upgrade request.
 * Called during the connection handshake.
 */
export function extractWsAuthContext(req: IncomingMessage): AuthContext {
  // Demo mode: allow x-demo-user-id header
  const demoUserId = req.headers[DEMO_USER_HEADER] as string | undefined;
  if (demoUserId && process.env.NODE_ENV !== 'production') {
    const user: UserClaims = {
      sub: demoUserId,
      name: `Demo User (${demoUserId})`,
      roles: ['user'],
    };
    return {
      isAuthenticated: true,
      user,
      scopes: ['messages:read', 'messages:write'],
    };
  }

  // Check for token in query string (common pattern for WebSocket auth)
  const url = new URL(req.url ?? '', `http://${req.headers.host}`);
  const token = url.searchParams.get('token');

  if (!token) {
    // Check Authorization header
    const authHeader = req.headers['authorization'];
    if (!authHeader || !authHeader.startsWith('Bearer ')) {
      return { isAuthenticated: false, user: null, scopes: [] };
    }
    return decodeToken(authHeader.slice(7));
  }

  return decodeToken(token);
}

/**
 * Decode JWT token (demo - no verification).
 * In production, verify signature.
 */
function decodeToken(token: string): AuthContext {
  try {
    const parts = token.split('.');
    if (parts.length !== 3) {
      return { isAuthenticated: false, user: null, scopes: [] };
    }

    const payload = JSON.parse(Buffer.from(parts[1], 'base64url').toString('utf8'));
    
    if (!payload.sub) {
      return { isAuthenticated: false, user: null, scopes: [] };
    }

    const user: UserClaims = {
      sub: payload.sub,
      email: payload.email,
      name: payload.name,
      roles: payload.roles ?? ['user'],
      tenantId: payload.tenantId,
    };

    return {
      isAuthenticated: true,
      user,
      scopes: typeof payload.scope === 'string' 
        ? payload.scope.split(' ') 
        : ['messages:read', 'messages:write'],
    };
  } catch {
    return { isAuthenticated: false, user: null, scopes: [] };
  }
}

/**
 * Validate that user can subscribe to a chat.
 * Returns true if allowed, false otherwise.
 */
export async function canSubscribeToChat(
  auth: AuthContext,
  chatId: string
): Promise<boolean> {
  // In demo mode without enforcement, allow all
  if (process.env.ENFORCE_AUTH !== 'true') {
    return true;
  }

  if (!auth.isAuthenticated || !auth.user) {
    return false;
  }

  // TODO: Check chat membership from database
  // For now, allow all authenticated users
  return true;
}
