import type { AuthContext, UserClaims } from './types';
import { ANONYMOUS_AUTH, UserClaimsSchema } from './types';

// Header name for auth token (standard Bearer token)
const AUTH_HEADER = 'authorization';

// Demo mode header - allows impersonating a user for development
const DEMO_USER_HEADER = 'x-demo-user-id';

/**
 * Extract and validate auth context from request headers.
 * 
 * In production:
 * - Validate JWT signature against JWKS
 * - Check token expiration
 * - Verify issuer and audience
 * 
 * For demo/dev:
 * - Accept x-demo-user-id header for easy testing
 * - Skip signature validation
 */
export async function extractAuthContext(req: Request): Promise<AuthContext> {
  // Demo mode: allow x-demo-user-id header for development
  const demoUserId = req.headers.get(DEMO_USER_HEADER);
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

  // Production: validate Bearer token
  const authHeader = req.headers.get(AUTH_HEADER);
  if (!authHeader) {
    return ANONYMOUS_AUTH;
  }

  const [scheme, token] = authHeader.split(' ');
  if (scheme?.toLowerCase() !== 'bearer' || !token) {
    return ANONYMOUS_AUTH;
  }

  try {
    // TODO: In production, validate JWT here:
    // 1. Verify signature using JWKS from your IdP
    // 2. Check exp, iat, nbf claims
    // 3. Verify iss and aud match your app
    // 
    // For now, decode without verification (demo mode)
    const payload = decodeJwtPayload(token);
    if (!payload) {
      return ANONYMOUS_AUTH;
    }

    const parsed = UserClaimsSchema.safeParse(payload);
    if (!parsed.success) {
      return ANONYMOUS_AUTH;
    }

    return {
      isAuthenticated: true,
      user: parsed.data,
      scopes: extractScopes(payload),
    };
  } catch {
    return ANONYMOUS_AUTH;
  }
}

/**
 * Decode JWT payload without verification (demo only).
 * In production, use a proper JWT library with signature verification.
 */
function decodeJwtPayload(token: string): Record<string, unknown> | null {
  try {
    const parts = token.split('.');
    if (parts.length !== 3) return null;
    
    const payload = parts[1];
    const decoded = Buffer.from(payload, 'base64url').toString('utf8');
    return JSON.parse(decoded);
  } catch {
    return null;
  }
}

/**
 * Extract scopes from token payload.
 */
function extractScopes(payload: Record<string, unknown>): string[] {
  // Standard OIDC scope claim
  if (typeof payload.scope === 'string') {
    return payload.scope.split(' ').filter(Boolean);
  }
  // Array format
  if (Array.isArray(payload.scopes)) {
    return payload.scopes.filter((s): s is string => typeof s === 'string');
  }
  // Default scopes for authenticated users
  return ['messages:read', 'messages:write'];
}

/**
 * Require authentication - throws if not authenticated.
 */
export function requireAuth(auth: AuthContext): asserts auth is AuthContext & { isAuthenticated: true; user: UserClaims } {
  if (!auth.isAuthenticated || !auth.user) {
    const { unauthenticated } = require('./types');
    throw unauthenticated();
  }
}

/**
 * Require specific scope - throws if missing.
 */
export function requireScope(auth: AuthContext, scope: string): void {
  requireAuth(auth);
  if (!auth.scopes.includes(scope)) {
    const { forbidden } = require('./types');
    throw forbidden(`Missing required scope: ${scope}`);
  }
}
