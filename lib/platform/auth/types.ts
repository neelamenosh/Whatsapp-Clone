import { z } from 'zod';

// Auth context passed through request pipeline.
// In production, populate from validated JWT claims.

export const UserClaimsSchema = z.object({
  sub: z.string().min(1), // User ID (subject)
  email: z.string().email().optional(),
  name: z.string().optional(),
  roles: z.array(z.string()).default([]),
  // Multi-tenant support (future)
  tenantId: z.string().optional(),
});

export type UserClaims = z.infer<typeof UserClaimsSchema>;

export interface AuthContext {
  isAuthenticated: boolean;
  user: UserClaims | null;
  // Scopes/permissions for fine-grained access control
  scopes: string[];
}

export const ANONYMOUS_AUTH: AuthContext = {
  isAuthenticated: false,
  user: null,
  scopes: [],
};

// Standard auth-related errors
export class AuthError extends Error {
  constructor(
    message: string,
    public readonly code: 'UNAUTHENTICATED' | 'FORBIDDEN' | 'INVALID_TOKEN',
    public readonly statusCode: number = 401
  ) {
    super(message);
    this.name = 'AuthError';
  }
}

export function unauthenticated(message = 'Authentication required'): AuthError {
  return new AuthError(message, 'UNAUTHENTICATED', 401);
}

export function forbidden(message = 'Access denied'): AuthError {
  return new AuthError(message, 'FORBIDDEN', 403);
}
