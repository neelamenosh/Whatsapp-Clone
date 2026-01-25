// Multi-tenant support for enterprise deployments.
// Tenant context is extracted from auth claims and propagated through the request pipeline.

import type { AuthContext } from '@/lib/platform/auth';

export interface TenantContext {
  tenantId: string;
  tenantName?: string;
  // Tenant-specific feature flags
  features: {
    maxMessageSize: number;
    maxAttachmentSize: number;
    retentionDays: number | null; // null = unlimited
    auditLoggingEnabled: boolean;
    e2eEncryptionRequired: boolean;
  };
}

// Default tenant for non-enterprise (single-tenant) deployments
export const DEFAULT_TENANT: TenantContext = {
  tenantId: 'default',
  tenantName: 'Default Tenant',
  features: {
    maxMessageSize: 65536, // 64KB
    maxAttachmentSize: 25 * 1024 * 1024, // 25MB
    retentionDays: null, // Unlimited
    auditLoggingEnabled: false,
    e2eEncryptionRequired: false,
  },
};

// Tenant store interface for multi-tenant deployments
export interface TenantStore {
  getTenant(tenantId: string): Promise<TenantContext | null>;
  listTenants(): Promise<TenantContext[]>;
}

// Demo implementation - single tenant
export const demoTenantStore: TenantStore = {
  async getTenant(tenantId: string): Promise<TenantContext | null> {
    if (tenantId === 'default') return DEFAULT_TENANT;
    // Enterprise: look up tenant from database
    return null;
  },
  async listTenants(): Promise<TenantContext[]> {
    return [DEFAULT_TENANT];
  },
};

/**
 * Extract tenant context from auth context.
 * In multi-tenant deployments, tenantId comes from JWT claims.
 */
export function extractTenantContext(auth: AuthContext): TenantContext {
  const tenantId = auth.user?.tenantId ?? 'default';
  // In production, look up tenant config from database/cache
  return { ...DEFAULT_TENANT, tenantId };
}

/**
 * Validate that a resource belongs to the current tenant.
 * Throws if tenant mismatch (prevents cross-tenant data access).
 */
export function validateTenantAccess(
  resourceTenantId: string,
  currentTenantId: string
): void {
  if (resourceTenantId !== currentTenantId) {
    const error = new Error('Access denied: resource belongs to different tenant');
    (error as any).statusCode = 403;
    (error as any).code = 'TENANT_MISMATCH';
    throw error;
  }
}
