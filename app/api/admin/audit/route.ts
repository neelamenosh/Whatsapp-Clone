import { NextResponse } from 'next/server';
import { extractAuthContext, AuthError, type AuthContext } from '@/lib/platform/auth';
import { getAuditStore } from '@/lib/infra/audit/memory-audit-store';
import { extractTenantContext } from '@/lib/platform/tenant/context';
import type { AuditAction, AuditResourceType } from '@/lib/platform/audit/types';

export const runtime = 'nodejs';

function json(data: unknown, init?: ResponseInit) {
  return NextResponse.json(data, init);
}

function handleError(err: unknown) {
  if (err instanceof AuthError) {
    return json({ error: err.message, code: err.code }, { status: err.statusCode });
  }
  const statusCode = typeof (err as any)?.statusCode === 'number' ? (err as any).statusCode : 500;
  const message = statusCode === 400 ? 'Invalid query' : 'Server error';
  return json({ error: message }, { status: statusCode });
}

/**
 * Check if user has admin privileges.
 * In production, this would check roles/permissions from auth claims.
 */
function isAdmin(auth: AuthContext): boolean {
  // Demo mode: check for admin role or specific users
  return auth.user?.roles?.includes('admin') ?? auth.user?.sub === 'admin';
}

/**
 * GET /api/admin/audit?startDate=2024-01-01&endDate=2024-01-31&actions=message.send,message.delete
 * 
 * Query audit logs (admin only).
 */
export async function GET(req: Request) {
  try {
    const auth = await extractAuthContext(req);
    const tenant = extractTenantContext(auth);
    
    // Admin check
    if (!isAdmin(auth)) {
      return json({ error: 'Admin access required' }, { status: 403 });
    }
    
    const url = new URL(req.url);
    const startDate = url.searchParams.get('startDate');
    const endDate = url.searchParams.get('endDate');
    const actorUserId = url.searchParams.get('actorUserId') ?? undefined;
    const actions = url.searchParams.get('actions')?.split(',').filter(Boolean) as AuditAction[] | undefined;
    const resourceType = url.searchParams.get('resourceType') as AuditResourceType | undefined;
    const resourceId = url.searchParams.get('resourceId') ?? undefined;
    const outcome = url.searchParams.get('outcome') as 'success' | 'failure' | 'denied' | undefined;
    const limit = parseInt(url.searchParams.get('limit') ?? '100', 10);
    const cursor = url.searchParams.get('cursor') ?? undefined;
    
    const auditStore = getAuditStore();
    
    const result = await auditStore.query({
      tenantId: tenant.tenantId,
      startDate: startDate ? new Date(startDate) : undefined,
      endDate: endDate ? new Date(endDate) : undefined,
      actorUserId,
      actions,
      resourceType,
      resourceId,
      outcome,
      limit: Math.min(limit, 1000),
      cursor,
    });
    
    return json({
      entries: result.entries,
      nextCursor: result.nextCursor,
      totalCount: result.totalCount,
    });
  } catch (err) {
    return handleError(err);
  }
}

/**
 * GET /api/admin/audit/resource/:type/:id
 * 
 * Get audit trail for a specific resource.
 */
export async function POST(req: Request) {
  try {
    const auth = await extractAuthContext(req);
    
    // Admin check
    if (!isAdmin(auth)) {
      return json({ error: 'Admin access required' }, { status: 403 });
    }
    
    const body = await req.json();
    const { resourceType, resourceId, limit = 50 } = body;
    
    if (!resourceType || !resourceId) {
      return json({ error: 'resourceType and resourceId are required' }, { status: 400 });
    }
    
    const auditStore = getAuditStore();
    const entries = await auditStore.getForResource(
      resourceType,
      resourceId,
      Math.min(limit, 500)
    );
    
    return json({ entries });
  } catch (err) {
    return handleError(err);
  }
}
