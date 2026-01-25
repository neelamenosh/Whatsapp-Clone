import { NextResponse } from 'next/server';
import { extractAuthContext, AuthError, type AuthContext } from '@/lib/platform/auth';
import { getRetentionService } from '@/lib/platform/retention/service';
import { extractTenantContext } from '@/lib/platform/tenant/context';

export const runtime = 'nodejs';

function json(data: unknown, init?: ResponseInit) {
  return NextResponse.json(data, init);
}

function handleError(err: unknown) {
  if (err instanceof AuthError) {
    return json({ error: err.message, code: err.code }, { status: err.statusCode });
  }
  const statusCode = typeof (err as any)?.statusCode === 'number' ? (err as any).statusCode : 500;
  const message = statusCode === 400 ? 'Invalid request' : 'Server error';
  return json({ error: message }, { status: statusCode });
}

/**
 * Check if user has admin privileges.
 */
function isAdmin(auth: AuthContext): boolean {
  return auth.user?.roles?.includes('admin') ?? auth.user?.sub === 'admin';
}

/**
 * POST /api/admin/compliance
 * 
 * Execute compliance operations:
 * - action: 'export-data' - Export user data (GDPR data subject request)
 * - action: 'delete-data' - Delete user data (GDPR right to erasure)
 * - action: 'run-cleanup' - Execute retention policy cleanup
 */
export async function POST(req: Request) {
  try {
    const auth = await extractAuthContext(req);
    const tenant = extractTenantContext(auth);
    
    // Admin check for most operations
    const body = await req.json();
    const { action, userId } = body;
    
    // Users can request their own data export; other operations require admin
    const isSelfRequest = action === 'export-data' && userId === auth.user?.sub;
    if (!isSelfRequest && !isAdmin(auth)) {
      return json({ error: 'Admin access required' }, { status: 403 });
    }
    
    const retentionService = getRetentionService();
    
    switch (action) {
      case 'export-data': {
        if (!userId) {
          return json({ error: 'userId is required' }, { status: 400 });
        }
        const exportData = await retentionService.exportUserData(tenant.tenantId, userId);
        return json({ export: exportData });
      }
      
      case 'delete-data': {
        if (!userId) {
          return json({ error: 'userId is required' }, { status: 400 });
        }
        const result = await retentionService.deleteUserData(tenant.tenantId, userId);
        return json({ deletion: result });
      }
      
      case 'run-cleanup': {
        const result = await retentionService.runCleanup(tenant.tenantId);
        return json({ cleanup: result });
      }
      
      case 'check-legal-hold': {
        const { messageId, chatId } = body;
        if (!messageId || !chatId) {
          return json({ error: 'messageId and chatId are required' }, { status: 400 });
        }
        const isHeld = await retentionService.isUnderLegalHold(tenant.tenantId, messageId, chatId);
        return json({ messageId, chatId, isUnderLegalHold: isHeld });
      }
      
      default:
        return json({ error: `Unknown action: ${action}` }, { status: 400 });
    }
  } catch (err) {
    return handleError(err);
  }
}

/**
 * GET /api/admin/compliance/policy?chatId=xxx
 * 
 * Get retention policy for a chat.
 */
export async function GET(req: Request) {
  try {
    const auth = await extractAuthContext(req);
    const tenant = extractTenantContext(auth);
    
    if (!isAdmin(auth)) {
      return json({ error: 'Admin access required' }, { status: 403 });
    }
    
    const url = new URL(req.url);
    const chatId = url.searchParams.get('chatId');
    
    if (!chatId) {
      return json({ error: 'chatId is required' }, { status: 400 });
    }
    
    const retentionService = getRetentionService();
    const policy = await retentionService.getPolicyForChat(tenant.tenantId, chatId);
    
    return json({ chatId, policy });
  } catch (err) {
    return handleError(err);
  }
}
