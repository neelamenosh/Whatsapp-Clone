import { NextResponse } from 'next/server';
import { extractAuthContext, AuthError } from '@/lib/platform/auth';
import { getSearchService } from '@/lib/platform/search/service';
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
  const message = statusCode === 400 ? 'Invalid query' : 'Server error';
  return json({ error: message }, { status: statusCode });
}

/**
 * GET /api/search?q=query&chatIds=id1,id2&limit=20
 * 
 * Search messages across chats with full-text search.
 */
export async function GET(req: Request) {
  try {
    // Extract auth context
    const auth = await extractAuthContext(req);
    const tenant = extractTenantContext(auth);
    
    // Parse query parameters
    const url = new URL(req.url);
    const query = url.searchParams.get('q') ?? '';
    const chatIds = url.searchParams.get('chatIds')?.split(',').filter(Boolean);
    const senderIds = url.searchParams.get('senderIds')?.split(',').filter(Boolean);
    const startDate = url.searchParams.get('startDate');
    const endDate = url.searchParams.get('endDate');
    const limit = parseInt(url.searchParams.get('limit') ?? '20', 10);
    const offset = parseInt(url.searchParams.get('offset') ?? '0', 10);
    const sortBy = url.searchParams.get('sortBy') as 'relevance' | 'timestamp' | null;
    
    if (!query.trim()) {
      return json({ error: 'Search query is required' }, { status: 400 });
    }
    
    const searchService = getSearchService();
    
    const result = await searchService.search({
      tenantId: tenant.tenantId,
      userId: auth.user?.sub ?? 'anonymous',
      query,
      filters: {
        chatIds: chatIds?.length ? chatIds : undefined,
        senderIds: senderIds?.length ? senderIds : undefined,
        startDate: startDate ? new Date(startDate) : undefined,
        endDate: endDate ? new Date(endDate) : undefined,
      },
      limit: Math.min(limit, 100), // Cap at 100
      offset,
      sortBy: sortBy ?? 'relevance',
    });
    
    return json({
      results: result.results,
      totalCount: result.totalCount,
      query: result.query,
      executionTimeMs: result.executionTimeMs,
    });
  } catch (err) {
    return handleError(err);
  }
}

/**
 * GET /api/search/suggestions?prefix=hel&limit=10
 * 
 * Get search autocomplete suggestions.
 */
export async function POST(req: Request) {
  try {
    const auth = await extractAuthContext(req);
    const tenant = extractTenantContext(auth);
    
    const body = await req.json();
    const { prefix, limit = 10 } = body;
    
    if (!prefix || typeof prefix !== 'string') {
      return json({ error: 'Prefix is required' }, { status: 400 });
    }
    
    const searchService = getSearchService();
    const suggestions = await searchService.getSuggestions(
      tenant.tenantId,
      prefix,
      Math.min(limit, 20)
    );
    
    return json({ suggestions });
  } catch (err) {
    return handleError(err);
  }
}
