// In-memory audit store for development and testing.
// Production deployments should use a dedicated audit database (append-only, immutable).

import { randomUUID } from 'crypto';
import type {
  AuditStore,
  AuditEntry,
  AuditQueryParams,
  AuditQueryResult,
  AuditResourceType,
} from '@/lib/platform/audit/types';

export class MemoryAuditStore implements AuditStore {
  private entries: AuditEntry[] = [];
  private maxEntries: number;
  
  constructor(maxEntries = 10000) {
    this.maxEntries = maxEntries;
  }
  
  async log(entry: Omit<AuditEntry, 'id' | 'timestamp'>): Promise<string> {
    const id = randomUUID();
    const fullEntry: AuditEntry = {
      ...entry,
      id,
      timestamp: new Date().toISOString(),
    };
    
    this.entries.push(fullEntry);
    
    // Enforce max entries (FIFO eviction)
    if (this.entries.length > this.maxEntries) {
      this.entries.shift();
    }
    
    // In development, log to console
    if (process.env.NODE_ENV === 'development') {
      console.log('[AUDIT]', fullEntry.action, fullEntry.resource.type, fullEntry.resource.id);
    }
    
    return id;
  }
  
  async query(params: AuditQueryParams): Promise<AuditQueryResult> {
    let filtered = this.entries.filter(e => e.tenantId === params.tenantId);
    
    if (params.startDate) {
      const start = params.startDate.toISOString();
      filtered = filtered.filter(e => e.timestamp >= start);
    }
    
    if (params.endDate) {
      const end = params.endDate.toISOString();
      filtered = filtered.filter(e => e.timestamp <= end);
    }
    
    if (params.actorUserId) {
      filtered = filtered.filter(e => e.actor.userId === params.actorUserId);
    }
    
    if (params.actions && params.actions.length > 0) {
      filtered = filtered.filter(e => params.actions!.includes(e.action));
    }
    
    if (params.resourceType) {
      filtered = filtered.filter(e => e.resource.type === params.resourceType);
    }
    
    if (params.resourceId) {
      filtered = filtered.filter(e => e.resource.id === params.resourceId);
    }
    
    if (params.outcome) {
      filtered = filtered.filter(e => e.outcome === params.outcome);
    }
    
    // Sort by timestamp descending (newest first)
    filtered.sort((a, b) => b.timestamp.localeCompare(a.timestamp));
    
    // Handle cursor-based pagination
    let startIndex = 0;
    if (params.cursor) {
      const cursorIndex = filtered.findIndex(e => e.id === params.cursor);
      if (cursorIndex >= 0) {
        startIndex = cursorIndex + 1;
      }
    }
    
    const limit = params.limit ?? 100;
    const page = filtered.slice(startIndex, startIndex + limit);
    const hasMore = startIndex + limit < filtered.length;
    
    return {
      entries: page,
      nextCursor: hasMore ? page[page.length - 1]?.id : undefined,
      totalCount: filtered.length,
    };
  }
  
  async getForResource(
    resourceType: AuditResourceType,
    resourceId: string,
    limit = 50
  ): Promise<AuditEntry[]> {
    return this.entries
      .filter(e => e.resource.type === resourceType && e.resource.id === resourceId)
      .sort((a, b) => b.timestamp.localeCompare(a.timestamp))
      .slice(0, limit);
  }
  
  // Testing helper
  clear(): void {
    this.entries = [];
  }
  
  // Get all entries (for debugging)
  getAll(): AuditEntry[] {
    return [...this.entries];
  }
}

// Singleton instance
let auditStore: AuditStore | null = null;

export function getAuditStore(): AuditStore {
  if (!auditStore) {
    auditStore = new MemoryAuditStore();
  }
  return auditStore;
}
