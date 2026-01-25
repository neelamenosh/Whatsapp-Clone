// Audit log types and interface for compliance and security tracking.
// All significant user and system actions are logged for audit trail.

import type { AuthContext } from '@/lib/platform/auth';

export interface AuditEntry {
  id: string;
  timestamp: string;
  tenantId: string;
  
  // Who performed the action
  actor: {
    userId: string;
    displayName?: string;
    ipAddress?: string;
    userAgent?: string;
    sessionId?: string;
  };
  
  // What action was performed
  action: AuditAction;
  
  // Resource affected
  resource: {
    type: AuditResourceType;
    id: string;
    metadata?: Record<string, unknown>;
  };
  
  // Additional context
  context?: {
    chatId?: string;
    messageId?: string;
    reason?: string;
    details?: Record<string, unknown>;
  };
  
  // Outcome
  outcome: 'success' | 'failure' | 'denied';
  errorMessage?: string;
}

export type AuditAction =
  // Authentication
  | 'auth.login'
  | 'auth.logout'
  | 'auth.token_refresh'
  | 'auth.failed_login'
  
  // Messaging
  | 'message.send'
  | 'message.delete'
  | 'message.edit'
  | 'message.read'
  | 'message.forward'
  | 'message.export'
  
  // Chat management
  | 'chat.create'
  | 'chat.delete'
  | 'chat.archive'
  | 'chat.add_member'
  | 'chat.remove_member'
  | 'chat.change_admin'
  
  // Media
  | 'media.upload'
  | 'media.download'
  | 'media.delete'
  
  // User management
  | 'user.create'
  | 'user.update'
  | 'user.delete'
  | 'user.block'
  | 'user.unblock'
  
  // Admin operations
  | 'admin.data_export'
  | 'admin.legal_hold'
  | 'admin.retention_override'
  | 'admin.user_impersonate';

export type AuditResourceType =
  | 'message'
  | 'chat'
  | 'user'
  | 'media'
  | 'tenant'
  | 'system';

// Audit log store interface
export interface AuditStore {
  /**
   * Log an audit entry.
   */
  log(entry: Omit<AuditEntry, 'id' | 'timestamp'>): Promise<string>;
  
  /**
   * Query audit logs with filters.
   */
  query(params: AuditQueryParams): Promise<AuditQueryResult>;
  
  /**
   * Get audit entries for a specific resource.
   */
  getForResource(
    resourceType: AuditResourceType,
    resourceId: string,
    limit?: number
  ): Promise<AuditEntry[]>;
}

export interface AuditQueryParams {
  tenantId: string;
  startDate?: Date;
  endDate?: Date;
  actorUserId?: string;
  actions?: AuditAction[];
  resourceType?: AuditResourceType;
  resourceId?: string;
  outcome?: 'success' | 'failure' | 'denied';
  limit?: number;
  cursor?: string;
}

export interface AuditQueryResult {
  entries: AuditEntry[];
  nextCursor?: string;
  totalCount?: number;
}

/**
 * Create an audit entry from an auth context.
 */
export function createAuditEntry(
  auth: AuthContext,
  action: AuditAction,
  resource: AuditEntry['resource'],
  outcome: AuditEntry['outcome'] = 'success',
  context?: AuditEntry['context']
): Omit<AuditEntry, 'id' | 'timestamp'> {
  return {
    tenantId: auth.user?.tenantId ?? 'default',
    actor: {
      userId: auth.user?.sub ?? 'anonymous', // 'sub' is the standard JWT user ID claim
      displayName: auth.user?.name,           // 'name' is the display name claim
      // IP and user agent would come from request headers
    },
    action,
    resource,
    outcome,
    context,
  };
}
