// Retention policy service for compliance with data retention regulations.
// Handles automatic message deletion, legal holds, and data export.

import type { AuditStore } from '@/lib/platform/audit/types';

export interface RetentionPolicy {
  id: string;
  tenantId: string;
  name: string;
  
  // Retention settings
  retentionDays: number; // Days to keep messages (0 = delete immediately after read)
  
  // Scope - which messages this policy applies to
  scope: {
    chatTypes?: ('private' | 'group')[];
    chatIds?: string[];
    excludeChatIds?: string[];
  };
  
  // Legal hold overrides retention for specified entities
  isLegalHold?: boolean;
  
  // Policy metadata
  createdAt: string;
  updatedAt: string;
  createdBy: string;
}

export interface LegalHold {
  id: string;
  tenantId: string;
  name: string;
  description?: string;
  
  // Scope of the hold
  targetType: 'user' | 'chat' | 'all';
  targetIds: string[];
  
  // Date range to preserve
  startDate: string;
  endDate?: string; // null = ongoing
  
  // Metadata
  caseReference?: string;
  createdAt: string;
  createdBy: string;
  isActive: boolean;
}

export interface RetentionService {
  /**
   * Get applicable retention policy for a chat.
   */
  getPolicyForChat(tenantId: string, chatId: string): Promise<RetentionPolicy | null>;
  
  /**
   * Check if a message is under legal hold.
   */
  isUnderLegalHold(tenantId: string, messageId: string, chatId: string): Promise<boolean>;
  
  /**
   * Run retention cleanup for a tenant.
   * Deletes messages past their retention period (unless under legal hold).
   */
  runCleanup(tenantId: string): Promise<RetentionCleanupResult>;
  
  /**
   * Export data for a user (GDPR data subject request).
   */
  exportUserData(tenantId: string, userId: string): Promise<DataExport>;
  
  /**
   * Delete user data (GDPR right to erasure).
   * Returns what was deleted vs what was retained (legal holds).
   */
  deleteUserData(tenantId: string, userId: string): Promise<DeletionResult>;
}

export interface RetentionCleanupResult {
  tenantId: string;
  messagesDeleted: number;
  messagesRetained: number; // Due to legal holds
  errors: string[];
  completedAt: string;
}

export interface DataExport {
  userId: string;
  exportedAt: string;
  messages: ExportedMessage[];
  profile: Record<string, unknown>;
  media: ExportedMedia[];
}

export interface ExportedMessage {
  chatId: string;
  messageId: string;
  content: string;
  timestamp: string;
  direction: 'sent' | 'received';
}

export interface ExportedMedia {
  id: string;
  type: string;
  filename: string;
  size: number;
  uploadedAt: string;
}

export interface DeletionResult {
  userId: string;
  deletedAt: string;
  messagesDeleted: number;
  messagesRetained: number;
  retainedReasons: string[];
}

// Demo implementation for development
export class DemoRetentionService implements RetentionService {
  async getPolicyForChat(_tenantId: string, _chatId: string): Promise<RetentionPolicy | null> {
    // Default: no retention policy (keep forever)
    return null;
  }
  
  async isUnderLegalHold(_tenantId: string, _messageId: string, _chatId: string): Promise<boolean> {
    // No legal holds in demo mode
    return false;
  }
  
  async runCleanup(_tenantId: string): Promise<RetentionCleanupResult> {
    // No-op in demo mode
    return {
      tenantId: _tenantId,
      messagesDeleted: 0,
      messagesRetained: 0,
      errors: [],
      completedAt: new Date().toISOString(),
    };
  }
  
  async exportUserData(_tenantId: string, userId: string): Promise<DataExport> {
    return {
      userId,
      exportedAt: new Date().toISOString(),
      messages: [],
      profile: {},
      media: [],
    };
  }
  
  async deleteUserData(_tenantId: string, userId: string): Promise<DeletionResult> {
    return {
      userId,
      deletedAt: new Date().toISOString(),
      messagesDeleted: 0,
      messagesRetained: 0,
      retainedReasons: [],
    };
  }
}

// Singleton
let retentionService: RetentionService | null = null;

export function getRetentionService(): RetentionService {
  if (!retentionService) {
    retentionService = new DemoRetentionService();
  }
  return retentionService;
}
