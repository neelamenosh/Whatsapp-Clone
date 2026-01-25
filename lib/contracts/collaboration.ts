import { z } from 'zod';

// ============================================================================
// COLLABORATIVE DOCUMENT
// ============================================================================

export const DocumentPermissionSchema = z.enum(['view', 'comment', 'edit']);
export type DocumentPermission = z.infer<typeof DocumentPermissionSchema>;

export const CollaborativeDocumentSchema = z.object({
  id: z.string().min(1),
  chatId: z.string().min(1),
  title: z.string().min(1).max(200),
  content: z.string(), // Markdown or rich text
  
  // Permissions
  creatorId: z.string().min(1),
  defaultPermission: DocumentPermissionSchema,
  permissions: z.record(z.string(), DocumentPermissionSchema), // userId -> permission
  
  // Version control
  version: z.number().int().min(1),
  lastEditedBy: z.string().optional(),
  lastEditedAt: z.string().datetime().optional(),
  
  // Collaboration state
  activeEditors: z.array(z.string()), // Currently editing
  
  createdAt: z.string().datetime(),
  updatedAt: z.string().datetime(),
});

export type CollaborativeDocument = z.infer<typeof CollaborativeDocumentSchema>;

export const DocumentChangeSchema = z.object({
  documentId: z.string().min(1),
  userId: z.string().min(1),
  changeType: z.enum(['insert', 'delete', 'replace', 'format']),
  position: z.number().int().min(0),
  length: z.number().int().min(0).optional(),
  content: z.string().optional(),
  timestamp: z.string().datetime(),
  version: z.number().int().min(1),
});

export type DocumentChange = z.infer<typeof DocumentChangeSchema>;

export const CreateDocumentRequestSchema = z.object({
  chatId: z.string().min(1),
  title: z.string().min(1).max(200),
  content: z.string().optional(),
  defaultPermission: DocumentPermissionSchema.optional(),
});

export type CreateDocumentRequest = z.infer<typeof CreateDocumentRequestSchema>;

// ============================================================================
// COLLABORATIVE WHITEBOARD
// ============================================================================

export const WhiteboardToolSchema = z.enum([
  'pen',
  'highlighter',
  'eraser',
  'text',
  'shape',
  'arrow',
  'sticky_note',
  'image',
  'select',
]);
export type WhiteboardTool = z.infer<typeof WhiteboardToolSchema>;

export const WhiteboardShapeSchema = z.enum([
  'rectangle',
  'ellipse',
  'triangle',
  'line',
  'arrow',
  'star',
]);
export type WhiteboardShape = z.infer<typeof WhiteboardShapeSchema>;

export const WhiteboardElementSchema = z.object({
  id: z.string().min(1),
  type: z.enum(['path', 'text', 'shape', 'image', 'sticky_note']),
  
  // Position and dimensions
  x: z.number(),
  y: z.number(),
  width: z.number().optional(),
  height: z.number().optional(),
  rotation: z.number().optional(),
  
  // Path data (for pen/highlighter)
  path: z.string().optional(),
  
  // Style
  strokeColor: z.string().optional(),
  fillColor: z.string().optional(),
  strokeWidth: z.number().optional(),
  opacity: z.number().min(0).max(1).optional(),
  
  // Text content
  text: z.string().optional(),
  fontSize: z.number().optional(),
  fontFamily: z.string().optional(),
  
  // Image
  imageUrl: z.string().url().optional(),
  
  // Shape type
  shape: WhiteboardShapeSchema.optional(),
  
  // Metadata
  createdBy: z.string().min(1),
  createdAt: z.string().datetime(),
  updatedAt: z.string().datetime(),
  lockedBy: z.string().optional(), // For collaborative editing
});

export type WhiteboardElement = z.infer<typeof WhiteboardElementSchema>;

export const CollaborativeWhiteboardSchema = z.object({
  id: z.string().min(1),
  chatId: z.string().min(1),
  title: z.string().min(1).max(200),
  
  // Canvas settings
  width: z.number().int().positive(),
  height: z.number().int().positive(),
  backgroundColor: z.string(),
  gridEnabled: z.boolean(),
  
  // Elements
  elements: z.array(WhiteboardElementSchema),
  
  // Permissions
  creatorId: z.string().min(1),
  defaultPermission: DocumentPermissionSchema,
  permissions: z.record(z.string(), DocumentPermissionSchema),
  
  // Collaboration state
  activeUsers: z.array(z.object({
    userId: z.string(),
    cursorX: z.number(),
    cursorY: z.number(),
    color: z.string(),
  })),
  
  // Version
  version: z.number().int().min(1),
  
  createdAt: z.string().datetime(),
  updatedAt: z.string().datetime(),
});

export type CollaborativeWhiteboard = z.infer<typeof CollaborativeWhiteboardSchema>;

export const WhiteboardActionSchema = z.object({
  whiteboardId: z.string().min(1),
  userId: z.string().min(1),
  action: z.enum(['add', 'update', 'delete', 'move', 'lock', 'unlock']),
  elementId: z.string().optional(),
  element: WhiteboardElementSchema.optional(),
  timestamp: z.string().datetime(),
  version: z.number().int().min(1),
});

export type WhiteboardAction = z.infer<typeof WhiteboardActionSchema>;

export const CreateWhiteboardRequestSchema = z.object({
  chatId: z.string().min(1),
  title: z.string().min(1).max(200),
  width: z.number().int().positive().optional().default(1920),
  height: z.number().int().positive().optional().default(1080),
  backgroundColor: z.string().optional().default('#ffffff'),
});

export type CreateWhiteboardRequest = z.infer<typeof CreateWhiteboardRequestSchema>;
