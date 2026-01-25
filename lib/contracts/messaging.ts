import { z } from 'zod';

// v1 messaging contracts. Keep these stable and versioned.

export const MessageStatusSchema = z.enum(['sending', 'sent', 'delivered', 'read']);
export type MessageStatus = z.infer<typeof MessageStatusSchema>;

export const MessageTypeSchema = z.enum(['text', 'image', 'voice', 'location', 'document']);
export type MessageType = z.infer<typeof MessageTypeSchema>;

// Canonical server-facing representation. Timestamps are ISO strings.
export const MessageDTOSchema = z.object({
  id: z.string().min(1),
  chatId: z.string().min(1),
  senderId: z.string().min(1),
  content: z.string().min(1),
  timestamp: z.string().datetime(),
  status: MessageStatusSchema,
  type: MessageTypeSchema,
  expiresAt: z.string().datetime().optional(),
  // Multi-tenant support
  tenantId: z.string().min(1).optional(), // defaults to 'default'
});

export type MessageDTO = z.infer<typeof MessageDTOSchema>;

// Client → server send message request.
// Note: chatId is typically in the route; kept optional here to support both patterns.
export const SendMessageRequestSchema = z.object({
  id: z.string().min(1).optional(),
  chatId: z.string().min(1).optional(),
  senderId: z.string().min(1),
  content: z.string().min(1),
  timestamp: z.string().datetime().optional(),
  status: MessageStatusSchema.optional(),
  type: MessageTypeSchema.optional(),
  expiresAt: z.string().datetime().optional(),
  // Idempotency key for safe retries. If omitted, server generates one from id.
  idempotencyKey: z.string().min(1).optional(),
  // Multi-tenant: provided by server from auth context
  tenantId: z.string().min(1).optional(),
});

export type SendMessageRequest = z.infer<typeof SendMessageRequestSchema>;

export const MessageCreatedEventSchema = z.object({
  eventId: z.string().min(1),
  eventType: z.literal('MessageCreated'),
  occurredAt: z.string().datetime(),
  schemaVersion: z.literal(1),
  payload: MessageDTOSchema,
});

export type MessageCreatedEvent = z.infer<typeof MessageCreatedEventSchema>;
