// Reminders Service
// Handles message reminders and scheduled notifications

import type { Reminder } from '@/lib/contracts/engagement';
import { getEventBus, type EventBus } from '@/lib/domain/events/bus';

// ============================================================================
// EVENTS
// ============================================================================

export const REMINDER_EVENTS = {
  REMINDER_CREATED: 'reminder:created',
  REMINDER_TRIGGERED: 'reminder:triggered',
  REMINDER_CANCELLED: 'reminder:cancelled',
  REMINDER_SNOOZED: 'reminder:snoozed',
} as const;

export interface ReminderCreatedEvent {
  type: typeof REMINDER_EVENTS.REMINDER_CREATED;
  payload: Reminder;
}

export interface ReminderTriggeredEvent {
  type: typeof REMINDER_EVENTS.REMINDER_TRIGGERED;
  payload: {
    reminderId: string;
    userId: string;
    messageId: string;
    chatId: string;
  };
}

export interface ReminderCancelledEvent {
  type: typeof REMINDER_EVENTS.REMINDER_CANCELLED;
  payload: {
    reminderId: string;
    userId: string;
  };
}

export interface ReminderSnoozedEvent {
  type: typeof REMINDER_EVENTS.REMINDER_SNOOZED;
  payload: {
    reminderId: string;
    newTime: string;
    snoozeDuration: number;
  };
}

// ============================================================================
// STORE INTERFACE
// ============================================================================

export interface ReminderStore {
  create(reminder: Omit<Reminder, 'id'>): Promise<Reminder>;
  get(reminderId: string): Promise<Reminder | null>;
  getByUser(userId: string): Promise<Reminder[]>;
  getByMessage(messageId: string): Promise<Reminder[]>;
  getPending(beforeTime: string): Promise<Reminder[]>;
  update(reminderId: string, updates: Partial<Reminder>): Promise<Reminder | null>;
  delete(reminderId: string): Promise<boolean>;
}

// ============================================================================
// IN-MEMORY STORE
// ============================================================================

export class MemoryReminderStore implements ReminderStore {
  private reminders = new Map<string, Reminder>();
  
  async create(data: Omit<Reminder, 'id'>): Promise<Reminder> {
    const id = `reminder-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const reminder: Reminder = {
      id,
      ...data,
    };
    
    this.reminders.set(id, reminder);
    return reminder;
  }
  
  async get(reminderId: string): Promise<Reminder | null> {
    return this.reminders.get(reminderId) ?? null;
  }
  
  async getByUser(userId: string): Promise<Reminder[]> {
    return Array.from(this.reminders.values())
      .filter(r => r.userId === userId && r.status === 'pending')
      .sort((a, b) => new Date(a.remindAt).getTime() - new Date(b.remindAt).getTime());
  }
  
  async getByMessage(messageId: string): Promise<Reminder[]> {
    return Array.from(this.reminders.values())
      .filter(r => r.messageId === messageId);
  }
  
  async getPending(beforeTime: string): Promise<Reminder[]> {
    const cutoff = new Date(beforeTime).getTime();
    return Array.from(this.reminders.values())
      .filter(r => r.status === 'pending' && new Date(r.remindAt).getTime() <= cutoff);
  }
  
  async update(reminderId: string, updates: Partial<Reminder>): Promise<Reminder | null> {
    const existing = this.reminders.get(reminderId);
    if (!existing) return null;
    
    const updated: Reminder = { ...existing, ...updates, id: reminderId };
    this.reminders.set(reminderId, updated);
    return updated;
  }
  
  async delete(reminderId: string): Promise<boolean> {
    return this.reminders.delete(reminderId);
  }
}

// ============================================================================
// SERVICE
// ============================================================================

export interface ReminderService {
  createReminder(
    userId: string,
    messageId: string,
    chatId: string,
    remindAt: string,
    note?: string
  ): Promise<Reminder>;
  getReminder(reminderId: string): Promise<Reminder | null>;
  getUserReminders(userId: string): Promise<Reminder[]>;
  snoozeReminder(reminderId: string, durationMinutes: number): Promise<Reminder | null>;
  cancelReminder(reminderId: string, userId: string): Promise<boolean>;
  triggerReminder(reminderId: string): Promise<boolean>;
  processDueReminders(): Promise<number>;
}

export class ReminderServiceImpl implements ReminderService {
  private store: ReminderStore;
  private eventBus: EventBus;
  private checkInterval: ReturnType<typeof setInterval> | null = null;
  
  constructor(store: ReminderStore, eventBus: EventBus) {
    this.store = store;
    this.eventBus = eventBus;
  }
  
  async createReminder(
    userId: string,
    messageId: string,
    chatId: string,
    remindAt: string,
    note?: string
  ): Promise<Reminder> {
    const reminder = await this.store.create({
      userId,
      messageId,
      chatId,
      remindAt,
      note,
      status: 'pending',
      createdAt: new Date().toISOString(),
    });
    
    this.eventBus.publish({
      type: REMINDER_EVENTS.REMINDER_CREATED,
      payload: reminder,
    });
    
    return reminder;
  }
  
  async getReminder(reminderId: string): Promise<Reminder | null> {
    return this.store.get(reminderId);
  }
  
  async getUserReminders(userId: string): Promise<Reminder[]> {
    return this.store.getByUser(userId);
  }
  
  async snoozeReminder(reminderId: string, durationMinutes: number): Promise<Reminder | null> {
    const reminder = await this.store.get(reminderId);
    if (!reminder || reminder.status !== 'pending') return null;
    
    const newTime = new Date(Date.now() + durationMinutes * 60 * 1000).toISOString();
    
    const updated = await this.store.update(reminderId, {
      remindAt: newTime,
      snoozeCount: (reminder.snoozeCount ?? 0) + 1,
    });
    
    if (updated) {
      this.eventBus.publish({
        type: REMINDER_EVENTS.REMINDER_SNOOZED,
        payload: {
          reminderId,
          newTime,
          snoozeDuration: durationMinutes,
        },
      });
    }
    
    return updated;
  }
  
  async cancelReminder(reminderId: string, userId: string): Promise<boolean> {
    const reminder = await this.store.get(reminderId);
    if (!reminder || reminder.userId !== userId) return false;
    
    await this.store.update(reminderId, { status: 'cancelled' });
    
    this.eventBus.publish({
      type: REMINDER_EVENTS.REMINDER_CANCELLED,
      payload: { reminderId, userId },
    });
    
    return true;
  }
  
  async triggerReminder(reminderId: string): Promise<boolean> {
    const reminder = await this.store.get(reminderId);
    if (!reminder || reminder.status !== 'pending') return false;
    
    await this.store.update(reminderId, {
      status: 'triggered',
      triggeredAt: new Date().toISOString(),
    });
    
    this.eventBus.publish({
      type: REMINDER_EVENTS.REMINDER_TRIGGERED,
      payload: {
        reminderId,
        userId: reminder.userId,
        messageId: reminder.messageId,
        chatId: reminder.chatId,
      },
    });
    
    return true;
  }
  
  async processDueReminders(): Promise<number> {
    const now = new Date().toISOString();
    const dueReminders = await this.store.getPending(now);
    
    let triggered = 0;
    for (const reminder of dueReminders) {
      const success = await this.triggerReminder(reminder.id);
      if (success) triggered++;
    }
    
    return triggered;
  }
  
  startBackgroundProcessor(intervalMs: number = 60000): void {
    if (this.checkInterval) return;
    
    this.checkInterval = setInterval(() => {
      this.processDueReminders().catch(console.error);
    }, intervalMs);
  }
  
  stopBackgroundProcessor(): void {
    if (this.checkInterval) {
      clearInterval(this.checkInterval);
      this.checkInterval = null;
    }
  }
}

// ============================================================================
// SINGLETON
// ============================================================================

let reminderService: ReminderService | null = null;

export function getReminderService(): ReminderService {
  if (!reminderService) {
    reminderService = new ReminderServiceImpl(new MemoryReminderStore(), getEventBus());
  }
  return reminderService;
}
