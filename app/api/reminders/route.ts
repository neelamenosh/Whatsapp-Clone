// Reminders API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getReminderService } from '@/lib/domain/reminders/service';

const CreateReminderSchema = z.object({
  userId: z.string(),
  messageId: z.string(),
  chatId: z.string(),
  remindAt: z.string(),
  note: z.string().optional(),
});

const SnoozeSchema = z.object({
  reminderId: z.string(),
  durationMinutes: z.number().min(1).max(10080), // Max 1 week
});

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const data = CreateReminderSchema.parse(body);
    
    const service = getReminderService();
    const reminder = await service.createReminder(
      data.userId,
      data.messageId,
      data.chatId,
      data.remindAt,
      data.note
    );
    
    return NextResponse.json({ success: true, reminder });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Create reminder error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const userId = searchParams.get('userId');
    const reminderId = searchParams.get('reminderId');
    
    const service = getReminderService();
    
    if (reminderId) {
      const reminder = await service.getReminder(reminderId);
      if (!reminder) {
        return NextResponse.json(
          { error: 'Reminder not found' },
          { status: 404 }
        );
      }
      return NextResponse.json({ reminder });
    }
    
    if (userId) {
      const reminders = await service.getUserReminders(userId);
      return NextResponse.json({ reminders });
    }
    
    return NextResponse.json(
      { error: 'userId or reminderId required' },
      { status: 400 }
    );
  } catch (error) {
    console.error('Get reminder error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function PATCH(request: NextRequest) {
  try {
    const body = await request.json();
    const data = SnoozeSchema.parse(body);
    
    const service = getReminderService();
    const reminder = await service.snoozeReminder(
      data.reminderId,
      data.durationMinutes
    );
    
    if (!reminder) {
      return NextResponse.json(
        { error: 'Reminder not found or already triggered' },
        { status: 404 }
      );
    }
    
    return NextResponse.json({ success: true, reminder });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Snooze reminder error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function DELETE(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const reminderId = searchParams.get('reminderId');
    const userId = searchParams.get('userId');
    
    if (!reminderId || !userId) {
      return NextResponse.json(
        { error: 'reminderId and userId required' },
        { status: 400 }
      );
    }
    
    const service = getReminderService();
    const success = await service.cancelReminder(reminderId, userId);
    
    return NextResponse.json({ success });
  } catch (error) {
    console.error('Cancel reminder error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
