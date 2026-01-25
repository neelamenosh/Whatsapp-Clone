// Presence API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getPresenceService } from '@/lib/domain/presence/service';

const UpdatePresenceSchema = z.object({
  userId: z.string(),
  status: z.enum(['online', 'offline', 'away', 'busy']),
  customStatus: z.string().optional(),
});

const TypingSchema = z.object({
  userId: z.string(),
  chatId: z.string(),
});

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const data = UpdatePresenceSchema.parse(body);
    
    const service = getPresenceService();
    await service.updatePresence(data.userId, data.status, data.customStatus);
    
    return NextResponse.json({ success: true });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Update presence error:', error);
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
    const userIds = searchParams.get('userIds');
    
    const service = getPresenceService();
    
    if (userId) {
      const presence = await service.getUserPresence(userId);
      return NextResponse.json({ presence });
    }
    
    if (userIds) {
      const ids = userIds.split(',');
      const presenceList = await Promise.all(
        ids.map(id => service.getUserPresence(id))
      );
      return NextResponse.json({
        presences: presenceList.filter(Boolean),
      });
    }
    
    return NextResponse.json(
      { error: 'userId or userIds parameter required' },
      { status: 400 }
    );
  } catch (error) {
    console.error('Get presence error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
