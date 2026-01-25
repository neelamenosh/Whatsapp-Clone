// Typing Indicator API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getPresenceService } from '@/lib/domain/presence/service';

const TypingSchema = z.object({
  userId: z.string(),
  chatId: z.string(),
});

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const data = TypingSchema.parse(body);
    
    const service = getPresenceService();
    await service.startTyping(data.userId, data.chatId);
    
    return NextResponse.json({ success: true });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Start typing error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function DELETE(request: NextRequest) {
  try {
    const body = await request.json();
    const data = TypingSchema.parse(body);
    
    const service = getPresenceService();
    await service.stopTyping(data.userId, data.chatId);
    
    return NextResponse.json({ success: true });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Stop typing error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const chatId = searchParams.get('chatId');
    
    if (!chatId) {
      return NextResponse.json(
        { error: 'chatId is required' },
        { status: 400 }
      );
    }
    
    const service = getPresenceService();
    const typingUsers = await service.getTypingUsers(chatId);
    
    return NextResponse.json({ typingUsers });
  } catch (error) {
    console.error('Get typing users error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
