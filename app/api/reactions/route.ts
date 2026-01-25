// Reactions API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getReactionService } from '@/lib/domain/reactions/service';

const AddReactionSchema = z.object({
  messageId: z.string(),
  chatId: z.string(),
  userId: z.string(),
  emoji: z.string(),
});

const RemoveReactionSchema = z.object({
  messageId: z.string(),
  userId: z.string(),
  emoji: z.string(),
});

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const data = AddReactionSchema.parse(body);
    
    const service = getReactionService();
    const reaction = await service.addReaction(
      data.messageId,
      data.chatId,
      data.userId,
      data.emoji
    );
    
    return NextResponse.json({ success: true, reaction });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Add reaction error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function DELETE(request: NextRequest) {
  try {
    const body = await request.json();
    const data = RemoveReactionSchema.parse(body);
    
    const service = getReactionService();
    const success = await service.removeReaction(
      data.messageId,
      data.userId,
      data.emoji
    );
    
    return NextResponse.json({ success });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Remove reaction error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const messageId = searchParams.get('messageId');
    
    if (!messageId) {
      return NextResponse.json(
        { error: 'messageId is required' },
        { status: 400 }
      );
    }
    
    const service = getReactionService();
    const reactions = await service.getMessageReactions(messageId);
    
    return NextResponse.json({ reactions });
  } catch (error) {
    console.error('Get reactions error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
