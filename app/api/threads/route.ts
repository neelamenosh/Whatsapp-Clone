// Threads API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getThreadService } from '@/lib/domain/threads/service';

const CreateThreadSchema = z.object({
  parentMessageId: z.string(),
  chatId: z.string(),
  creatorId: z.string(),
});

const AddReplySchema = z.object({
  threadId: z.string(),
  messageId: z.string(),
  userId: z.string(),
});

const QuoteSchema = z.object({
  quotingMessageId: z.string(),
  originalMessageId: z.string(),
  originalSenderId: z.string(),
  originalContent: z.string(),
  originalTimestamp: z.string(),
});

export async function POST(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const action = searchParams.get('action');
    const body = await request.json();
    
    const service = getThreadService();
    
    switch (action) {
      case 'create': {
        const data = CreateThreadSchema.parse(body);
        const thread = await service.createThread(
          data.parentMessageId,
          data.chatId,
          data.creatorId
        );
        return NextResponse.json({ success: true, thread });
      }
      
      case 'reply': {
        const data = AddReplySchema.parse(body);
        const thread = await service.addReplyToThread(
          data.threadId,
          data.messageId,
          data.userId
        );
        if (!thread) {
          return NextResponse.json({ error: 'Thread not found' }, { status: 404 });
        }
        return NextResponse.json({ success: true, thread });
      }
      
      case 'quote': {
        const data = QuoteSchema.parse(body);
        const quote = await service.createQuote(
          data.quotingMessageId,
          data.originalMessageId,
          data.originalSenderId,
          data.originalContent,
          data.originalTimestamp
        );
        return NextResponse.json({ success: true, quote });
      }
      
      default: {
        // Default: get or create thread
        const data = CreateThreadSchema.parse(body);
        const thread = await service.getOrCreateThread(
          data.parentMessageId,
          data.chatId,
          data.creatorId
        );
        return NextResponse.json({ success: true, thread });
      }
    }
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Thread error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const threadId = searchParams.get('threadId');
    const parentMessageId = searchParams.get('parentMessageId');
    const quotedMessageId = searchParams.get('quotedMessageId');
    
    const service = getThreadService();
    
    // Get thread by ID
    if (threadId) {
      const thread = await service.getThread(threadId);
      if (!thread) {
        return NextResponse.json({ error: 'Thread not found' }, { status: 404 });
      }
      return NextResponse.json({ thread });
    }
    
    // Get thread by parent message
    if (parentMessageId) {
      const thread = await service.getThreadByParent(parentMessageId);
      return NextResponse.json({ thread });
    }
    
    // Get quoted message
    if (quotedMessageId) {
      const quote = await service.getQuote(quotedMessageId);
      return NextResponse.json({ quote });
    }
    
    return NextResponse.json(
      { error: 'threadId, parentMessageId, or quotedMessageId required' },
      { status: 400 }
    );
  } catch (error) {
    console.error('Get thread error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
