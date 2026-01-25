// Read Receipts API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getReadReceiptService } from '@/lib/domain/receipts/service';

const MarkReadSchema = z.object({
  messageId: z.string(),
  userId: z.string(),
  chatId: z.string(),
});

const MarkBatchReadSchema = z.object({
  messageIds: z.array(z.string()).min(1),
  userId: z.string(),
  chatId: z.string(),
});

export async function POST(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const batch = searchParams.get('batch');
    const body = await request.json();
    
    const service = getReadReceiptService();
    
    if (batch === 'true') {
      const data = MarkBatchReadSchema.parse(body);
      await service.markMessagesRead(data.messageIds, data.userId, data.chatId);
      return NextResponse.json({ success: true });
    }
    
    const data = MarkReadSchema.parse(body);
    await service.markMessageRead(data.messageId, data.userId, data.chatId);
    
    return NextResponse.json({ success: true });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Mark read error:', error);
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
        { error: 'messageId required' },
        { status: 400 }
      );
    }
    
    const service = getReadReceiptService();
    const receipts = await service.getMessageReceipts(messageId);
    
    return NextResponse.json({ receipts });
  } catch (error) {
    console.error('Get receipts error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
