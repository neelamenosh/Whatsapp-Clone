// AI Features API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getAIService } from '@/lib/domain/ai/service';

const ConversationStartersSchema = z.object({
  chatId: z.string(),
  userId: z.string(),
  relationshipContext: z.string().optional(),
  count: z.number().min(1).max(10).optional(),
});

const SmartRepliesSchema = z.object({
  messageId: z.string(),
  chatId: z.string(),
  messageContent: z.string(),
  userId: z.string(),
  context: z.array(z.object({
    senderId: z.string(),
    content: z.string(),
  })).optional(),
  count: z.number().min(1).max(5).optional(),
});

const ConversationSummarySchema = z.object({
  chatId: z.string(),
  userId: z.string(),
  messageRange: z.object({
    from: z.string().optional(),
    to: z.string().optional(),
    lastN: z.number().optional(),
  }).optional(),
  maxLength: z.enum(['brief', 'moderate', 'detailed']).optional(),
  includeActionItems: z.boolean().optional(),
  includeKeyTopics: z.boolean().optional(),
  language: z.string().optional(),
});

export async function POST(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const action = searchParams.get('action');
    const body = await request.json();
    
    const service = getAIService();
    
    switch (action) {
      case 'starters': {
        const data = ConversationStartersSchema.parse(body);
        const response = await service.getConversationStarters(data);
        return NextResponse.json(response);
      }
      
      case 'replies': {
        const data = SmartRepliesSchema.parse(body);
        const response = await service.getSmartReplies(data);
        return NextResponse.json(response);
      }
      
      case 'summary': {
        const data = ConversationSummarySchema.parse(body);
        // Note: In real implementation, fetch messages based on messageRange
        const messages = []; // Placeholder - fetch from message store
        const response = await service.getConversationSummary(data, messages);
        return NextResponse.json(response);
      }
      
      default:
        return NextResponse.json(
          { error: 'Invalid action. Use: starters, replies, or summary' },
          { status: 400 }
        );
    }
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('AI feature error:', error);
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
    
    if (!userId) {
      return NextResponse.json(
        { error: 'userId required' },
        { status: 400 }
      );
    }
    
    const service = getAIService();
    const preferences = await service.getUserPreferences(userId);
    
    return NextResponse.json({ preferences });
  } catch (error) {
    console.error('Get AI preferences error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function PATCH(request: NextRequest) {
  try {
    const body = await request.json();
    const { userId, ...updates } = body;
    
    if (!userId) {
      return NextResponse.json(
        { error: 'userId required' },
        { status: 400 }
      );
    }
    
    const service = getAIService();
    const preferences = await service.updateUserPreferences(userId, updates);
    
    return NextResponse.json({ success: true, preferences });
  } catch (error) {
    console.error('Update AI preferences error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
