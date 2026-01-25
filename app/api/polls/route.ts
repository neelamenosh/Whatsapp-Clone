// Polls API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getPollService } from '@/lib/domain/polls/service';

const CreatePollSchema = z.object({
  chatId: z.string(),
  creatorId: z.string(),
  question: z.string().min(1).max(500),
  options: z.array(z.string()).min(2).max(12),
  settings: z.object({
    allowMultipleVotes: z.boolean().optional(),
    isAnonymous: z.boolean().optional(),
    expiresAt: z.string().optional(),
  }).optional(),
});

const VoteSchema = z.object({
  pollId: z.string(),
  optionIds: z.array(z.string()).min(1),
  userId: z.string(),
});

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const data = CreatePollSchema.parse(body);
    
    const service = getPollService();
    const poll = await service.createPoll(
      data.chatId,
      data.creatorId,
      data.question,
      data.options,
      data.settings
    );
    
    return NextResponse.json({ success: true, poll });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Create poll error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function PUT(request: NextRequest) {
  try {
    const body = await request.json();
    const data = VoteSchema.parse(body);
    
    const service = getPollService();
    const poll = await service.vote(data.pollId, data.userId, data.optionIds);
    
    if (!poll) {
      return NextResponse.json(
        { error: 'Poll not found or voting not allowed' },
        { status: 404 }
      );
    }
    
    return NextResponse.json({ success: true, poll });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Vote error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const pollId = searchParams.get('pollId');
    const chatId = searchParams.get('chatId');
    
    const service = getPollService();
    
    if (pollId) {
      const poll = await service.getPoll(pollId);
      if (!poll) {
        return NextResponse.json(
          { error: 'Poll not found' },
          { status: 404 }
        );
      }
      return NextResponse.json({ poll });
    }
    
    if (chatId) {
      const polls = await service.getChatPolls(chatId);
      return NextResponse.json({ polls });
    }
    
    return NextResponse.json(
      { error: 'pollId or chatId required' },
      { status: 400 }
    );
  } catch (error) {
    console.error('Get poll error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function PATCH(request: NextRequest) {
  try {
    const body = await request.json();
    const { pollId, creatorId } = body;
    
    if (!pollId || !creatorId) {
      return NextResponse.json(
        { error: 'pollId and creatorId required' },
        { status: 400 }
      );
    }
    
    const service = getPollService();
    const poll = await service.closePoll(pollId, creatorId);
    
    if (!poll) {
      return NextResponse.json(
        { error: 'Poll not found or not authorized' },
        { status: 404 }
      );
    }
    
    return NextResponse.json({ success: true, poll });
  } catch (error) {
    console.error('Close poll error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
