// GIFs API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getStickerGifService } from '@/lib/domain/stickers/service';

const SearchGifsSchema = z.object({
  query: z.string().min(1),
  limit: z.number().min(1).max(50).optional(),
  offset: z.number().min(0).optional(),
  rating: z.enum(['g', 'pg', 'pg-13', 'r']).optional(),
});

const SendGifSchema = z.object({
  userId: z.string(),
  chatId: z.string(),
  gifId: z.string(),
});

export async function POST(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const action = searchParams.get('action');
    const body = await request.json();
    
    const service = getStickerGifService();
    
    if (action === 'send') {
      const data = SendGifSchema.parse(body);
      const gif = await service.sendGif(data.userId, data.chatId, data.gifId);
      
      if (!gif) {
        return NextResponse.json(
          { error: 'GIF not found' },
          { status: 404 }
        );
      }
      
      return NextResponse.json({ success: true, gif });
    }
    
    // Default: search
    const data = SearchGifsSchema.parse(body);
    const response = await service.searchGifs({
      query: data.query,
      limit: data.limit,
      offset: data.offset,
      rating: data.rating,
    });
    
    return NextResponse.json(response);
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('GIF error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const query = searchParams.get('query');
    const trending = searchParams.get('trending');
    const limit = parseInt(searchParams.get('limit') ?? '20');
    
    const service = getStickerGifService();
    
    // Get trending GIFs
    if (trending === 'true') {
      const response = await service.getTrendingGifs(limit);
      return NextResponse.json(response);
    }
    
    // Search GIFs
    if (query) {
      const response = await service.searchGifs({
        query,
        limit,
        offset: parseInt(searchParams.get('offset') ?? '0'),
        rating: searchParams.get('rating') as 'g' | 'pg' | 'pg-13' | 'r' | undefined,
      });
      return NextResponse.json(response);
    }
    
    return NextResponse.json(
      { error: 'query or trending parameter required' },
      { status: 400 }
    );
  } catch (error) {
    console.error('Get GIFs error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
