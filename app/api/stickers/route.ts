// Stickers API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getStickerGifService } from '@/lib/domain/stickers/service';

const SendStickerSchema = z.object({
  userId: z.string(),
  chatId: z.string(),
  stickerId: z.string(),
});

const CreateCustomStickerSchema = z.object({
  userId: z.string(),
  imageUrl: z.string().url(),
  emoji: z.string(),
});

const FavoriteSchema = z.object({
  userId: z.string(),
  stickerId: z.string(),
});

export async function POST(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const action = searchParams.get('action');
    const body = await request.json();
    
    const service = getStickerGifService();
    
    switch (action) {
      case 'send': {
        const data = SendStickerSchema.parse(body);
        const sticker = await service.sendSticker(data.userId, data.chatId, data.stickerId);
        return NextResponse.json({ success: true, sticker });
      }
      
      case 'custom': {
        const data = CreateCustomStickerSchema.parse(body);
        const sticker = await service.createCustomSticker(
          data.userId,
          data.imageUrl,
          data.emoji
        );
        return NextResponse.json({ success: true, sticker });
      }
      
      case 'favorite': {
        const data = FavoriteSchema.parse(body);
        await service.favoriteSticker(data.userId, data.stickerId);
        return NextResponse.json({ success: true });
      }
      
      case 'unfavorite': {
        const data = FavoriteSchema.parse(body);
        await service.unfavoriteSticker(data.userId, data.stickerId);
        return NextResponse.json({ success: true });
      }
      
      case 'addPack': {
        const { userId, packId } = body;
        if (!userId || !packId) {
          return NextResponse.json({ error: 'userId and packId required' }, { status: 400 });
        }
        await service.addPackToLibrary(userId, packId);
        return NextResponse.json({ success: true });
      }
      
      case 'removePack': {
        const { userId, packId } = body;
        if (!userId || !packId) {
          return NextResponse.json({ error: 'userId and packId required' }, { status: 400 });
        }
        await service.removePackFromLibrary(userId, packId);
        return NextResponse.json({ success: true });
      }
      
      default:
        return NextResponse.json(
          { error: 'Invalid action' },
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
    console.error('Sticker action error:', error);
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
    const packId = searchParams.get('packId');
    const type = searchParams.get('type'); // 'packs', 'favorites', 'recent', 'custom', 'available'
    
    const service = getStickerGifService();
    
    // Get available packs
    if (type === 'available') {
      const packs = await service.getAvailablePacks();
      return NextResponse.json({ packs });
    }
    
    // Get pack stickers
    if (packId) {
      const stickers = await service.getPackStickers(packId);
      return NextResponse.json({ stickers });
    }
    
    if (!userId) {
      return NextResponse.json(
        { error: 'userId required' },
        { status: 400 }
      );
    }
    
    switch (type) {
      case 'packs': {
        const packs = await service.getUserPacks(userId);
        return NextResponse.json({ packs });
      }
      
      case 'favorites': {
        const stickers = await service.getFavoriteStickers(userId);
        return NextResponse.json({ stickers });
      }
      
      case 'recent': {
        const stickers = await service.getRecentStickers(userId);
        return NextResponse.json({ stickers });
      }
      
      case 'custom': {
        const stickers = await service.getCustomStickers(userId);
        return NextResponse.json({ stickers });
      }
      
      default:
        return NextResponse.json(
          { error: 'Invalid type parameter' },
          { status: 400 }
        );
    }
  } catch (error) {
    console.error('Get stickers error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
