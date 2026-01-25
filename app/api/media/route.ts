// Media API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getMediaService } from '@/lib/domain/media/service';

const InitiateUploadSchema = z.object({
  userId: z.string(),
  chatId: z.string(),
  filename: z.string(),
  mimeType: z.string(),
  size: z.number().positive(),
});

const CompleteUploadSchema = z.object({
  mediaId: z.string(),
  url: z.string().url(),
  thumbnailUrl: z.string().url().optional(),
});

const FetchPreviewSchema = z.object({
  url: z.string().url(),
});

export async function POST(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const action = searchParams.get('action');
    const body = await request.json();
    
    const service = getMediaService();
    
    switch (action) {
      case 'initiate': {
        const data = InitiateUploadSchema.parse(body);
        const upload = await service.initiateUpload(
          data.userId,
          data.chatId,
          data.filename,
          data.mimeType,
          data.size
        );
        return NextResponse.json({ success: true, upload });
      }
      
      case 'complete': {
        const data = CompleteUploadSchema.parse(body);
        const media = await service.completeUpload(
          data.mediaId,
          data.url,
          data.thumbnailUrl
        );
        if (!media) {
          return NextResponse.json({ error: 'Upload not found' }, { status: 404 });
        }
        return NextResponse.json({ success: true, media });
      }
      
      case 'preview': {
        const data = FetchPreviewSchema.parse(body);
        const preview = await service.fetchLinkPreview(data.url);
        return NextResponse.json({ preview });
      }
      
      case 'thumbnail': {
        const { mediaId, thumbnailType } = body;
        if (!mediaId) {
          return NextResponse.json({ error: 'mediaId required' }, { status: 400 });
        }
        const thumbnail = await service.generateThumbnail(mediaId, thumbnailType ?? 'small');
        return NextResponse.json({ thumbnail });
      }
      
      default:
        return NextResponse.json(
          { error: 'Invalid action. Use: initiate, complete, preview, or thumbnail' },
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
    console.error('Media error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const mediaId = searchParams.get('mediaId');
    const chatId = searchParams.get('chatId');
    const thumbnailType = searchParams.get('thumbnailType');
    const url = searchParams.get('previewUrl');
    
    const service = getMediaService();
    
    // Get link preview for URL
    if (url) {
      const preview = await service.getLinkPreview(url);
      return NextResponse.json({ preview });
    }
    
    // Get specific media
    if (mediaId) {
      const media = await service.getMedia(mediaId);
      if (!media) {
        return NextResponse.json({ error: 'Media not found' }, { status: 404 });
      }
      
      // Include thumbnail if requested
      if (thumbnailType) {
        const thumbnail = await service.getThumbnail(
          mediaId,
          thumbnailType as 'small' | 'medium' | 'large'
        );
        return NextResponse.json({ media, thumbnail });
      }
      
      return NextResponse.json({ media });
    }
    
    // Get chat media
    if (chatId) {
      const mediaList = await service.getChatMedia(chatId);
      return NextResponse.json({ media: mediaList });
    }
    
    return NextResponse.json(
      { error: 'mediaId, chatId, or previewUrl required' },
      { status: 400 }
    );
  } catch (error) {
    console.error('Get media error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function DELETE(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const mediaId = searchParams.get('mediaId');
    
    if (!mediaId) {
      return NextResponse.json(
        { error: 'mediaId required' },
        { status: 400 }
      );
    }
    
    const service = getMediaService();
    const success = await service.deleteMedia(mediaId);
    
    return NextResponse.json({ success });
  } catch (error) {
    console.error('Delete media error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
