// Translation API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getTranslationService } from '@/lib/domain/translation/service';

const TranslateSchema = z.object({
  messageId: z.string(),
  originalText: z.string(),
  targetLanguage: z.string(),
  sourceLanguage: z.string().optional(),
});

const AutoTranslateSchema = z.object({
  userId: z.string(),
  chatId: z.string(),
  enabled: z.boolean(),
});

const PreferenceSchema = z.object({
  userId: z.string(),
  language: z.string(),
});

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const data = TranslateSchema.parse(body);
    
    const service = getTranslationService();
    const translation = await service.translateMessage(
      data.messageId,
      data.originalText,
      data.targetLanguage,
      data.sourceLanguage
    );
    
    return NextResponse.json({ success: true, translation });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Translation error:', error);
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
    const targetLanguage = searchParams.get('targetLanguage');
    const languages = searchParams.get('languages');
    const detect = searchParams.get('detect');
    const text = searchParams.get('text');
    
    const service = getTranslationService();
    
    // Get supported languages
    if (languages === 'true') {
      const supportedLanguages = await service.getSupportedLanguages();
      return NextResponse.json({ languages: supportedLanguages });
    }
    
    // Detect language
    if (detect === 'true' && text) {
      const detection = await service.detectLanguage(text);
      return NextResponse.json({ detection });
    }
    
    // Get cached translation
    if (messageId && targetLanguage) {
      const translation = await service.getTranslation(messageId, targetLanguage);
      if (!translation) {
        return NextResponse.json(
          { error: 'Translation not found' },
          { status: 404 }
        );
      }
      return NextResponse.json({ translation });
    }
    
    return NextResponse.json(
      { error: 'Invalid parameters' },
      { status: 400 }
    );
  } catch (error) {
    console.error('Get translation error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function PUT(request: NextRequest) {
  try {
    const body = await request.json();
    
    // Handle auto-translate toggle
    if ('enabled' in body) {
      const data = AutoTranslateSchema.parse(body);
      const service = getTranslationService();
      
      if (data.enabled) {
        await service.enableAutoTranslate(data.userId, data.chatId);
      } else {
        await service.disableAutoTranslate(data.userId, data.chatId);
      }
      
      return NextResponse.json({ success: true });
    }
    
    // Handle language preference
    const data = PreferenceSchema.parse(body);
    const service = getTranslationService();
    await service.setUserPreferredLanguage(data.userId, data.language);
    
    return NextResponse.json({ success: true });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Update translation settings error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
