// Collaboration API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getCollaborationService } from '@/lib/domain/collaboration/service';

const CreateDocumentSchema = z.object({
  chatId: z.string(),
  createdBy: z.string(),
  title: z.string().min(1).max(200),
  type: z.enum(['text', 'markdown', 'richtext']).optional(),
});

const UpdateDocumentContentSchema = z.object({
  docId: z.string(),
  userId: z.string(),
  content: z.string(),
});

const CreateWhiteboardSchema = z.object({
  chatId: z.string(),
  createdBy: z.string(),
  name: z.string().min(1).max(200),
});

const AddElementSchema = z.object({
  whiteboardId: z.string(),
  userId: z.string(),
  type: z.enum(['pen', 'shape', 'text', 'image', 'sticky']),
  data: z.record(z.unknown()),
  position: z.object({
    x: z.number(),
    y: z.number(),
  }),
  size: z.object({
    width: z.number(),
    height: z.number(),
  }).optional(),
  style: z.object({
    color: z.string().optional(),
    strokeWidth: z.number().optional(),
    fill: z.string().optional(),
    fontSize: z.number().optional(),
  }).optional(),
});

export async function POST(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const type = searchParams.get('type');
    const body = await request.json();
    
    const service = getCollaborationService();
    
    switch (type) {
      case 'document': {
        const data = CreateDocumentSchema.parse(body);
        const doc = await service.createDocument(
          data.chatId,
          data.createdBy,
          data.title,
          data.type
        );
        return NextResponse.json({ success: true, document: doc });
      }
      
      case 'whiteboard': {
        const data = CreateWhiteboardSchema.parse(body);
        const whiteboard = await service.createWhiteboard(
          data.chatId,
          data.createdBy,
          data.name
        );
        return NextResponse.json({ success: true, whiteboard });
      }
      
      case 'element': {
        const data = AddElementSchema.parse(body);
        const element = await service.addWhiteboardElement(
          data.whiteboardId,
          data.userId,
          {
            type: data.type,
            data: data.data,
            position: data.position,
            size: data.size,
            style: data.style,
          }
        );
        return NextResponse.json({ success: true, element });
      }
      
      default:
        return NextResponse.json(
          { error: 'Invalid type. Use: document, whiteboard, or element' },
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
    console.error('Collaboration error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const type = searchParams.get('type');
    const chatId = searchParams.get('chatId');
    const docId = searchParams.get('docId');
    const whiteboardId = searchParams.get('whiteboardId');
    
    const service = getCollaborationService();
    
    // Get specific document
    if (type === 'document' && docId) {
      const doc = await service.getDocument(docId);
      if (!doc) {
        return NextResponse.json({ error: 'Document not found' }, { status: 404 });
      }
      return NextResponse.json({ document: doc });
    }
    
    // Get chat documents
    if (type === 'documents' && chatId) {
      const documents = await service.getChatDocuments(chatId);
      return NextResponse.json({ documents });
    }
    
    // Get specific whiteboard
    if (type === 'whiteboard' && whiteboardId) {
      const whiteboard = await service.getWhiteboard(whiteboardId);
      if (!whiteboard) {
        return NextResponse.json({ error: 'Whiteboard not found' }, { status: 404 });
      }
      const elements = await service.getWhiteboardElements(whiteboardId);
      return NextResponse.json({ whiteboard, elements });
    }
    
    // Get chat whiteboards
    if (type === 'whiteboards' && chatId) {
      const whiteboards = await service.getChatWhiteboards(chatId);
      return NextResponse.json({ whiteboards });
    }
    
    // Get whiteboard elements
    if (type === 'elements' && whiteboardId) {
      const elements = await service.getWhiteboardElements(whiteboardId);
      return NextResponse.json({ elements });
    }
    
    // Get document collaborators
    if (type === 'collaborators' && docId) {
      const collaborators = await service.getDocumentCollaborators(docId);
      return NextResponse.json({ collaborators });
    }
    
    return NextResponse.json(
      { error: 'Invalid parameters' },
      { status: 400 }
    );
  } catch (error) {
    console.error('Get collaboration error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function PUT(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const type = searchParams.get('type');
    const body = await request.json();
    
    const service = getCollaborationService();
    
    switch (type) {
      case 'document': {
        const data = UpdateDocumentContentSchema.parse(body);
        const doc = await service.updateDocumentContent(
          data.docId,
          data.userId,
          data.content
        );
        if (!doc) {
          return NextResponse.json({ error: 'Document not found' }, { status: 404 });
        }
        return NextResponse.json({ success: true, document: doc });
      }
      
      case 'element': {
        const { whiteboardId, elementId, userId, updates } = body;
        if (!whiteboardId || !elementId || !userId) {
          return NextResponse.json(
            { error: 'whiteboardId, elementId, and userId required' },
            { status: 400 }
          );
        }
        await service.updateWhiteboardElement(whiteboardId, elementId, userId, updates);
        return NextResponse.json({ success: true });
      }
      
      case 'join': {
        const { docId, userId } = body;
        if (!docId || !userId) {
          return NextResponse.json(
            { error: 'docId and userId required' },
            { status: 400 }
          );
        }
        await service.joinDocument(docId, userId);
        return NextResponse.json({ success: true });
      }
      
      case 'leave': {
        const { docId, userId } = body;
        if (!docId || !userId) {
          return NextResponse.json(
            { error: 'docId and userId required' },
            { status: 400 }
          );
        }
        await service.leaveDocument(docId, userId);
        return NextResponse.json({ success: true });
      }
      
      default:
        return NextResponse.json(
          { error: 'Invalid type' },
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
    console.error('Update collaboration error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function DELETE(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const whiteboardId = searchParams.get('whiteboardId');
    const elementId = searchParams.get('elementId');
    const userId = searchParams.get('userId');
    
    if (!whiteboardId || !elementId || !userId) {
      return NextResponse.json(
        { error: 'whiteboardId, elementId, and userId required' },
        { status: 400 }
      );
    }
    
    const service = getCollaborationService();
    await service.removeWhiteboardElement(whiteboardId, elementId, userId);
    
    return NextResponse.json({ success: true });
  } catch (error) {
    console.error('Delete element error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
