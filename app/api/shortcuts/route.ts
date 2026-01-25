// Shortcuts API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getShortcutService, initializeDefaultShortcuts } from '@/lib/domain/shortcuts/service';

const CreateShortcutSchema = z.object({
  userId: z.string(),
  trigger: z.string().min(1).max(20),
  expansion: z.string().min(1).max(2000),
  name: z.string().max(50).optional(),
  category: z.string().max(30).optional(),
  isGlobal: z.boolean().optional(),
});

const UpdateShortcutSchema = z.object({
  shortcutId: z.string(),
  trigger: z.string().min(1).max(20).optional(),
  expansion: z.string().min(1).max(2000).optional(),
  name: z.string().max(50).optional(),
  category: z.string().max(30).optional(),
});

const ExpandSchema = z.object({
  userId: z.string(),
  text: z.string(),
});

export async function POST(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const action = searchParams.get('action');
    const body = await request.json();
    
    const service = getShortcutService();
    
    // Initialize default shortcuts for user
    if (action === 'init') {
      const { userId } = body;
      if (!userId) {
        return NextResponse.json({ error: 'userId required' }, { status: 400 });
      }
      await initializeDefaultShortcuts(userId);
      return NextResponse.json({ success: true });
    }
    
    // Expand text
    if (action === 'expand') {
      const data = ExpandSchema.parse(body);
      const expanded = await service.expandText(data.userId, data.text);
      return NextResponse.json({ expanded });
    }
    
    // Create shortcut
    const data = CreateShortcutSchema.parse(body);
    const shortcut = await service.createShortcut(
      data.userId,
      data.trigger,
      data.expansion,
      {
        name: data.name,
        category: data.category,
        isGlobal: data.isGlobal,
      }
    );
    
    return NextResponse.json({ success: true, shortcut });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    if (error instanceof Error && error.message.includes('already exists')) {
      return NextResponse.json(
        { error: error.message },
        { status: 409 }
      );
    }
    console.error('Shortcut error:', error);
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
    const shortcutId = searchParams.get('shortcutId');
    const query = searchParams.get('query');
    const trigger = searchParams.get('trigger');
    
    const service = getShortcutService();
    
    // Get specific shortcut
    if (shortcutId) {
      const shortcut = await service.getShortcut(shortcutId);
      if (!shortcut) {
        return NextResponse.json({ error: 'Shortcut not found' }, { status: 404 });
      }
      return NextResponse.json({ shortcut });
    }
    
    if (!userId) {
      return NextResponse.json({ error: 'userId required' }, { status: 400 });
    }
    
    // Search shortcuts
    if (query) {
      const shortcuts = await service.searchShortcuts(userId, query);
      return NextResponse.json({ shortcuts });
    }
    
    // Find by trigger
    if (trigger) {
      const shortcut = await service.findMatchingShortcut(userId, trigger);
      return NextResponse.json({ shortcut });
    }
    
    // Get all user shortcuts
    const shortcuts = await service.getUserShortcuts(userId);
    return NextResponse.json({ shortcuts });
  } catch (error) {
    console.error('Get shortcuts error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function PUT(request: NextRequest) {
  try {
    const body = await request.json();
    const data = UpdateShortcutSchema.parse(body);
    
    const service = getShortcutService();
    const shortcut = await service.updateShortcut(data.shortcutId, {
      trigger: data.trigger,
      expansion: data.expansion,
      name: data.name,
      category: data.category,
    });
    
    if (!shortcut) {
      return NextResponse.json({ error: 'Shortcut not found' }, { status: 404 });
    }
    
    return NextResponse.json({ success: true, shortcut });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Update shortcut error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function DELETE(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const shortcutId = searchParams.get('shortcutId');
    
    if (!shortcutId) {
      return NextResponse.json({ error: 'shortcutId required' }, { status: 400 });
    }
    
    const service = getShortcutService();
    const success = await service.deleteShortcut(shortcutId);
    
    return NextResponse.json({ success });
  } catch (error) {
    console.error('Delete shortcut error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
