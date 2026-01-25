// Group Members API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getGroupService } from '@/lib/domain/groups/service';

const AddMemberSchema = z.object({
  groupId: z.string(),
  userId: z.string(),
  addedBy: z.string(),
});

const RemoveMemberSchema = z.object({
  groupId: z.string(),
  userId: z.string(),
  removedBy: z.string(),
});

const UpdateRoleSchema = z.object({
  groupId: z.string(),
  userId: z.string(),
  role: z.enum(['admin', 'member']),
  updatedBy: z.string(),
});

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const data = AddMemberSchema.parse(body);
    
    const service = getGroupService();
    const success = await service.addMember(
      data.groupId,
      data.userId,
      data.addedBy
    );
    
    if (!success) {
      return NextResponse.json(
        { error: 'Failed to add member' },
        { status: 400 }
      );
    }
    
    return NextResponse.json({ success: true });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Add member error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function DELETE(request: NextRequest) {
  try {
    const body = await request.json();
    const data = RemoveMemberSchema.parse(body);
    
    const service = getGroupService();
    const success = await service.removeMember(
      data.groupId,
      data.userId,
      data.removedBy
    );
    
    if (!success) {
      return NextResponse.json(
        { error: 'Failed to remove member' },
        { status: 400 }
      );
    }
    
    return NextResponse.json({ success: true });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Remove member error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function PATCH(request: NextRequest) {
  try {
    const body = await request.json();
    const data = UpdateRoleSchema.parse(body);
    
    const service = getGroupService();
    const success = await service.updateMemberRole(
      data.groupId,
      data.userId,
      data.role,
      data.updatedBy
    );
    
    if (!success) {
      return NextResponse.json(
        { error: 'Failed to update role' },
        { status: 400 }
      );
    }
    
    return NextResponse.json({ success: true });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Update role error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const groupId = searchParams.get('groupId');
    
    if (!groupId) {
      return NextResponse.json(
        { error: 'groupId required' },
        { status: 400 }
      );
    }
    
    const service = getGroupService();
    const members = await service.getGroupMembers(groupId);
    
    return NextResponse.json({ members });
  } catch (error) {
    console.error('Get members error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
