// Groups API Route
import { NextRequest, NextResponse } from 'next/server';
import { z } from 'zod';
import { getGroupService } from '@/lib/domain/groups/service';

const CreateGroupSchema = z.object({
  name: z.string().min(1).max(100),
  description: z.string().max(500).optional(),
  creatorId: z.string(),
  initialMembers: z.array(z.string()).optional(),
  settings: z.object({
    onlyAdminsCanSend: z.boolean().optional(),
    onlyAdminsCanAddMembers: z.boolean().optional(),
    onlyAdminsCanEditInfo: z.boolean().optional(),
    requireApprovalToJoin: z.boolean().optional(),
    disappearingMessages: z.object({
      enabled: z.boolean(),
      duration: z.number(),
    }).optional(),
    muteNotifications: z.boolean().optional(),
  }).optional(),
});

const AddMemberSchema = z.object({
  groupId: z.string(),
  userId: z.string(),
  addedBy: z.string(),
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
    const data = CreateGroupSchema.parse(body);
    
    const service = getGroupService();
    const group = await service.createGroup(
      data.name,
      data.creatorId,
      data.description,
      data.initialMembers,
      data.settings
    );
    
    return NextResponse.json({ success: true, group });
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Invalid request', details: error.errors },
        { status: 400 }
      );
    }
    console.error('Create group error:', error);
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
    const userId = searchParams.get('userId');
    
    const service = getGroupService();
    
    if (groupId) {
      const group = await service.getGroup(groupId);
      if (!group) {
        return NextResponse.json(
          { error: 'Group not found' },
          { status: 404 }
        );
      }
      return NextResponse.json({ group });
    }
    
    if (userId) {
      const groups = await service.getUserGroups(userId);
      return NextResponse.json({ groups });
    }
    
    return NextResponse.json(
      { error: 'groupId or userId required' },
      { status: 400 }
    );
  } catch (error) {
    console.error('Get group error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function PUT(request: NextRequest) {
  try {
    const body = await request.json();
    const { groupId, updates, updatedBy } = body;
    
    if (!groupId || !updates || !updatedBy) {
      return NextResponse.json(
        { error: 'groupId, updates, and updatedBy required' },
        { status: 400 }
      );
    }
    
    const service = getGroupService();
    const group = await service.updateGroup(groupId, updates, updatedBy);
    
    if (!group) {
      return NextResponse.json(
        { error: 'Group not found or not authorized' },
        { status: 404 }
      );
    }
    
    return NextResponse.json({ success: true, group });
  } catch (error) {
    console.error('Update group error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function DELETE(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const groupId = searchParams.get('groupId');
    const userId = searchParams.get('userId');
    
    if (!groupId || !userId) {
      return NextResponse.json(
        { error: 'groupId and userId required' },
        { status: 400 }
      );
    }
    
    const service = getGroupService();
    const success = await service.leaveGroup(groupId, userId);
    
    return NextResponse.json({ success });
  } catch (error) {
    console.error('Leave group error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
