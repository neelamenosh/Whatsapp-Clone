'use client';

import { useState } from 'react';
import { cn } from '@/lib/utils';
import {
    ChevronDown,
    ChevronRight,
    Search,
    UserPlus,
    Hash,
    Volume2,
    Globe,
    VolumeX
} from 'lucide-react';
import { ServerSidebar } from '@/components/shared/server-sidebar';
import type { Node, NodeSection, Channel, User } from '@/lib/types';

// Mock data for the Node view
const mockNode: Node = {
    id: '1',
    name: 'Mainnet',
    sections: [
        {
            id: 's1',
            name: "Mainnet's home",
            type: 'category',
            isExpanded: true,
            channels: [
                { id: 'c1', name: 'Welcome', type: 'text' },
                { id: 'c2', name: 'Rules', type: 'text' },
                { id: 'c3', name: 'Admin', type: 'text' },
            ]
        },
        {
            id: 's2',
            name: 'Design is my passion',
            type: 'category',
            isExpanded: true,
            channels: [
                { id: 'c4', name: 'Design & Dev', type: 'text', unreadCount: 15 },
                { id: 'c5', name: 'Reference Pic', type: 'text' },
                { id: 'c6', name: 'Assets', type: 'text' },
            ]
        },
        {
            id: 's3',
            name: 'External Connection',
            type: 'external',
            icon: 'globe',
            isExpanded: true,
            channels: [
                { id: 'c7', name: "Klaive's Barbecue", type: 'text' },
                { id: 'c8', name: 'Meeting of the Creators', type: 'text' },
                {
                    id: 'c9',
                    name: 'Success Squad',
                    type: 'voice',
                    participants: [
                        { id: 'u1', name: 'Maria', avatar: 'https://images.unsplash.com/photo-1494790108377-be9c29b29330?w=50&h=50&fit=crop&crop=face', status: 'online' },
                        { id: 'u2', name: 'Julie', avatar: 'https://images.unsplash.com/photo-1438761681033-6461ffad8d80?w=50&h=50&fit=crop&crop=face', status: 'online' },
                    ]
                },
            ]
        },
    ]
};

// Redundant sidebar mock data removed and moved to shared component

interface SectionProps {
    section: NodeSection;
    onToggle: () => void;
}

function Section({ section, onToggle }: SectionProps) {
    return (
        <div className="mb-2">
            {/* Section Header */}
            <button
                type="button"
                onClick={onToggle}
                className="flex items-center gap-2 w-full px-4 py-2 hover:bg-muted/50 transition-colors"
            >
                {section.type === 'external' && (
                    <Globe className="h-4 w-4 text-primary" />
                )}
                <span className="text-sm font-medium text-foreground">{section.name}</span>
                <ChevronDown className={cn(
                    "h-4 w-4 text-muted-foreground transition-transform",
                    !section.isExpanded && "-rotate-90"
                )} />
            </button>

            {/* Channels */}
            {section.isExpanded && (
                <div className="space-y-0.5">
                    {section.channels.map((channel) => (
                        <ChannelItem key={channel.id} channel={channel} />
                    ))}
                </div>
            )}
        </div>
    );
}

function ChannelItem({ channel }: { channel: Channel }) {
    const isVoice = channel.type === 'voice';

    return (
        <div>
            <button
                type="button"
                className="channel-item w-full text-left"
            >
                {isVoice ? (
                    <Volume2 className="h-4 w-4 text-muted-foreground shrink-0" />
                ) : (
                    <Hash className="h-4 w-4 text-muted-foreground shrink-0" />
                )}
                <span className="flex-1 text-sm text-foreground truncate">{channel.name}</span>

                {channel.unreadCount && channel.unreadCount > 0 && (
                    <span className="badge-unread">{channel.unreadCount}</span>
                )}

                {isVoice && !channel.participants?.length && (
                    <VolumeX className="h-4 w-4 text-muted-foreground" />
                )}
            </button>

            {/* Voice channel participants */}
            {isVoice && channel.participants && channel.participants.length > 0 && (
                <div className="pl-10 space-y-1 py-1">
                    {channel.participants.map((user) => (
                        <div key={user.id} className="flex items-center gap-2 py-1 px-2">
                            <img
                                src={user.avatar}
                                alt={user.name}
                                className="w-6 h-6 rounded-full object-cover"
                                crossOrigin="anonymous"
                            />
                            <span className="text-sm text-muted-foreground">{user.name}</span>
                        </div>
                    ))}
                </div>
            )}
        </div>
    );
}

export function NodeView() {
    const [node, setNode] = useState<Node>(mockNode);

    const toggleSection = (sectionId: string) => {
        setNode(prev => ({
            ...prev,
            sections: prev.sections.map(s =>
                s.id === sectionId ? { ...s, isExpanded: !s.isExpanded } : s
            )
        }));
    };

    return (
        <div className="flex h-full">
            <ServerSidebar activeId="home" />
            {/* Main Content */}
            <div className="flex-1 flex flex-col overflow-hidden">
                {/* Header */}
                <div className="px-4 py-3 border-b border-border">
                    <button
                        type="button"
                        className="flex items-center gap-2 text-foreground"
                    >
                        <span className="font-semibold">{node.name}</span>
                        <ChevronRight className="h-4 w-4" />
                    </button>

                    {/* Action buttons */}
                    <div className="flex gap-2 mt-3">
                        <button
                            type="button"
                            className="flex items-center gap-2 px-4 py-2 rounded-full border border-border hover:bg-muted transition-colors"
                        >
                            <Search className="h-4 w-4" />
                            <span className="text-sm">Search</span>
                        </button>
                        <button
                            type="button"
                            className="flex items-center gap-2 px-4 py-2 rounded-full border border-border hover:bg-muted transition-colors"
                        >
                            <UserPlus className="h-4 w-4" />
                            <span className="text-sm">Add Members</span>
                        </button>
                    </div>
                </div>

                {/* Sections */}
                <div className="flex-1 overflow-y-auto scrollbar-hide py-2">
                    {node.sections.map((section) => (
                        <Section
                            key={section.id}
                            section={section}
                            onToggle={() => toggleSection(section.id)}
                        />
                    ))}
                </div>
            </div>
        </div>
    );
}
