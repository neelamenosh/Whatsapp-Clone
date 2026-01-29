'use client';

import { cn } from '@/lib/utils';

// Mock data for servers/nodes
export const sidebarServers = [
    { id: 'home', icon: '🏠', isActive: true, color: 'bg-primary' },
    { id: 'add', icon: '+', isActive: false },
    { id: 's1', avatar: 'https://images.unsplash.com/photo-1614680376593-902f74cf0d41?w=50&h=50&fit=crop', badge: null },
    { id: 's2', emoji: '🌍', badge: null },
    { id: 's3', text: 'UX', badge: null, bgColor: 'bg-purple-500' },
    { id: 's4', avatar: 'https://images.unsplash.com/photo-1560179707-f14e90ef3623?w=50&h=50&fit=crop', badge: 17 },
    { id: 's5', emoji: '🎯', badge: null },
    { id: 's6', emoji: '📊', badge: 153 },
    { id: 's7', avatar: 'https://images.unsplash.com/photo-1573497019940-1c28c88b4f3e?w=50&h=50&fit=crop', badge: 17 },
    { id: 's8', avatar: 'https://images.unsplash.com/photo-1557862921-37829c790f19?w=50&h=50&fit=crop', badge: 153 },
];

export function ServerSidebar({ activeId = 'home', onSelect }: { activeId?: string, onSelect?: (id: string) => void }) {
    return (
        <div className="w-16 bg-muted/30 border-r border-border flex flex-col items-center py-3 gap-3 overflow-y-auto scrollbar-hide shrink-0">
            {sidebarServers.map((server) => (
                <div key={server.id} className="relative">
                    {server.id === 'home' ? (
                        <button
                            type="button"
                            onClick={() => onSelect?.(server.id)}
                            className={cn(
                                "w-12 h-12 rounded-2xl flex items-center justify-center text-xl transition-all",
                                server.id === activeId ? "bg-primary text-white" : "bg-muted hover:bg-muted/80"
                            )}
                        >
                            🏠
                        </button>
                    ) : server.id === 'add' ? (
                        <button
                            type="button"
                            onClick={() => onSelect?.(server.id)}
                            className="w-12 h-12 rounded-2xl border-2 border-dashed border-muted-foreground/30 flex items-center justify-center text-muted-foreground hover:border-primary hover:text-primary transition-colors"
                        >
                            <span className="text-xl">+</span>
                        </button>
                    ) : server.avatar ? (
                        <button
                            type="button"
                            onClick={() => onSelect?.(server.id)}
                            className={cn(
                                "w-12 h-12 rounded-2xl overflow-hidden ring-2 transition-all",
                                server.id === activeId ? "ring-primary" : "ring-transparent hover:ring-primary/50"
                            )}
                        >
                            <img
                                src={server.avatar}
                                alt="Server"
                                className="w-full h-full object-cover"
                                crossOrigin="anonymous"
                            />
                        </button>
                    ) : server.text ? (
                        <button
                            type="button"
                            onClick={() => onSelect?.(server.id)}
                            className={cn(
                                "w-12 h-12 rounded-2xl flex items-center justify-center text-white font-bold text-sm transition-all",
                                server.id === activeId ? "ring-2 ring-primary" : "",
                                server.bgColor || "bg-gray-500"
                            )}
                        >
                            {server.text}
                        </button>
                    ) : (
                        <button
                            type="button"
                            onClick={() => onSelect?.(server.id)}
                            className={cn(
                                "w-12 h-12 rounded-2xl flex items-center justify-center text-xl transition-all",
                                server.id === activeId ? "bg-primary/20 ring-2 ring-primary" : "bg-muted hover:bg-muted/80"
                            )}
                        >
                            {server.emoji}
                        </button>
                    )}

                    {/* Badge */}
                    {server.badge && (
                        <span className="absolute -top-1 -right-1 min-w-[18px] h-[18px] bg-primary text-white text-[10px] font-bold rounded-full flex items-center justify-center px-1 border-2 border-background">
                            {server.badge}
                        </span>
                    )}
                </div>
            ))}
        </div>
    );
}
