## Project Summary
A pixel-perfect WhatsApp clone with real-time messaging, presence tracking, and enterprise-grade security features. It features a mobile-responsive design, multi-tab navigation (Chats, Status, Calls), and integrates with Supabase for real-time data synchronization.

## Tech Stack
- **Framework**: Next.js (App Router)
- **Language**: TypeScript
- **Styling**: Tailwind CSS, Lucide React (Icons)
- **Database/Auth**: Supabase
- **Real-time**: WebSockets (concurrently running with Next.js)
- **State Management**: Custom hooks and localStorage for persistence
- **Package Manager**: pnpm

## Architecture
- `app/`: Next.js App Router pages and API routes.
- `components/`: Reusable UI components (chat, navigation, profile, settings).
- `lib/`: Core logic, including domain models, database client, and state stores.
- `server/`: WebSocket server implementation.
- `styles/`: Global styles and Tailwind configurations.

## User Preferences
- None yet.

## Project Guidelines
- Use named exports for components.
- Keep client components lean; use React Server Components where possible.
- Mimic WhatsApp's UI/UX as closely as possible.

## Common Patterns
- Real-time updates via Supabase subscriptions.
- Persistent state using `localStorage` with a fallback to mock data.
- Mobile-first responsive design.
