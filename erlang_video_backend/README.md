# Enterprise Video Call Backend

A carrier-grade, WhatsApp-class video calling backend built with Erlang/OTP, XMPP signaling, and WebRTC.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Erlang/OTP Backend                       │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │                   Root Supervisor                     │  │
│  │  (video_signaling_sup - one_for_one)                 │  │
│  ├──────────────────────────────────────────────────────┤  │
│  │                                                       │  │
│  │  ┌─────────────────┐  ┌─────────────────┐            │  │
│  │  │ Connection Sup  │  │   Call Sup      │            │  │
│  │  │ (simple_1_for_1)│  │ (simple_1_for_1)│            │  │
│  │  ├─────────────────┤  ├─────────────────┤            │  │
│  │  │ UserConnection  │  │  CallProcess    │            │  │
│  │  │ (1 per user)    │  │ (1 per call)    │            │  │
│  │  └─────────────────┘  └─────────────────┘            │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                             │
│  ┌──────────────────┐  ┌────────────────────────────────┐  │
│  │   Mnesia Store   │  │      Cowboy Web Server         │  │
│  │  (user_session)  │  │  - WebSocket handler           │  │
│  │  (call_session)  │  │  - Auth handler (JWT)          │  │
│  │                  │  │  - TURN credentials            │  │
│  └──────────────────┘  └────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## Prerequisites

- Erlang/OTP 25+
- rebar3
- Coturn TURN server (for production)

### macOS
```bash
brew install erlang rebar3
```

### FreeBSD
```bash
pkg install -y erlang rebar3
```

### Ubuntu/Debian
```bash
apt-get install -y erlang rebar3
```

## Quick Start

1. **Compile the project**
   ```bash
   cd erlang_video_backend
   make compile
   ```

2. **Run in development mode**
   ```bash
   make dev
   ```

3. **Create production release**
   ```bash
   make prod
   ```

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `HTTP_PORT` | 8080 | HTTP listener port |
| `HTTPS_PORT` | 8443 | HTTPS listener port |
| `JWT_SECRET` | (change me) | JWT signing secret |
| `TURN_SECRET` | (change me) | TURN credential secret |

### Coturn Setup

1. Install Coturn:
   ```bash
   apt-get install coturn  # Debian/Ubuntu
   pkg install coturn      # FreeBSD
   ```

2. Copy configuration:
   ```bash
   cp config/turnserver.conf /etc/coturn/turnserver.conf
   ```

3. Update the `static-auth-secret` to match your `TURN_SECRET`

4. Start Coturn:
   ```bash
   systemctl start coturn
   ```

## API Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/ws` | WebSocket | XMPP signaling |
| `/api/auth/login` | POST | User login |
| `/api/auth/token` | POST | Token refresh |
| `/api/turn/credentials` | GET | Get TURN credentials |
| `/health` | GET | Health check |

## WebSocket Protocol

Connect to `/ws` and authenticate:

```json
{"type": "auth", "token": "your-jwt-token"}
```

### Call Initiation
```json
{"type": "call_initiate", "to": "user-id", "call_type": "video"}
```

### Accept Call
```json
{"type": "call_accept", "call_id": "call-uuid"}
```

### Send ICE Candidate
```json
{"type": "ice_candidate", "call_id": "call-uuid", "candidate": {...}}
```

## Scaling

The system is designed to scale horizontally:

- Each Erlang node can handle 100k+ concurrent connections
- Mnesia provides distributed session storage across nodes
- Use consistent hashing on user_id for connection routing

### Multi-Node Setup

1. Update `vm.args` with unique node names
2. Configure Mnesia replication:
   ```erlang
   mnesia:change_table_copy_type(user_session, node(), disc_copies).
   mnesia:add_table_copy(user_session, 'node2@host', ram_copies).
   ```

## License

Apache-2.0
