# MCP Server Architecture: Dynamic AI Rules & Language Detection

**Project**: Global AI Rules MCP Server  
**Type**: System Architecture & Implementation Strategy  
**Version**: 1.0.0  
**Status**: Architecture Planning Phase  

---

## 📋 Executive Summary

This document outlines the architecture for an MCP (Model Context Protocol) server that will transform our static global AI rules system into a dynamic, intelligent service. The MCP server will automatically detect programming languages, load appropriate FP style guides, validate code against patterns, and integrate seamlessly with AI coding assistants including Cursor, Kimi, Claude, and Gemini.

**Key Innovation**: Zero-configuration functional programming pattern enforcement. Users simply start coding, and their AI assistant automatically applies the correct language-specific functional programming patterns.

---

## 🎯 Project Goals

### Primary Objectives

1. **Automatic Language Detection**: Detect primary programming language from project files
2. **Dynamic Style Guide Loading**: Load appropriate FP style guide based on detected language
3. **Real-time Code Validation**: Validate code snippets against FP patterns
4. **Automatic Rule Application**: Create and apply AI tool rules files without manual intervention
5. **Multi-AI Tool Integration**: Support Cursor, Kimi, Claude, and Gemini simultaneously
6. **SetTodoList Integration**: Native task tracking integration for Kimi
7. **Cross-Tool Consistency**: Maintain same FP patterns across all AI assistants

### Success Metrics

- **Detection Accuracy**: >95% correct language detection
- **Setup Time**: <5 seconds from project start to rule application
- **User Experience**: Zero manual configuration required
- **Tool Coverage**: 4/4 major AI assistants supported
- **Language Coverage**: 6/6 languages (Python, TypeScript, Rust, Kotlin, Swift, Haskell)
- **Platform Coverage**: AWS, GCP auto-detection

---

## 🏗️ System Architecture

### Deployment Architecture

```
┌─────────────────────────────────────────────────────────────┐
│              Digital Ocean App Platform                     │
│                                                             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  MCP Server Container                                │  │
│  │  ┌──────────────┐  ┌────────────────────────────┐  │  │
│  │  │  MCP Server  │  │  Rules Repository          │  │  │
│  │  │  (Node.js)   │◄─┼─┤  - ai-tool/*.md          │  │  │
│  │  │              │  │  - templates/              │  │  │
│  │  └──────────────┘  └────────────────────────────┘  │  │
│  │         ▲                                                  │
│  └─────────┼──────────────────────────────────────────────┘  │
│            │ MCP Protocol (HTTP/WebSocket)                      │
│            │                                                    │
│  ┌─────────┼──────────────────────────────────────────────┐  │
│  │  Health Check Endpoint                                  │  │
│  │  GET /health → 200 OK                                   │  │
│  └─────────────────────────────────────────────────────────┘  │
│                                                               │
│  Environment Variables:                                       │
│  - RULES_PATH=/app/rules                                    │
│  - NODE_ENV=production                                      │
│  - PORT=8080                                                │
└─────────────────────────────────────────────────────────────┘
                            │
                            │ Internet
                            │
┌───────────────────────────┼───────────────────────────────┐
│                           ▼                               │
│  ┌────────────────────────────────────────────────────┐  │
│  │  AI Assistant (Cursor/Kimi/Claude/Gemini)        │  │
│  │  MCP Client Configuration                        │  │
│  │  - transport: http                               │  │
│  │  - url: https://mcp-server.ondigitalocean.app  │  │
│  └────────────────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────┘
```

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   AI Assistant (Cursor/Kimi/Claude/Gemini)  │
└────────────────────────┬────────────────────────────────────┘
                         │ MCP Protocol
                         ▼
┌─────────────────────────────────────────────────────────────┐
│  MCP Server (Node.js/TypeScript)                           │
│  Package: @modelcontextprotocol/sdk                        │
└──────────┬──────────────────┬──────────────────┬────────────┘
           │                  │                  │
           │                  │                  ▼
           │                  │         ┌──────────────────┐
           │                  │         │  Resources API   │
           │                  │         │  (Read-only)     │
           │                  │         └──────────────────┘
           │                  │         │ Style Guides     │
           │                  │         │ Templates        │
           │                  │         │ Examples         │
           │                  │         └──────────────────┘
           │                  ▼
           │         ┌──────────────────┐
           │         │   Tools API      │
           │         │  (Read-Write)    │
           │         └────────┬─────────┘
           │                  │
           ▼                  ▼
┌──────────────────┐  ┌──────────────────┐
│ detect_language  │  │ setup_project    │
│ validate_code    │  │ track_todo       │
│ load_style_guide │  │ ...              │
└────────┬─────────┘  └────────┬─────────┘
         │                     │
         ▼                     ▼
┌────────────────────────────────────────┐
│  Local Project Directory               │
│  - .py/.ts/.rs files                   │
│  - requirements.txt/Cargo.toml         │
│  - Git repository                      │
└────────────────────────────────────────┘
```

### Component Breakdown

**0. Deployment Layer (Digital Ocean App Platform)**
- **App Spec**: YAML configuration for deployment
- **Build Process**: npm install, npm run build, copy rules
- **Runtime**: Node.js 20+ container
- **Health Checks**: HTTP endpoint for uptime monitoring
- **Scaling**: Automatic scaling based on request volume
- **Environment**: Managed secrets and environment variables

**1. Core Server (index.ts)**
- MCP server entry point
- Tool registration
- Request routing
- Transport layer (HTTP/WebSocket for cloud deployment)

**1. Core Server (index.ts)**
- MCP server entry point
- Tool registration
- Request routing
- Transport layer (stdio)

**2. Language Detection Engine** 
- File system analysis
- Pattern matching algorithms
- Confidence scoring
- Caching layer

**3. Style Guide Loader**
- Reads from disk: ai-tool/*.md files
- Content formatting
- Pattern extraction
- Cross-reference resolution

**4. Code Validator**
- AST parsing (tree-sitter)
- Pattern matching
- Violation detection
- Suggestion generation

**5. Project Setup Tool**
- Rules file generation
- Template application
- Path resolution
- Validation

**6. Resources API**
- Read-only access to style guides
- Structured data format
- Search capabilities
- Version tracking

### Data Flow

**Workflow 0: Cloud Deployment Flow**
```
Git Push → Digital Ocean Build → Container Deployed
    ↓
Rules Repository Copied → RULES_PATH Set
    ↓
MCP Server Starts → Health Check Passes
    ↓
AI Assistant Connects → Tools Available
```

**Workflow 1: New Project Setup**

**Workflow 1: New Project Setup**
```
User creates project → Opens AI assistant
    ↓
AI calls: detect_language(".")
    ↓
MCP analyzes files → Returns: { language: "python", confidence: 0.98 }
    ↓
AI calls: load_style_guide({ language: "python", aiTool: "kimi" })
    ↓
MCP reads: kimi/python-fp-style-guide.md
    ↓
AI calls: setup_project_rules({ aiTool: "kimi", platform: "aws" })
    ↓
MCP creates: .kimirules (35 lines)
    ↓
AI: "I've set up Kimi with Python FP patterns. Start coding!"
```

**Workflow 2: Code Validation**
```
User writes code → AI assistant active
    ↓
AI calls: validate_code({ code: "def foo():
    try:
        return x
    except:", language: "python" })
    ↓
MCP analyzes → Finds: try/except (violates Result type pattern)
    ↓
MCP returns: {
  violations: [
    { line: 2, issue: "Uses exceptions", suggestion: "Use returns.Result" }
  ],
  complianceScore: 0.45
}
    ↓
AI suggests refactoring to use Result types
```

---

## 🛠️ Technical Implementation

### Digital Ocean App Platform Configuration

```yaml
# .do/app.yaml
name: mcp-fp-rules
services:
- name: mcp-server
  github:
    repo: your-org/mcp-server
    branch: main
    deploy_on_push: true
  build_command: npm run build
  run_command: npm start
  environment_slug: node-js
  instance_count: 1
  instance_size_slug: basic-xxs
  routes:
  - path: /
  health_check:
    http_path: /health
    initial_delay_seconds: 10
    period_seconds: 10
    timeout_seconds: 5
    success_threshold: 1
    failure_threshold: 3
  envs:
  - key: RULES_PATH
    value: /app/rules
    scope: RUN_AND_BUILD_TIME
  - key: NODE_ENV
    value: production
    scope: RUN_TIME
  - key: PORT
    value: 8080
    scope: RUN_TIME
```

### Repository Structure for Deployment

```
mcp-server/
├── src/
│   ├── index.ts              # MCP server entry
│   ├── tools/                # Tool implementations
│   └── utils/                # Utilities
├── rules/                    # Rules repository (submodule or copy)
│   ├── ai-tool/
│   ├── templates/
│   └── examples/
├── package.json
├── tsconfig.json
├── .do/
│   └── app.yaml              # Digital Ocean config
└── Dockerfile                # Optional: custom container
```

### Build & Deployment Process

```bash
# Build script (package.json)
"scripts": {
  "build": "tsc && npm run copy-rules",
  "copy-rules": "cp -r ../rules ./rules || echo 'Rules copied'",
  "start": "node dist/index.js",
  "dev": "tsx src/index.ts"
}
```

### Environment Configuration

```typescript
// src/config.ts
export const config = {
  rulesPath: process.env.RULES_PATH || './rules',
  port: parseInt(process.env.PORT || '8080'),
  nodeEnv: process.env.NODE_ENV || 'development',
  healthCheckPath: '/health'
};

// Health check endpoint for Digital Ocean
app.get('/health', (req, res) => {
  res.status(200).json({ status: 'ok', timestamp: new Date().toISOString() });
});
```

### Transport Layer Configuration

```typescript
// HTTP/WebSocket transport for cloud deployment
import express from 'express';
import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { SSEServerTransport } from '@modelcontextprotocol/sdk/server/sse.js';

const app = express();
const server = new Server(...);

app.post('/mcp', async (req, res) => {
  const transport = new SSEServerTransport('/mcp', res);
  await server.connect(transport);
});

app.listen(config.port, () => {
  console.log(`MCP server running on port ${config.port}`);
});
```

### Technology Stack

- **Runtime**: Node.js 18+ or Deno
- **Language**: TypeScript 5.0+
- **MCP SDK**: @modelcontextprotocol/sdk
- **File System**: Node fs/promises
- **Pattern Matching**: glob + custom matchers
- **AST Parsing**: tree-sitter (optional, for code validation)
- **Validation**: Zod schemas
- **Testing**: vitest or Jest

### Detection Algorithm

```typescript
// Priority-based language detection
async function detectLanguage(dir: string): Promise<DetectionResult> {
  const checks = [
    // High confidence indicators
    { lang: "python", files: ["requirements.txt", "pyproject.toml", "setup.py"], weight: 1.0 },
    { lang: "typescript", files: ["tsconfig.json"], weight: 1.0 },
    { lang: "rust", files: ["Cargo.toml"], weight: 1.0 },
    
    // Medium confidence (source files)
    { lang: "python", pattern: "**/*.py", minCount: 3, weight: 0.8 },
    { lang: "typescript", pattern: "**/*.ts", minCount: 3, weight: 0.8 },
    
    // Lower confidence (generic indicators)
    { lang: "python", files: ["*.py"], weight: 0.6 }
  ];
  
  let bestMatch = { language: "unknown", confidence: 0 };
  
  for (const check of checks) {
    const matches = await glob(check.files || check.pattern, { cwd: dir });
    
    if (matches.length >= (check.minCount || 1)) {
      const confidence = check.weight * (matches.length / 10); // Normalize
      
      if (confidence > bestMatch.confidence) {
        bestMatch = {
          language: check.lang,
          confidence: Math.min(confidence, 1.0)
        };
      }
    }
  }
  
  return bestMatch;
}
```

### Platform Detection

```typescript
async function detectPlatform(dir: string): Promise<string[]> {
  const platforms = [];
  
  // AWS indicators
  if (await fileExists("serverless.yml") || 
      await fileExists("sam.yaml") ||
      await hasFileWithContent("*.tf", "aws_")) {
    platforms.push("aws");
  }
  
  // GCP indicators  
  if (await fileExists("app.yaml") ||
      await hasFileWithContent("*.tf", "google_")) {
    platforms.push("gcp");
  }
  
  return platforms;
}
```

---
 
 ## 🔒 Security Architecture & Best Practices
 
 ### Authentication & Authorization
 
 **API Key Authentication (Recommended)**
 ```typescript
 // src/middleware/auth.ts
 import { Request, Response, NextFunction } from 'express';
 
 const API_KEYS = new Set([
   process.env.MCP_API_KEY_1,
   process.env.MCP_API_KEY_2,
   process.env.MCP_API_KEY_3
 ].filter(Boolean));
 
 export function authenticateApiKey(req: Request, res: Response, next: NextFunction) {
   const apiKey = req.headers['x-api-key'] || req.query.apikey;
   
   if (!apiKey || !API_KEYS.has(apiKey as string)) {
     return res.status(401).json({ error: 'Unauthorized: Invalid API key' });
   }
   
   next();
 }
 ```
 
 **MCP Protocol Token Validation**
 ```typescript
 // Validate MCP protocol tokens in request headers
 export function validateMcpToken(req: Request, res: Response, next: NextFunction) {
   const authHeader = req.headers['authorization'];
   const mcpToken = req.headers['x-mcp-token'];
   
   if (!authHeader || !authHeader.startsWith('Bearer ') || !mcpToken) {
     return res.status(401).json({ error: 'Unauthorized: Missing MCP credentials' });
   }
   
   // Verify token against environment variables
   const token = authHeader.split(' ')[1];
   if (token !== process.env.MCP_BEARER_TOKEN) {
     return res.status(401).json({ error: 'Unauthorized: Invalid bearer token' });
   }
   
   next();
 }
 ```
 
 ### Transport Layer Security (TLS/HTTPS)
 
 **Digital Ocean Managed TLS**
 - ✅ **Automatic TLS**: Digital Ocean App Platform provides automatic TLS certificates
 - ✅ **HTTPS Only**: Configure app to only accept HTTPS connections
 - ✅ **Certificate Rotation**: Managed automatically by Digital Ocean
 - ✅ **HSTS Headers**: Add HTTP Strict Transport Security headers
 
 ```typescript
 // src/middleware/security.ts
 export function enforceHttps(req: Request, res: Response, next: NextFunction) {
   const proto = req.headers['x-forwarded-proto'];
   
   if (proto && proto !== 'https' && process.env.NODE_ENV === 'production') {
     return res.status(403).json({ 
       error: 'HTTPS required. Use https://' + req.get('host') + req.originalUrl 
     });
   }
   
   // Add HSTS header
   res.setHeader('Strict-Transport-Security', 'max-age=31536000; includeSubDomains');
   next();
 }
 ```
 
 ### Network Security
 
 **Digital Ocean App Platform Features**
 - ✅ **Private Networking**: Use internal networking for service-to-service communication
 - ✅ **Ingress Firewall**: Digital Ocean manages firewall rules at the edge
 - ✅ **VPC Isolation**: Deploy in isolated Virtual Private Cloud
 - ✅ **IP Allowlisting**: Restrict access to known AI assistant IPs
 
 ```yaml
 # .do/app.yaml - Network security configuration
 services:
 - name: mcp-server
   # ... other config ...
   internal_ports:
   - 8080  # Only accessible within App Platform
   cors:
     allow_origins:
     - https://kimi.ai
     - https://cursor.sh
     - https://claude.ai
     - https://gemini.google.com
     allow_methods:
     - POST
     - GET
     allow_headers:
     - Authorization
     - X-API-Key
     - X-MCP-Token
     - Content-Type
     max_age: 86400
 ```
 
 ### Input Validation & Sanitization
 
 **Zod Schema Validation**
 ```typescript
 import { z } from 'zod';
 
 const detectLanguageSchema = z.object({
   directory: z.string()
     .min(1, 'Directory path is required')
     .max(500, 'Directory path too long')
     .regex(/^[a-zA-Z0-9_\-\/\.]+$/, 'Invalid directory path format')
     .transform(path => path.replace(/\.\./g, '')), // Prevent directory traversal
   confidenceThreshold: z.number()
     .min(0)
     .max(1)
     .default(0.8)
 });
 
 export function validateInput(schema: z.ZodSchema) {
   return (req: Request, res: Response, next: NextFunction) => {
     try {
       req.validatedData = schema.parse(req.body);
       next();
     } catch (error) {
       return res.status(400).json({
         error: 'Invalid input',
         details: error instanceof z.ZodError ? error.errors : 'Validation failed'
       });
     }
   };
 }
 ```
 
 **Request Size Limiting**
 ```typescript
 // Limit request body size to prevent DoS
 app.use(express.json({ limit: '1mb' }));
 app.use(express.urlencoded({ limit: '1mb', extended: true }));
 ```
 
 ### Rate Limiting & Abuse Prevention
 
 **Per-Client Rate Limiting**
 ```typescript
 import rateLimit from 'express-rate-limit';
 
 const limiter = rateLimit({
   windowMs: 15 * 60 * 1000, // 15 minutes
   max: 100, // limit each API key to 100 requests per windowMs
   standardHeaders: true,
   legacyHeaders: false,
   keyGenerator: (req) => {
     return req.headers['x-api-key']?.toString() || req.ip;
   },
   handler: (req, res) => {
     res.status(429).json({
       error: 'Too many requests',
       retryAfter: Math.ceil(req.rateLimit.resetTime / 1000)
     });
   }
 });
 
 // Apply to all MCP endpoints
 app.use('/mcp', limiter);
 ```
 
 **Tool-Specific Rate Limits**
 ```typescript
 const strictLimiter = rateLimit({
   windowMs: 60 * 1000, // 1 minute
   max: 30, // stricter limit for expensive operations
   skip: (req) => {
     // Skip rate limiting for health checks
     return req.path === '/health';
   }
 });
 
 app.use('/mcp/tools/validate-code', strictLimiter);
 ```
 
 ### Secrets Management
 
 **Digital Ocean Managed Secrets**
 ```yaml
 # .do/app.yaml - Secrets configuration
 services:
 - name: mcp-server
   # ... other config ...
   envs:
   - key: MCP_API_KEY_1
     value: ${MCP_API_KEY_1}
     type: SECRET
     scope: RUN_TIME
   - key: MCP_API_KEY_2
     value: ${MCP_API_KEY_2}
     type: SECRET
     scope: RUN_TIME
   - key: MCP_BEARER_TOKEN
     value: ${MCP_BEARER_TOKEN}
     type: SECRET
     scope: RUN_TIME
   - key: RULES_PATH
     value: /app/rules
     scope: RUN_AND_BUILD_TIME
 ```
 
 **Environment Variable Validation**
 ```typescript
 // src/config.ts
 export function validateEnv() {
   const required = [
     'MCP_API_KEY_1',
     'MCP_BEARER_TOKEN',
     'RULES_PATH'
   ];
   
   const missing = required.filter(key => !process.env[key]);
   
   if (missing.length > 0) {
     throw new Error(`Missing required environment variables: ${missing.join(', ')}`);
   }
   
   return {
     apiKeys: [
       process.env.MCP_API_KEY_1,
       process.env.MCP_API_KEY_2,
       process.env.MCP_API_KEY_3
     ].filter(Boolean),
     bearerToken: process.env.MCP_BEARER_TOKEN,
     rulesPath: process.env.RULES_PATH,
     port: parseInt(process.env.PORT || '8080'),
     nodeEnv: process.env.NODE_ENV || 'development'
   };
 }
 ```
 
 ### Logging & Monitoring
 
 **Structured Logging**
 ```typescript
 import winston from 'winston';
 
 const logger = winston.createLogger({
   level: process.env.LOG_LEVEL || 'info',
   format: winston.format.combine(
     winston.format.timestamp(),
     winston.format.errors({ stack: true }),
     winston.format.json()
   ),
   defaultMeta: { service: 'mcp-server' },
   transports: [
     new winston.transports.Console({
       format: winston.format.combine(
         winston.format.colorize(),
         winston.format.simple()
       )
     })
   ]
 });
 
 // Log security events
 export function logSecurityEvent(event: string, details: any) {
   logger.warn('SECURITY_EVENT', { event, details, timestamp: new Date().toISOString() });
 }
 ```
 
 **Digital Ocean Insights Integration**
 - ✅ **Log Forwarding**: Automatically forwarded to Digital Ocean Insights
 - ✅ **Metrics**: CPU, memory, request count, error rate
 - ✅ **Alerting**: Configure alerts for high error rates or security events
 - ✅ **Dashboards**: Visualize security metrics
 
 ### CORS & Origin Restrictions
 
 ```typescript
 import cors from 'cors';
 
 const allowedOrigins = [
   'https://kimi.ai',
   'https://cursor.sh',
   'https://claude.ai',
   'https://gemini.google.com'
 ];
 
 const corsOptions = {
   origin: (origin: string | undefined, callback: Function) => {
     // Allow requests with no origin (mobile apps, curl)
     if (!origin) {
       return callback(null, true);
     }
     
     if (allowedOrigins.includes(origin)) {
       callback(null, true);
     } else {
       logSecurityEvent('CORS_VIOLATION', { origin });
       callback(new Error('Origin not allowed by CORS'));
     }
   },
   methods: ['POST', 'GET'],
   allowedHeaders: ['Authorization', 'X-API-Key', 'X-MCP-Token', 'Content-Type'],
   credentials: true,
   maxAge: 86400
 };
 
 app.use(cors(corsOptions));
 ```
 
 ### Error Handling & Information Disclosure
 
 **Generic Error Messages**
 ```typescript
 // Don't leak stack traces or internal details
 app.use((err: Error, req: Request, res: Response, next: NextFunction) => {
   logger.error('Unhandled error', { 
     error: err.message,
     stack: err.stack,
     path: req.path,
     method: req.method
   });
   
   res.status(500).json({
     error: 'Internal server error',
     requestId: req.headers['x-request-id']
   });
 });
 ```
 
 **404 Handling**
 ```typescript
 app.use((req: Request, res: Response) => {
   logSecurityEvent('UNKNOWN_ENDPOINT', {
     path: req.path,
     method: req.method,
     ip: req.ip,
     userAgent: req.get('User-Agent')
   });
   
   res.status(404).json({ error: 'Endpoint not found' });
 });
 ```
 
 ### Digital Ocean Specific Security Features
 
 **App Platform Security**
 - ✅ **Automatic OS Patching**: Digital Ocean keeps underlying OS updated
 - ✅ **Container Isolation**: Each app runs in isolated containers
 - ✅ **DDoS Protection**: Automatic DDoS mitigation at edge
 - ✅ **WAF**: Web Application Firewall available as add-on
 - ✅ **VPC**: Deploy in private VPC with controlled egress
 
 **Recommended Configuration**
 ```yaml
 # .do/app.yaml - Security-hardened configuration
 name: mcp-fp-rules
 services:
 - name: mcp-server
   github:
     repo: your-org/mcp-server
     branch: main
     deploy_on_push: true
   build_command: npm run build
   run_command: npm start
   environment_slug: node-js
   instance_count: 1
   instance_size_slug: basic-xxs
   routes:
   - path: /
   health_check:
     http_path: /health
     initial_delay_seconds: 10
     period_seconds: 10
     timeout_seconds: 5
     success_threshold: 1
     failure_threshold: 3
   
   # Security configurations
   cors:
     allow_origins:
     - https://kimi.ai
     - https://cursor.sh
     - https://claude.ai
     - https://gemini.google.com
     allow_methods:
     - POST
     - GET
     allow_headers:
     - Authorization
     - X-API-Key
     - X-MCP-Token
     - Content-Type
     max_age: 86400
   
   # Secrets management
   envs:
   - key: MCP_API_KEY_1
     value: ${MCP_API_KEY_1}
     type: SECRET
     scope: RUN_TIME
   - key: MCP_API_KEY_2
     value: ${MCP_API_KEY_2}
     type: SECRET
     scope: RUN_TIME
   - key: MCP_BEARER_TOKEN
     value: ${MCP_BEARER_TOKEN}
     type: SECRET
     scope: RUN_TIME
   - key: RULES_PATH
     value: /app/rules
     scope: RUN_AND_BUILD_TIME
   - key: NODE_ENV
     value: production
     scope: RUN_TIME
   - key: LOG_LEVEL
     value: info
     scope: RUN_TIME
   
   # Security headers
   headers:
   - key: X-Content-Type-Options
     value: nosniff
   - key: X-Frame-Options
     value: DENY
   - key: X-XSS-Protection
     value: 1; mode=block
   - key: Strict-Transport-Security
     value: max-age=31536000; includeSubDomains
   - key: Content-Security-Policy
     value: "default-src 'self'"
 
   # Internal port (not exposed to internet directly)
   internal_ports:
   - 8080
 ```
 
 ### Security Checklist
 
 **Before Deployment:**
 - [ ] API keys generated and stored in Digital Ocean secrets
 - [ ] Bearer token generated (32+ character random string)
 - [ ] CORS origins restricted to known AI assistants
 - [ ] Rate limiting configured
 - [ ] Request size limits set (1MB max)
 - [ ] Input validation with Zod schemas implemented
 - [ ] Health check endpoint secured (no sensitive data)
 - [ ] Error messages generic (no stack traces)
 - [ ] Logging configured (no sensitive data in logs)
 - [ ] Security headers added (HSTS, CSP, X-Frame-Options)
 - [ ] Digital Ocean WAF enabled (if available)
 - [ ] VPC deployment configured
 - [ ] IP allowlisting considered (if static IPs known)
 
 **After Deployment:**
 - [ ] Test authentication with invalid API key (should fail)
 - [ ] Test authentication with valid API key (should succeed)
 - [ ] Test rate limiting (should block after limit)
 - [ ] Test CORS from allowed origins (should succeed)
 - [ ] Test CORS from disallowed origins (should fail)
 - [ ] Verify health check endpoint accessible
 - [ ] Verify logs in Digital Ocean Insights
 - [ ] Set up alerts for high error rates
 - [ ] Set up alerts for security events
 - [ ] Review logs for any suspicious activity
 - [ ] Schedule regular security reviews
 
 ---
 
 ## 📊 Implementation Phases

### **Phase 0: Digital Ocean Deployment Setup** (Estimated: 2-3 hours)

**Goal**: Deploy MCP server to Digital Ocean App Platform with rules repository

**Tasks:**
- Create Digital Ocean App Platform spec (app.yaml)
- Configure build process to include rules repository
- Set up environment variables and secrets
- Implement health check endpoint
- Configure HTTP/WebSocket transport
- Test deployment and connectivity

**Deliverables:**
- `.do/app.yaml` - App Platform configuration
- Updated build scripts in package.json
- Health check endpoint implementation
- Deployment documentation

**Success Criteria:**
- ✅ MCP server deployed successfully
- ✅ Health checks passing
- ✅ Rules repository accessible
- ✅ AI assistants can connect via HTTP
- ✅ All tools functional in cloud environment

**Phase 0 Tasks**: 5/5  
**Phase 0 Estimated**: 2-3 hours

### **Phase 1: Core Foundation** (Estimated: 4-6 hours)

**Tasks:**
- [ ] Initialize TypeScript Node.js project
- [ ] Install MCP SDK and dependencies
- [ ] Create basic server structure
- [ ] Implement stdio transport
- [ ] Add tool registration framework

**Deliverables:**
- MCP server that responds to `ListToolsRequest`
- Basic tool skeleton structure
- Development environment configured
- CI/CD pipeline setup

### **Phase 2: Language Detection** (Estimated: 3-4 hours)

**Tasks:**
- [ ] Implement `detect_language` tool
- [ ] Add glob pattern matching
- [ ] Create confidence scoring algorithm
- [ ] Add caching layer for performance
- [ ] Write comprehensive tests

**Deliverables:**
- >95% accurate language detection
- Fast detection (<1 second)
- Tests with sample projects

### **Phase 3: Style Guide Loading** (Estimated: 3-4 hours)

**Tasks:**
- [ ] Implement `load_style_guide` tool
- [ ] Read style guides from disk
- [ ] Format content for AI consumption
- [ ] Extract key patterns and examples
- [ ] Add cross-reference resolution

**Deliverables:**
- Correct guide loading per language
- Proper path resolution
- Formatted, AI-ready content

### **Phase 4: Project Setup** (Estimated: 3-4 hours)

**Tasks:**
- [ ] Implement `setup_project_rules` tool
- [ ] Create rules file templates
- [ ] Add AI tool-specific content
- [ ] Handle platform detection
- [ ] Add config file creation

**Deliverables:**
- Automatic .cursorrules/.kimirules creation
- Appropriate content for each AI tool
- Platform-specific additions

### **Phase 5: Resources API** (Estimated: 2-3 hours)

**Tasks:**
- [ ] Implement `ListResources`
- [ ] Implement `ReadResource`
- [ ] Expose all style guides
- [ ] Expose templates and examples
- [ ] Add resource filtering

**Deliverables:**
- All guides accessible via MCP
- Search capabilities
- Structured data format

### **Phase 6: Advanced Features** (Estimated: 4-6 hours)

**Tasks:**
- [ ] Implement `validate_code` tool
- [ ] Add SetTodoList integration
- [ ] Create batch operations support
- [ ] Add platform detection
- [ ] Implement caching layer

**Deliverables:**
- Code validation working
- Task tracking integrated
- Performance optimizations

### **Phase 7: Testing & Polishing** (Estimated: 3-4 hours)

**Tasks:**
- [ ] Write unit tests
- [ ] Create integration tests
- [ ] Test with real projects
- [ ] Add error handling
- [ ] Create documentation

**Deliverables:**
- >80% test coverage
- Working with sample projects
- Comprehensive README

### **Phase 8: Integration** (Estimated: 2-3 hours)

**Tasks:**
- [ ] Create MCP configuration files
- [ ] Test with Kimi CLI
- [ ] Test with Cursor (when MCP available)
- [ ] Create example workflows
- [ ] Document usage patterns

**Deliverables:**
- MCP server working with AI assistants
- Clear usage examples
- Video/gif demonstrations

**Total Estimated Time**: 24-34 hours (~3-4 full work days)

---

## 🎓 User Experience After MCP

### **Scenario 1: New Python Project (30 seconds)**

```bash
# User creates new directory
mkdir data-pipeline && cd data-pipeline
echo "import polars as pl

def process_data(df: pl.DataFrame) -> pl.DataFrame:
    return df.filter(pl.col('status') == 'active')" > main.py

# User opens terminal, starts Kimi
kimi

# Kimi (with MCP):
# 1. Calls: detect_language(".")
# 2. Detects: Python (confidence: 0.98)
# 3. Loads: kimi/python-fp-style-guide.md
# 4. Calls: setup_project_rules({ aiTool: "kimi" })
# 5. Creates: .kimirules (30 lines)

# Kimi says:
"Detected Python project. I've set up Kimi with functional 
programming patterns including Result types, railway-oriented 
programming, and immutable data transformations. Using returns 
library for error handling. Start coding!"

# User asks: "Process this CSV safely"
# Kimi writes:
from returns.result import Result, Success, Failure
from returns.pipeline import flow
from returns.pointfree import map

def load_and_process(filepath: str) -> Result[pl.DataFrame, Exception]:
    return flow(
        validate_path(filepath),
        bind(read_csv),
        bind(validate_schema),
        bind(clean_data),
        bind(process_data)
    )

# User: Zero configuration, immediate FP patterns!
```

### **Scenario 2: TypeScript Web App (45 seconds)**

```bash
mkdir web-app && cd web-app
npm init -y
npm install next@latest typescript @types/node
touch tsconfig.json

# User: Opens in Cursor with MCP

# Cursor (with MCP):
# 1. Calls: detect_language(".")
# 2. Detects: TypeScript (confidence: 0.95)
# 3. Loads: cursor/typescript-fp-style-guide.md
# 4. Also detects: next.config.js → Next.js
# 5. Creates: .cursorrules (45 lines)

# Cursor suggests:
"I'll use fp-ts for railway-oriented programming with 
Next.js. Here are the key patterns: TaskEither for async 
operations, pipe for composition, and exhaustive error 
handling. Should I create the project structure?"

# User: "Yes, create a type-safe API route"
# Cursor generates:
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'

export default async function handler(
  req: NextApiRequest
): Promise<NextApiResponse> {
  return await pipe(
    validateRequest(req),
    TE.fromEither,
    TE.chain(processBusinessLogic),
    TE.chain(saveToDatabase),
    TE.map(createSuccessResponse),
    TE.mapLeft(handleError)
  )()
}
```

### **Scenario 3: Rust Project with Validation**

```bash
cargo new --bin cli-tool
cd cli-tool

# User: Starts coding

# AI with MCP:
# 1. Detects: Rust (confidence: 1.0)
# 2. Loads: rust-fp-style-guide.md
# 3. User writes: unwrap() call

# AI warns:
"I noticed you're using unwrap() which can panic. 
According to our Rust FP patterns, consider using 
Result with proper error handling. Here's the safer approach:"

# Suggests:
use std::fs::File;

// Instead of:
// let file = File::open("data.txt").unwrap();

// Use:
let file: Result<File, std::io::Error> = File::open("data.txt");
match file {
    Ok(f) => process_file(f),
    Err(e) => log_error(e),
}
```

---

## 📈 Impact & Benefits

### **For End Users**

- **Zero Config**: No manual file creation
- **Instant**: Patterns applied within seconds
- **Accurate**: >95% correct detection
- **Consistent**: Same patterns across projects
- **Flexible**: Works with any AI assistant

### **For Teams**

- **Faster Onboarding**: New projects set up automatically
- **Consistent Standards**: Same FP patterns everywhere
- **Reduced Errors**: AI enforces patterns in real-time
- **Better Collaboration**: Shared understanding of patterns

### **For AI Assistants**

- **Better Context**: Rules loaded dynamically
- **More Helpful**: Pattern-aware suggestions
- **Integrated**: SetTodoList, validation, setup in one flow
- **Smarter**: Learns project patterns over time

---

## 🔮 Future Enhancements

### **Phase 10: Multi-Region Deployment**

- Deploy to multiple Digital Ocean regions
- Load balancing across regions
- Automatic failover
- Regional caching of rules

### **Phase 11: Advanced Monitoring**

- Prometheus metrics integration
- Grafana dashboards
- Log aggregation (Digital Ocean Insights)
- Performance monitoring
- Error tracking (Sentry)

### **Phase 12: Serverless Optimization**

- Optimize for Digital Ocean Functions
- Reduce cold start times
- Implement connection pooling
- Add request queuing

### **Phase 9: Machine Learning (Post MVP)**

- Learn project-specific patterns
- Suggest custom rules based on codebase
- Predict next actions
- Personalization layer

### **Phase 10: Multi-Language Projects**

- Detect polyglot projects
- Load multiple style guides
- Handle language interop patterns
- Mixed-language validation

### **Phase 11: Team Patterns**

- Learn from team behavior
- Suggest team-specific additions
- Codify team conventions
- Collaborative rule evolution

### **Phase 12: IDE Integration**

- VS Code extension
- JetBrains plugin
- Real-time validation
- In-editor suggestions

---

## 📋 Next Steps

**Immediate (This Session):**
1. ✅ Architecture plan updated with Digital Ocean deployment
2. ⏳ Update implementation plan with deployment phase
3. ⏳ Update TODO list with deployment tasks
4. ⏳ Review and approve deployment strategy

**Deployment-Specific Next Steps:**
1. Create Digital Ocean account and configure access
2. Set up GitHub integration for automatic deployments
3. Configure environment variables and secrets
4. Test deployment with minimal MCP server
5. Verify rules repository accessibility
6. Test AI assistant connectivity
7. Monitor health checks and logs
8. Scale based on usage patterns

**Short-term (Next Session):**
1. Implement Phase 0: Digital Ocean deployment setup
2. Deploy minimal MCP server (Phase 1-2)
3. Test with Kimi CLI over HTTP
4. Verify end-to-end workflow

**Medium-term (This Week):**
1. Complete all phases (0-9)
2. Full integration testing with cloud deployment
3. Performance testing and optimization
4. Documentation and examples for cloud setup
5. Beta release to team

**Long-term (This Month):**
1. Production deployment monitoring
2. Community testing and feedback
3. Iterate and improve based on usage
4. Plan future enhancements (Phases 10-12)
5. Consider multi-region deployment

**Immediate (This Session):**
1. ✅ Architecture plan created (this document)
2. ⏳ Create implementation plan (docs/plans/MCP_IMPLEMENTATION_PLAN.md)
3. ⏳ Create TODO list (docs/plans/MCP_IMPLEMENTATION_TODO.md)
4. ⏳ Get approval for implementation

**Short-term (Next Session):**
1. Create MCP server project structure
2. Implement core server foundation
3. Build language detection tool
4. Test with sample directories

**Medium-term (Next Week):**
1. Complete all phases (1-8)
2. Full integration testing
3. Documentation and examples
4. Beta release to team

**Long-term (Next Month):**
1. Community testing and feedback
2. Performance optimizations
3. Future enhancements (Phase 9-12)
4. Production release

---

## 🎯 Success Criteria

**Deployment Success Criteria:**
- ✅ MCP server deployed to Digital Ocean App Platform
- ✅ Health checks consistently passing
- ✅ Rules repository accessible at runtime
- ✅ AI assistants can connect via HTTP/WebSocket
- ✅ All tools functional in production environment
- ✅ Logs visible in Digital Ocean dashboard
- ✅ Can scale automatically based on demand
- ✅ Zero-downtime deployments working

**Implementation Complete When:**
- ✅ MCP server responds to all tool requests
- ✅ Language detection >95% accurate
- ✅ Style guides load correctly from deployed rules
- ✅ Project setup creates appropriate files
- ✅ Works with Kimi CLI over HTTP
- ✅ Tested with sample projects (Python, TypeScript, Rust)
- ✅ Documentation complete including deployment guide
- ✅ Team successfully uses it
- ✅ Digital Ocean deployment stable and monitored

**Production Ready When:**
- ✅ All tests passing
- ✅ Performance acceptable (<1s detection)
- ✅ Error handling robust
- ✅ Community feedback positive
- ✅ No critical bugs
- ✅ Health checks and monitoring in place
- ✅ Automatic deployments from GitHub
- ✅ Scaled appropriately for usage

**Implementation Complete When:**
- ✅ MCP server responds to all tool requests
- ✅ Language detection >95% accurate
- ✅ Style guides load correctly
- ✅ Project setup creates appropriate files
- ✅ Works with Kimi CLI
- ✅ Tested with sample projects (Python, TypeScript, Rust)
- ✅ Documentation complete
- ✅ Team successfully uses it

**Production Ready When:**
- ✅ All tests passing
- ✅ Performance acceptable (<1s detection)
- ✅ Error handling robust
- ✅ Community feedback positive
- ✅ No critical bugs

---

**Document Metadata:**
- **Status**: Architecture Planning Complete
- **Created**: 2025-11-14
- **Version**: 1.0.0
- **Next**: Implementation Plan & TODO list
- **Estimated Implementation**: 24-34 hours
- **Priority**: High (game-changing feature)

🤖 Generated with [Kimi](https://kimi.ai)

**Note**: This architecture plan transforms the static global rules system into a dynamic, intelligent MCP service that automatically detects languages and applies appropriate functional programming patterns. This represents a significant evolution from manual file creation to zero-configuration AI-assisted development.
