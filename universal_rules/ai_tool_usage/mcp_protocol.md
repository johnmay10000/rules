---
title: MCP Protocol Usage Guidelines
+category: universal_rules
+type: ai_tool_usage
+applies_to: all
+version: 1.0.0
+last_updated: 2025-11-19
+---
+
+# MCP Protocol Usage Guidelines
+
+**Status**: MANDATORY - All AI tool integrations must follow MCP protocol standards
+
+This document defines the universal guidelines for using the Model Context Protocol (MCP) across all AI assistants (Cursor, Kimi, Claude, Gemini) and projects.
+
+---
+
+## What is MCP?
+
+**Model Context Protocol (MCP)** is a standardized protocol for communication between AI assistants and external tools, services, and data sources. It enables:
+
+- **Tool Discovery**: AI assistants can discover available tools dynamically
+- **Structured Requests**: Type-safe, schema-validated tool calls
+- **Consistent Responses**: Standardized success/error handling
+- **Cross-Platform**: Works with any AI assistant that supports MCP
+- **Extensibility**: Easy to add new tools and capabilities
+
+---
+
+## Core Principles
+
+### 1. MCP-First Integration
+
+**Rule**: All AI tool integrations must use MCP when available.
+
+- Use MCP instead of custom protocols or ad-hoc integrations
+- Follow MCP specification for all tool implementations
+- Validate all requests and responses against MCP schemas
+- Use official MCP SDKs when available
+
+**Rationale**:
+- Standardization reduces integration complexity
+- MCP provides built-in validation and error handling
+- Tools work across multiple AI assistants automatically
+- Future-proof against protocol changes
+
+---
+
+### 2. Tool Discovery and Registration
+
+**Rule**: All tools must be properly registered and discoverable via MCP.
+
+**Required Tool Metadata**:
+- `name`: Unique tool identifier (kebab-case)
+- `description`: Clear, concise purpose statement
+- `inputSchema`: JSON Schema for request validation
+- `outputSchema`: JSON Schema for response validation
+- `version`: Semantic version (e.g., "1.0.0")
+- `author`: Tool creator/maintainer
+
+**Example Tool Registration**:
+```typescript
+// MCP Tool Registration
+const tools = [
+  {
+    name: "detect-language",
+    description: "Detects the primary programming language from project files",
+    inputSchema: {
+      type: "object",
+      properties: {
+        directory: {
+          type: "string",
+          description: "Project directory path to analyze"
+        },
+        confidenceThreshold: {
+          type: "number",
+          minimum: 0,
+          maximum: 1,
+          default: 0.8
+        }
+      },
+      required: ["directory"]
+    },
+    outputSchema: {
+      type: "object",
+      properties: {
+        language: { type: "string" },
+        confidence: { type: "number" },
+        detectedFiles: {
+          type: "array",
+          items: { type: "string" }
+        }
+      }
+    },
+    version: "1.0.0",
+    author: "Global AI Rules System"
+  }
+]
+```
+
+---
+
+### 3. Type Safety and Validation
+
+**Rule**: All MCP requests and responses must be type-safe and validated.
+
+**Input Validation**:
+- Use Zod (TypeScript), Pydantic (Python), or equivalent for runtime validation
+- Validate all required fields are present
+- Validate field types and formats
+- Validate constraints (min/max, patterns, enums)
+- Return clear validation errors
+
+**Example: Type-Safe Validation**:
+```typescript
+import { z } from 'zod';
+
+// Define schema with Zod
+const DetectLanguageSchema = z.object({
+  directory: z.string()
+    .min(1, "Directory path is required")
+    .max(500, "Directory path too long")
+    .regex(/^[a-zA-Z0-9_\-\/\.]+$/, "Invalid directory path format"),
+  confidenceThreshold: z.number()
+    .min(0)
+    .max(1)
+    .default(0.8)
+});
+
+// Validate request
+function validateRequest(input: unknown) {
+  try {
+    return DetectLanguageSchema.parse(input);
+  } catch (error) {
+    return {
+      error: "Invalid input",
+      details: error instanceof z.ZodError ? error.errors : "Validation failed"
+    };
+  }
+}
+```
+
+---
+
+### 4. Error Handling
+
+**Rule**: All errors must be handled according to MCP specification.
+
+**Error Response Format**:
+```json
+{
+  "error": {
+    "code": "ERROR_CODE",
+    "message": "Human-readable error message",
+    "details": {
+      // Additional context
+    },
+    "recoverable": true
+  }
+}
+```
+
+**Error Codes**:
+- `INVALID_INPUT`: Request validation failed
+- `TOOL_EXECUTION_ERROR`: Tool failed during execution
+- `RESOURCE_NOT_FOUND`: Requested resource doesn't exist
+- `PERMISSION_DENIED`: Insufficient permissions
+- `RATE_LIMITED`: Too many requests
+- `INTERNAL_ERROR`: Unexpected server error
+
+**Example: Proper Error Handling**:
+```typescript
+async function detectLanguage(input: unknown) {
+  try {
+    // Validate input
+    const validated = validateRequest(input);
+    if ('error' in validated) {
+      return {
+        error: {
+          code: 'INVALID_INPUT',
+          message: 'Request validation failed',
+          details: validated.error,
+          recoverable: true
+        }
+      };
+    }
+    
+    // Execute tool logic
+    const result = await analyzeProject(validated.directory);
+    
+    return { result };
+    
+  } catch (error) {
+    // Handle unexpected errors
+    return {
+      error: {
+        code: 'INTERNAL_ERROR',
+        message: 'Failed to detect language',
+        details: { 
+          error: error instanceof Error ? error.message : 'Unknown error'
+        },
+        recoverable: false
+      }
+    };
+  }
+}
+```
+
+---
+
+### 5. Security and Authentication
+
+**Rule**: All MCP connections must be secured with authentication and encryption.
+
+**Security Requirements**:
+- **Authentication**: API keys, bearer tokens, or mTLS
+- **Encryption**: TLS 1.2+ for all network connections
+- **Authorization**: Validate permissions for each tool call
+- **Rate Limiting**: Prevent abuse and DoS attacks
+- **Input Sanitization**: Prevent injection attacks
+- **Secrets Management**: Never hardcode credentials
+
+**Example: Secure MCP Configuration**:
+```yaml
+# MCP Configuration with Security
+mcp:
+  server:
+    url: "https://mcp-server.example.com"
+    auth:
+      type: "bearer_token"
+      token: "${MCP_API_TOKEN}"  # From environment variable
+    tls:
+      min_version: "1.2"
+      verify_cert: true
+    rate_limit:
+      requests_per_minute: 100
+      requests_per_hour: 1000
+```
+
+---
+
+### 6. Performance and Timeouts
+
+**Rule**: All MCP tool calls must have appropriate timeouts and performance targets.
+
+**Performance Requirements**:
+- **Tool Discovery**: < 1 second
+- **Tool Execution**: < 5 seconds (default)
+- **Complex Operations**: < 30 seconds (with progress updates)
+- **Timeout Configuration**: Configurable per tool
+- **Progress Reporting**: For long-running operations
+
+**Example: Timeout Configuration**:
+```typescript
+const toolConfig = {
+  detectLanguage: {
+    timeout: 5000,  // 5 seconds
+    retryAttempts: 3,
+    retryDelay: 1000  // 1 second between retries
+  },
+  loadStyleGuide: {
+    timeout: 2000,
+    retryAttempts: 2
+  },
+  validateCode: {
+    timeout: 10000,  // 10 seconds (complex operation)
+    progressCallback: (progress: number) => {
+      console.log(`Validation progress: ${progress}%`);
+    }
+  }
+};
+```
+
+---
+
+## MCP Implementation Patterns
+
+### Pattern 1: Tool Composition
+
+Combine multiple tools to create complex workflows:
+
+```typescript
+// Compose multiple MCP tools
+async function setupProject(directory: string) {
+  // Step 1: Detect language
+  const langResult = await mcp.call('detect-language', { directory });
+  if ('error' in langResult) return langResult;
+  
+  // Step 2: Load style guide
+  const guideResult = await mcp.call('load-style-guide', {
+    language: langResult.result.language
+  });
+  if ('error' in guideResult) return guideResult;
+  
+  // Step 3: Setup project rules
+  const setupResult = await mcp.call('setup-project-rules', {
+    language: langResult.result.language,
+    directory
+  });
+  
+  return setupResult;
+}
+```
+
+**Benefits**:
+- Railway-oriented programming (errors short-circuit)
+- Type-safe composition
+- Clear error propagation
+
+---
+
+### Pattern 2: Parallel Tool Execution
+
+Execute independent tools in parallel for better performance:
+
+```typescript
+// Execute multiple tools in parallel
+async function analyzeProject(directory: string) {
+  const [langResult, structureResult, testResult] = await Promise.all([
+    mcp.call('detect-language', { directory }),
+    mcp.call('analyze-project-structure', { directory }),
+    mcp.call('run-tests', { directory })
+  ]);
+  
+  return {
+    language: langResult,
+    structure: structureResult,
+    tests: testResult
+  };
+}
+```
+
+**Benefits**:
+- Faster execution (parallelism)
+- Independent error handling
+- Better resource utilization
+
+---
+
+### Pattern 3: Tool Caching
+
+Cache tool results to avoid redundant computations:
+
+```typescript
+class MCPCache {
+  private cache = new Map<string, { result: any; timestamp: number }>();
+  private ttl = 5 * 60 * 1000; // 5 minutes
+  
+  async call<T>(toolName: string, params: any): Promise<T> {
+    const cacheKey = `${toolName}:${JSON.stringify(params)}`;
+    
+    // Check cache
+    const cached = this.cache.get(cacheKey);
+    if (cached && Date.now() - cached.timestamp < this.ttl) {
+      return cached.result;
+    }
+    
+    // Execute tool
+    const result = await mcp.call(toolName, params);
+    
+    // Cache result
+    this.cache.set(cacheKey, { result, timestamp: Date.now() });
+    
+    return result;
+  }
+  
+  invalidate(toolName: string, params: any) {
+    const cacheKey = `${toolName}:${JSON.stringify(params)}`;
+    this.cache.delete(cacheKey);
+  }
+}
+```
+
+**Benefits**:
+- Improved performance (avoid redundant work)
+- Reduced API calls
+- Better user experience
+
+---
+
+### Pattern 4: Tool Fallbacks
+
+Provide fallback tools when primary tools fail:
+
+```typescript
+async function loadStyleGuide(language: string) {
+  // Try primary tool
+  const primaryResult = await mcp.call('load-style-guide', { language });
+  
+  if ('error' in primaryResult) {
+    // Fallback to legacy tool
+    console.warn(`Primary tool failed: ${primaryResult.error.message}, trying fallback`);
+    const fallbackResult = await mcp.call('load-style-guide-legacy', { language });
+    
+    if ('error' in fallbackResult) {
+      // Both failed, return error
+      return {
+        error: {
+          code: 'STYLE_GUIDE_UNAVAILABLE',
+          message: `No style guide available for ${language}`,
+          recoverable: false
+        }
+      };
+    }
+    
+    return fallbackResult;
+  }
+  
+  return primaryResult;
+}
+```
+
+**Benefits**:
+- Graceful degradation
+- Backward compatibility
+- Improved reliability
+
+---
+
+## MCP Best Practices
+
+### ✅ DO:
+
+- **Validate all inputs** using schemas before processing
+- **Use descriptive tool names** (kebab-case: `detect-language`, not `detectLanguage`)
+- **Provide clear error messages** with recovery suggestions
+- **Implement timeouts** for all tool calls
+- **Use authentication** for all MCP connections
+- **Log tool usage** for monitoring and debugging
+- **Version your tools** (semantic versioning)
+- **Document tool schemas** with examples
+- **Test tools independently** before integration
+- **Handle errors gracefully** with appropriate error codes
+
+### ❌ DON'T:
+
+- **Don't expose sensitive data** in error messages
+- **Don't trust user input** - always validate
+- **Don't hardcode credentials** - use environment variables
+- **Don't ignore timeouts** - always set reasonable limits
+- **Don't return stack traces** to clients (log internally)
+- **Don't make breaking changes** without versioning
+- **Don't skip authentication** in production
+- **Don't log sensitive data** (API keys, tokens, PII)
+
+---
+
+## MCP Configuration Examples
+
+### Example 1: Kimi CLI Configuration
+
+```json
+{
+  "mcpServers": {
+    "rules-server": {
+      "name": "Global AI Rules MCP Server",
+      "transport": {
+        "type": "http",
+        "url": "https://mcp-rules.example.com"
+      },
+      "auth": {
+        "type": "bearer_token",
+        "token": "${MCP_RULES_TOKEN}"
+      },
+      "tools": [
+        "detect-language",
+        "load-style-guide",
+        "setup-project-rules",
+        "validate-code"
+      ]
+    }
+  }
+}
+```
+
+### Example 2: Cursor Configuration
+
+```json
+{
+  "mcp": {
+    "servers": {
+      "rules": {
+        "url": "https://mcp-rules.example.com",
+        "auth": {
+          "type": "api_key",
+          "header": "X-API-Key",
+          "value": "${MCP_API_KEY}"
+        },
+        "timeout": 5000,
+        "retry": {
+          "attempts": 3,
+          "delay": 1000
+        }
+      }
+    }
+  }
+}
+```
+
+### Example 3: Claude Desktop Configuration
+
+```yaml
+mcp:
+  servers:
+    rules-server:
+      url: https://mcp-rules.example.com
+      auth:
+        type: mtls
+        cert: ${MCP_CLIENT_CERT}
+        key: ${MCP_CLIENT_KEY}
+      rate_limit:
+        requests_per_minute: 100
+      caching:
+        enabled: true
+        ttl: 300
+```
+
+---
+
+## MCP Monitoring and Observability
+
+### Metrics to Track
+
+Track these MCP metrics:
+
+- **Tool Call Count**: Number of calls per tool
+- **Success Rate**: Percentage of successful calls
+- **Error Rate**: Percentage of failed calls by error type
+- **Latency**: P50, P95, P99 response times
+- **Cache Hit Rate**: Percentage of cached responses
+- **Rate Limit Hits**: Number of rate-limited requests
+
+### Logging Best Practices
+
+```typescript
+// Structured logging for MCP calls
+function logMCPCall(toolName: string, params: any, result: any, duration: number) {
+  logger.info({
+    event: 'mcp_tool_call',
+    tool: toolName,
+    params: sanitize(params), // Remove sensitive data
+    success: !('error' in result),
+    duration_ms: duration,
+    timestamp: new Date().toISOString()
+  });
+}
+
+// Sanitize sensitive data
+function sanitize(params: any): any {
+  const sensitive = ['password', 'token', 'api_key', 'secret'];
+  const sanitized = { ...params };
+  for (const key of sensitive) {
+    if (key in sanitized) {
+      sanitized[key] = '[REDACTED]';
+    }
+  }
+  return sanitized;
+}
+```
+
+---
+
+## MCP Error Recovery
+
+### Retry Strategies
+
+```typescript
+// Exponential backoff retry
+async function callWithRetry<T>(
+  toolName: string,
+  params: any,
+  maxAttempts: number = 3
+): Promise<T> {
+  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
+    try {
+      const result = await mcp.call(toolName, params);
+      
+      if ('error' in result) {
+        // Check if error is retryable
+        if (result.error.recoverable && attempt < maxAttempts) {
+          const delay = Math.pow(2, attempt) * 1000; // Exponential backoff
+          console.log(`Attempt ${attempt} failed, retrying in ${delay}ms...`);
+          await sleep(delay);
+          continue;
+        }
+      }
+      
+      return result;
+    } catch (error) {
+      if (attempt === maxAttempts) throw error;
+      await sleep(Math.pow(2, attempt) * 1000);
+    }
+  }
+  
+  throw new Error('Max retry attempts exceeded');
+}
+```
+
+---
+
+## MCP Testing
+
+### Test Your Tools
+
+```typescript
+// Unit test for MCP tool
+test('detect-language tool returns correct result', async () => {
+  const result = await detectLanguage({ directory: './test-project' });
+  
+  expect(result).toHaveProperty('language');
+  expect(result).toHaveProperty('confidence');
+  expect(result.language).toBe('typescript');
+  expect(result.confidence).toBeGreaterThan(0.8);
+});
+
+test('detect-language tool handles errors', async () => {
+  const result = await detectLanguage({ directory: '/nonexistent' });
+  
+  expect(result).toHaveProperty('error');
+  expect(result.error.code).toBe('RESOURCE_NOT_FOUND');
+});
+```
+
+---
+
+## Tool-Specific Notes
+
+### For Kimi Users
+
+Kimi provides enhanced MCP support:
+- **Parallel Tool Calls**: Execute multiple tools simultaneously
+- **Subagent Tool Execution**: Spawn subagents for complex tool workflows
+- **SetTodoList Integration**: Track tool-based tasks automatically
+- **Tool Caching**: Automatic caching of tool results
+
+**Kimi-Specific Features**:
+```bash
+# Execute tools in parallel
+kimi mcp-call --parallel detect-language load-style-guide setup-project
+
+# Use subagents for complex workflows
+kimi mcp-workflow --file workflow.json --use-subagents
+
# Cache tool results
+kimi mcp-cache --ttl 300 detect-language .
+```
+
+### For Cursor Users
+
+Cursor provides IDE-integrated MCP:
+- **Inline Tool Suggestions**: Real-time tool recommendations
+- **Tool Discovery**: Browse available tools in IDE
+- **Parameter Validation**: Inline validation of tool parameters
+- **Result Visualization**: Rich display of tool results
+
+### For Claude Users
+
+Claude excels at MCP workflow design:
+- **Tool Composition**: Design complex multi-tool workflows
+- **Error Handling**: Design robust error recovery patterns
+- **Best Practices**: MCP integration patterns
+- **Protocol Design**: Design new MCP tools and schemas
+
+### For Gemini Users
+
+Gemini provides comprehensive MCP guidance:
+- **Protocol Deep Dive**: Detailed MCP specification explanations
+- **Multi-Platform**: MCP patterns across different AI assistants
+- **Security**: MCP security best practices
+- **Advanced Patterns**: Complex MCP usage patterns
+
+---
+
+## Further Reading
+
+- **MCP Specification**: https://modelcontextprotocol.io
+- **Tool Selection**: `tool_selection.md` - Choosing the right tools
+- **Context Management**: `context_management.md` - Managing tool context
+- **Security**: See security sections in implementation plans
+
+---
+
+**Last Updated**: 2025-11-19
+**Maintained By**: Global AI Rules System
+**Status**: Active
+**Applies To**: All AI assistants (Cursor, Kimi, Claude, Gemini)
+**MCP Version**: 1.0.0
+**Specification**: https://modelcontextprotocol.io
+