import { Tool } from "@modelcontextprotocol/sdk/types.js";

export interface ToolHandler {
  name: string;
  description: string;
  inputSchema: any;
  handler: (args: any) => Promise<any>;
}

export const tools: ToolHandler[] = [];

export function registerTool(tool: ToolHandler) {
  tools.push(tool);
}
