import { z } from "zod";
import fg from "fast-glob";
import path from "path";
import { ToolHandler } from "./types.js";

const LanguageSchema = z.object({
  path: z.string().optional().describe("Path to the project root (default: current working directory)"),
});

interface LanguageScore {
  language: string;
  score: number;
  files: number;
  confidence: number;
}

const LANGUAGE_PATTERNS = {
  python: {
    config: ["pyproject.toml", "setup.py", "requirements.txt", "Pipfile"],
    source: ["**/*.py"],
    ignore: ["**/venv/**", "**/__pycache__/**"],
  },
  typescript: {
    config: ["tsconfig.json", "package.json"],
    source: ["**/*.ts", "**/*.tsx"],
    ignore: ["**/node_modules/**", "**/dist/**"],
  },
  javascript: {
    config: ["package.json", "jsconfig.json"],
    source: ["**/*.js", "**/*.jsx"],
    ignore: ["**/node_modules/**", "**/dist/**"],
  },
  rust: {
    config: ["Cargo.toml"],
    source: ["**/*.rs"],
    ignore: ["**/target/**"],
  },
  go: {
    config: ["go.mod"],
    source: ["**/*.go"],
    ignore: ["**/vendor/**"],
  },
  kotlin: {
    config: ["build.gradle.kts", "settings.gradle.kts"],
    source: ["**/*.kt"],
    ignore: ["**/build/**", "**/gradle/**"],
  },
  swift: {
    config: ["Package.swift"],
    source: ["**/*.swift"],
    ignore: ["**/.build/**"],
  },
};

async function detectLanguage(projectPath: string = process.cwd()): Promise<any> {
  const scores: LanguageScore[] = [];

  for (const [lang, patterns] of Object.entries(LANGUAGE_PATTERNS)) {
    let score = 0;
    let fileCount = 0;

    // Check config files (High confidence)
    const configFiles = await fg(patterns.config, {
      cwd: projectPath,
      ignore: patterns.ignore,
      deep: 2, // Config files usually at root or shallow depth
    });

    if (configFiles.length > 0) {
      score += 1.0; // Base score for existing config
      score += configFiles.length * 0.2; // Bonus for multiple config files
    }

    // Check source files (Medium confidence)
    // We limit to 50 files to avoid performance issues on large repos
    const sourceFiles = await fg(patterns.source, {
      cwd: projectPath,
      ignore: patterns.ignore,
    });

    if (sourceFiles.length > 0) {
      fileCount = sourceFiles.length;
      score += Math.min(fileCount * 0.1, 2.0); // Cap source file score
    }

    if (score > 0) {
      scores.push({
        language: lang,
        score,
        files: fileCount + configFiles.length,
        confidence: Math.min(score / 3.0, 1.0), // Normalize confidence
      });
    }
  }

  // Sort by score descending
  scores.sort((a, b) => b.score - a.score);

  if (scores.length === 0) {
    return {
      language: "unknown",
      confidence: 0,
      detected: [],
    };
  }

  const bestMatch = scores[0];

  return {
    language: bestMatch.language,
    confidence: parseFloat(bestMatch.confidence.toFixed(2)),
    detected: scores.map(s => ({
      language: s.language,
      confidence: parseFloat(s.confidence.toFixed(2)),
    })),
  };
}

export const detectLanguageTool: ToolHandler = {
  name: "detect_language",
  description: "Detects the primary programming language of the project",
  inputSchema: {
    type: "object",
    properties: {
      path: {
        type: "string",
        description: "Path to the project root (default: current working directory)",
      },
    },
  },
  handler: async (args: any) => {
    const { path: projectPath } = LanguageSchema.parse(args);
    return await detectLanguage(projectPath);
  },
};
