import { spawn } from "child_process";
import path from "path";

const serverPath = path.join(process.cwd(), "dist/index.js");
const server = spawn("node", [serverPath], {
  stdio: ["pipe", "pipe", "inherit"],
});

const request = {
  jsonrpc: "2.0",
  id: 1,
  method: "tools/list",
  params: {},
};

server.stdout.on("data", (data) => {
  console.log("Received:", data.toString());
  server.kill();
});

server.stdin.write(JSON.stringify(request) + "\n");

setTimeout(() => {
  console.error("Timeout waiting for response");
  server.kill();
}, 5000);
