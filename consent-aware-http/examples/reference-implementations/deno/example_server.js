#!/usr/bin/env -S deno run --allow-read --allow-net
// SPDX-License-Identifier: MIT OR GPL-3.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// Example Deno server demonstrating AIBDP + HTTP 430 middleware (port of
// the former Python/Flask example; Python is banned estate-wide).
//
// Usage:
//   deno run --allow-read --allow-net example_server.js
//
// Test with:
//   curl http://localhost:5000/
//   curl http://localhost:5000/article -H "User-Agent: GPTBot/1.0"
//   curl http://localhost:5000/.well-known/aibdp.json

import {
  aibdpRequired,
  AIBDPMiddleware,
  serveManifest,
} from "./aibdp_middleware.js";

const MANIFEST = "example-aibdp.json";

const middleware = new AIBDPMiddleware({
  manifestPath: MANIFEST,
  enforceForAll: false, // only enforce for detected AI systems
  onViolation: (req, policy, purpose) => {
    const url = new URL(req.url);
    console.log(
      `[AIBDP] Blocked: ${req.method} ${url.pathname}\n` +
        `  User-Agent: ${req.headers.get("User-Agent")}\n` +
        `  Purpose: ${purpose}\n` +
        `  Policy: ${policy.status}`,
    );
  },
});

const manifestHandler = serveManifest(MANIFEST);

function html(body) {
  return new Response(body, { headers: { "Content-Type": "text/html" } });
}

const indexPage = html(`
    <html>
      <head>
        <title>Consent-Aware HTTP Example (Deno)</title>
      </head>
      <body>
        <h1>Welcome to Consent-Aware HTTP</h1>
        <p>This Deno server implements HTTP 430 + AIBDP.</p>

        <h2>Try these requests:</h2>
        <ul>
          <li><code>curl http://localhost:5000/</code> - Normal access (allowed)</li>
          <li><code>curl http://localhost:5000/article -H "User-Agent: GPTBot/1.0"</code> - AI bot (may be blocked)</li>
          <li><code>curl http://localhost:5000/.well-known/aibdp.json</code> - View AIBDP manifest</li>
        </ul>

        <h2>Resources:</h2>
        <ul>
          <li><a href="/article">Article</a> - Protected content</li>
          <li><a href="/public">Public content</a> - Allowed for all</li>
          <li><a href="/protected">Protected route (decorator)</a> - Using aibdpRequired</li>
          <li><a href="/.well-known/aibdp.json">AIBDP Manifest</a></li>
        </ul>
      </body>
    </html>
    `);

const articlePage = () =>
  html(`
    <html>
      <head>
        <title>Protected Article</title>
      </head>
      <body>
        <h1>Protected Article</h1>
        <p>This content has AI usage boundaries declared via AIBDP.</p>
        <p><strong>Training:</strong> Conditional (requires attribution)</p>
        <p><strong>Generation:</strong> Refused</p>
        <p><strong>Indexing:</strong> Allowed</p>
      </body>
    </html>
    `);

const publicPage = html(`
    <html>
      <head>
        <title>Public Content</title>
      </head>
      <body>
        <h1>Public Content</h1>
        <p>This content is available for all purposes, including AI training.</p>
      </body>
    </html>
    `);

const protectedHandler = aibdpRequired(
  () =>
    html(`
    <html>
      <head>
        <title>Protected Route</title>
      </head>
      <body>
        <h1>Protected Route (Decorator)</h1>
        <p>This route uses the aibdpRequired wrapper.</p>
        <p>AI training is not permitted on this content.</p>
      </body>
    </html>
    `),
  { manifestPath: MANIFEST, purpose: "training" },
);

function route(req) {
  const { pathname } = new URL(req.url);
  switch (pathname) {
    case "/":
      return indexPage;
    case "/article":
      return articlePage();
    case "/public":
      return publicPage;
    case "/protected":
      return protectedHandler(req);
    case "/health":
      return Response.json({
        status: "healthy",
        aibdp_enabled: true,
        http_430_enabled: true,
      });
    case "/.well-known/aibdp.json":
      return manifestHandler();
    default:
      return new Response("Not Found", { status: 404 });
  }
}

const handler = middleware.wrap(route);

console.log("🚀 Consent-Aware HTTP server (Deno) running on http://localhost:5000");
console.log("📄 AIBDP manifest: http://localhost:5000/.well-known/aibdp.json");
console.log("🛡️  HTTP 430 enforcement: ENABLED");
console.log("");
console.log("Try these commands:");
console.log("  curl http://localhost:5000/");
console.log('  curl http://localhost:5000/article -H "User-Agent: GPTBot/1.0"');
console.log("  curl http://localhost:5000/.well-known/aibdp.json");
console.log("");

Deno.serve({ port: 5000 }, handler);
