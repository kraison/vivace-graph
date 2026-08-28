// Tiny JSON fetch wrapper (GH #270).  Every failure -- a network
// error or an {error, message} body -- rejects with an ApiError whose
// .message carries the server's text verbatim.

export class ApiError extends Error {
  constructor(message, { code = "network-error", status = 0 } = {}) {
    super(message);
    this.name = "ApiError";
    this.code = code;
    this.status = status;
  }
}

async function request(path, options = {}) {
  let response;
  try {
    response = await fetch(path, options);
  } catch (err) {
    throw new ApiError(`Cannot reach the server: ${err.message}`);
  }
  let body = null;
  try {
    body = await response.json();
  } catch {
    // Non-JSON body (e.g. the static handler's plain-text 404).
  }
  if (!response.ok) {
    const message = body && body.message
      ? body.message
      : `HTTP ${response.status} from ${path}`;
    const code = body && body.error ? body.error : "http-error";
    throw new ApiError(message, { code, status: response.status });
  }
  return body;
}

const graphPath = (name, tail) =>
  `/api/graphs/${encodeURIComponent(name)}/${tail}`;

export const api = {
  // What this server offers (GH #279): server-level, fetched once at
  // boot, before any graph exists to ask about.
  capabilities: () => request("/api/capabilities"),
  graphs: () => request("/api/graphs"),
  openGraph: (name) =>
    request(graphPath(name, "open"), { method: "POST" }),
  closeGraph: (name) =>
    request(graphPath(name, "close"), { method: "POST" }),
  stats: (name) => request(graphPath(name, "stats")),
  types: (name) => request(graphPath(name, "types")),
  // Query workbench (GH #278): the structured DSL rides as a JSON
  // document, so this is the one POST with a body.
  query: (name, dsl) =>
    request(graphPath(name, "query"), {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(dsl),
    }),
  // Free-text Prolog (GH #279).  Present on every build; the server
  // refuses it with a 403 unless it was started with :ALLOW-PROLOG.
  prolog: (name, text, limit) =>
    request(graphPath(name, "prolog"), {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ query: text, limit }),
    }),
  // Explorer reads (GH #271).  Limits ride as query parameters; the
  // server applies its own default when omitted.
  nodes: (name, type, limit) =>
    request(graphPath(name,
                      `nodes?type=${encodeURIComponent(type)}` +
                      (limit ? `&limit=${limit}` : ""))),
  node: (name, id) =>
    request(graphPath(name, `node/${encodeURIComponent(id)}`)),
  neighborhood: (name, id, limit) =>
    request(graphPath(name,
                      `neighborhood/${encodeURIComponent(id)}` +
                      (limit ? `?limit=${limit}` : ""))),
};
