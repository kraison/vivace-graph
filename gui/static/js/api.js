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
  graphs: () => request("/api/graphs"),
  openGraph: (name) =>
    request(graphPath(name, "open"), { method: "POST" }),
  closeGraph: (name) =>
    request(graphPath(name, "close"), { method: "POST" }),
  stats: (name) => request(graphPath(name, "stats")),
};
