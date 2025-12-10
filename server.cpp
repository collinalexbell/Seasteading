#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include <atomic>
#include <cerrno>
#include <csignal>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <thread>
#include <vector>
#include <chrono>
#include <cmath>
#include <unordered_map>
#include <mutex>
#include <algorithm>
#include <filesystem>
#include <nlohmann/json.hpp>

#include <openssl/sha.h>
#include <openssl/evp.h>
#include <openssl/buffer.h>

#define ENET_IMPLEMENTATION
#include "enet.h"

// Minimal HTTP server to persist state (tiles, cards, drawings) and registrations.
// Endpoints:
//   GET/POST /tiles      -> tiles.json (positions)
//   GET/POST /cards      -> cards.json (card definitions)
//   GET/POST /draw       -> draw.json (strokes/layers)
//   GET/POST /state      -> state.json (optional umbrella)
//   POST /register       -> append registration entry (GPG key + signed blob)
// CORS: Access-Control-Allow-Origin: *
// Storage files live in working directory.

struct ShipState {
    double x{0};
    double y{0};
    double heading{0};
    double speed{0};
    bool aground{false};
};
struct ControlInput {
    double heading{0};
    double speed{0};
    bool has{false};
};

static const int kDefaultPort = 8081;
static const char* kDefaultFile = "tiles.json";
static volatile sig_atomic_t g_stop = 0;
static const char* kRunnerSock = "/tmp/ship_runner.sock";
static const char* kGameCodeDir = "game_code";
static std::string g_refreshToken;

extern "C" void __gcov_flush() __attribute__((weak));

int GetPort() {
    const char* env = std::getenv("PORT");
    if (env && *env) {
        int p = std::atoi(env);
        if (p > 0 && p < 65536) return p;
    }
    return kDefaultPort;
}

std::string GetFilePath() {
    const char* env = std::getenv("TILES_FILE");
    if (env && *env) return std::string(env);
    return std::string(kDefaultFile);
}

void HandleSignal(int) {
    g_stop = 1;
}

std::string readFile(const std::string& path) {
    std::ifstream in(path, std::ios::in | std::ios::binary);
    if (!in) return "";
    std::ostringstream ss;
    ss << in.rdbuf();
    return ss.str();
}

bool writeFile(const std::string& path, const std::string& data) {
    std::ofstream out(path, std::ios::out | std::ios::trunc | std::ios::binary);
    if (!out) return false;
    out << data;
    return true;
}

std::string defaultJson() {
    // Fallback positions.
    return R"({"tiles":[{"id":"Realm","x":-260,"y":80},{"id":"Hardware / Events","x":-60,"y":-20},{"id":"Floorplan Tile","x":180,"y":60},{"id":"Lake Map","x":40,"y":-140}]})";
}

std::string clientIp(const sockaddr_in& addr) {
    char buf[INET_ADDRSTRLEN];
    if (inet_ntop(AF_INET, &addr.sin_addr, buf, sizeof(buf))) {
        return std::string(buf);
    }
    return "unknown";
}

std::string peerIp(const ENetAddress& addr) {
    char buf[256];
    if (enet_address_get_host_ip(&addr, buf, sizeof(buf)) == 0) return std::string(buf);
    return "unknown";
}

// --------------------
// WebSocket helpers
// --------------------
std::string base64Encode(const unsigned char* input, size_t len) {
    BIO* bmem = BIO_new(BIO_s_mem());
    BIO* b64 = BIO_new(BIO_f_base64());
    BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
    b64 = BIO_push(b64, bmem);
    BIO_write(b64, input, static_cast<int>(len));
    BIO_flush(b64);
    BUF_MEM* bptr;
    BIO_get_mem_ptr(b64, &bptr);
    std::string out(bptr->data, bptr->length);
    BIO_free_all(b64);
    return out;
}

std::string websocketAcceptKey(const std::string& clientKey) {
    static const std::string guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
    std::string src = clientKey + guid;
    unsigned char hash[SHA_DIGEST_LENGTH];
    SHA1(reinterpret_cast<const unsigned char*>(src.data()), src.size(), hash);
    return base64Encode(hash, SHA_DIGEST_LENGTH);
}

bool websocketHandshake(int clientFd, const std::string& req) {
    auto keyPos = req.find("Sec-WebSocket-Key:");
    if (keyPos == std::string::npos) return false;
    size_t start = keyPos + strlen("Sec-WebSocket-Key:");
    while (start < req.size() && (req[start] == ' ' || req[start] == '\t')) start++;
    size_t end = req.find("\r\n", start);
    std::string key = req.substr(start, end - start);
    std::string accept = websocketAcceptKey(key);
    std::ostringstream resp;
    resp << "HTTP/1.1 101 Switching Protocols\r\n"
         << "Upgrade: websocket\r\n"
         << "Connection: Upgrade\r\n"
         << "Sec-WebSocket-Accept: " << accept << "\r\n\r\n";
    auto s = resp.str();
    send(clientFd, s.data(), s.size(), 0);
    return true;
}

std::string wsFrameText(const std::string& payload) {
    std::string frame;
    frame.push_back(static_cast<char>(0x81)); // FIN + text
    if (payload.size() <= 125) {
        frame.push_back(static_cast<char>(payload.size()));
    } else {
        frame.push_back(126);
        uint16_t len = htons(static_cast<uint16_t>(payload.size()));
        frame.append(reinterpret_cast<char*>(&len), reinterpret_cast<char*>(&len) + 2);
    }
    frame.append(payload);
    return frame;
}

bool wsReadFrame(int fd, std::string& outPayload) {
    unsigned char hdr[2];
    ssize_t n = recv(fd, hdr, 2, MSG_WAITALL);
    if (n != 2) return false;
    bool masked = (hdr[1] & 0x80) != 0;
    size_t len = hdr[1] & 0x7F;
    if (len == 126) {
        unsigned char ext[2];
        if (recv(fd, ext, 2, MSG_WAITALL) != 2) return false;
        len = (ext[0] << 8) | ext[1];
    } else if (len == 127) {
        // not expected for this small protocol
        return false;
    }
    unsigned char mask[4] = {0,0,0,0};
    if (masked) {
        if (recv(fd, mask, 4, MSG_WAITALL) != 4) return false;
    }
    std::vector<char> payload(len);
    size_t got = 0;
    while (got < len) {
        ssize_t r = recv(fd, payload.data() + got, len - got, 0);
        if (r <= 0) return false;
        got += r;
    }
    if (masked) {
        for (size_t i = 0; i < len; ++i) payload[i] ^= mask[i % 4];
    }
    outPayload.assign(payload.begin(), payload.end());
    return true;
}

// --------------------
// Geometry helpers (lake bounds)
// --------------------
static const std::vector<std::pair<double,double>> kLakePoints = {
    {0.04, 0.55}, {0.12, 0.40}, {0.24, 0.32}, {0.36, 0.27},
    {0.50, 0.23}, {0.66, 0.24}, {0.82, 0.30}, {0.93, 0.38},
    {0.88, 0.44}, {0.78, 0.46}, {0.70, 0.50}, {0.60, 0.54},
    {0.50, 0.56}, {0.40, 0.53}, {0.32, 0.58}, {0.22, 0.63},
    {0.12, 0.67}, {0.06, 0.64}
};

struct LakePoly {
    std::vector<std::pair<double,double>> pts;
    double minX{0}, maxX{0}, minY{0}, maxY{0};
};

LakePoly BuildLakePoly() {
    const double w = 180000.0;
    const double h = 110000.0;
    LakePoly lp;
    for (auto [x, y] : kLakePoints) {
        double px = x * w - w / 2;
        double py = y * h - h / 2;
        lp.pts.emplace_back(-py, px); // rotate 90°
    }
    lp.minX = lp.maxX = lp.pts[0].first;
    lp.minY = lp.maxY = lp.pts[0].second;
    for (auto [x, y] : lp.pts) {
        lp.minX = std::min(lp.minX, x);
        lp.maxX = std::max(lp.maxX, x);
        lp.minY = std::min(lp.minY, y);
        lp.maxY = std::max(lp.maxY, y);
    }
    return lp;
}

bool PointInLake(const LakePoly& lp, double x, double y) {
    bool inside = false;
    size_t n = lp.pts.size();
    for (size_t i = 0, j = n - 1; i < n; j = i++) {
        double xi = lp.pts[i].first, yi = lp.pts[i].second;
        double xj = lp.pts[j].first, yj = lp.pts[j].second;
        bool intersect = ((yi > y) != (yj > y)) &&
            (x < (xj - xi) * (y - yi) / (yj - yi + 1e-9) + xi);
        if (intersect) inside = !inside;
    }
    return inside;
}

bool CheckAground(const LakePoly& lp, ShipState& ship, double margin) {
    bool clamped = false;
    if (ship.x < lp.minX - margin) { ship.x = lp.minX - margin; clamped = true; }
    if (ship.x > lp.maxX + margin) { ship.x = lp.maxX + margin; clamped = true; }
    if (ship.y < lp.minY - margin) { ship.y = lp.minY - margin; clamped = true; }
    if (ship.y > lp.maxY + margin) { ship.y = lp.maxY + margin; clamped = true; }
    bool outside = !PointInLake(lp, ship.x, ship.y);
    if (outside || clamped) {
        ship.aground = true;
        ship.speed = 0;
        return true;
    }
    ship.aground = false;
    return false;
}

// Global MMO state
static std::unordered_map<std::string, ShipState> g_ships = {
    {"main", {0,0,0,0,false}},
    {"player", {20000,0,0,0,false}}
};
static std::unordered_map<std::string, ControlInput> g_controls;
static std::unordered_map<std::string, std::unordered_map<std::string, bool>> g_keyStates;
static std::mutex g_shipMutex;
static LakePoly g_lakePoly = BuildLakePoly();
static std::mutex g_inputMutex;
static std::unordered_map<std::string, bool> g_keyState;
static bool g_debugLogs = true;
static bool g_forceIdle = false;
static bool g_traceInput = true;

// --------------------
// Runner (node) IPC over UNIX socket
// --------------------

int ConnectRunner() {
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) return -1;
    sockaddr_un addr{};
    addr.sun_family = AF_UNIX;
    std::strncpy(addr.sun_path, kRunnerSock, sizeof(addr.sun_path) - 1);
    if (connect(fd, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)) < 0) {
        close(fd);
        return -1;
    }
    return fd;
}

bool RunnerStep(const std::string& shipId,
                const ShipState& ship,
                const std::unordered_map<std::string, bool>& keys,
                double dt,
                ControlInput& out,
                ShipState& outShip) {
    int fd = ConnectRunner();
    if (fd < 0) return false;
    std::ostringstream req;
    req << R"({"cmd":"step","shipId":")" << shipId << R"(","ship":{)"
        << "\"x\":" << ship.x << ",\"y\":" << ship.y
        << ",\"heading\":" << ship.heading << ",\"speed\":" << ship.speed
        << "},\"dt\":" << dt << ",\"input\":{\"keys\":{";
    bool first = true;
    for (const auto& kv : keys) {
        if (!first) req << ",";
        first = false;
        req << "\"" << kv.first << "\":" << (kv.second ? "true" : "false");
    }
    req << "}}}\n";
    auto s = req.str();
    if (g_debugLogs) {
        std::cerr << "[runner step] ship=" << shipId << " keys=" << keys.size()
                  << " pos=(" << ship.x << "," << ship.y << ") dt=" << dt << "\n";
    }
    if (send(fd, s.data(), s.size(), 0) <= 0) { close(fd); return false; }

    std::string resp;
    char buf[1024];
    while (true) {
        ssize_t r = recv(fd, buf, sizeof(buf), 0);
        if (r <= 0) break;
        resp.append(buf, buf + r);
        auto pos = resp.find('\n');
        if (pos != std::string::npos) {
            resp = resp.substr(0, pos);
            break;
        }
    }
    close(fd);
    if (resp.empty()) return false;

    auto findNum = [&](const char* key, size_t startPos = 0) -> double {
        auto pos = resp.find(key, startPos);
        if (pos == std::string::npos) return 0.0;
        pos = resp.find(':', pos);
        if (pos == std::string::npos) return 0.0;
        return std::atof(resp.c_str() + pos + 1);
    };

    size_t shipPos = resp.find("\"ship\"");
    size_t controlPos = resp.find("\"control\"");

    outShip.x = findNum("\"x\"", shipPos);
    outShip.y = findNum("\"y\"", shipPos);
    outShip.heading = findNum("\"heading\"", shipPos);
    outShip.speed = findNum("\"speed\"", shipPos);

    out.heading = findNum("\"heading\"", controlPos);
    out.speed = findNum("\"speed\"", controlPos);
    out.has = true;
    if (g_debugLogs) {
        std::cerr << "[runner step resp] ship=" << shipId
                  << " outPos=(" << outShip.x << "," << outShip.y << ")"
                  << " ctrl=(" << out.heading << "," << out.speed << ")\n";
    }
    return true;
}

bool RunnerSetProgram(const std::string& shipId, const std::string& payload) {
    int fd = ConnectRunner();
    if (fd < 0) return false;
    std::string msg;
    std::ostringstream req;
    req << R"({"cmd":"set_program","shipId":")" << shipId << R"(","code":")";
    for (char c : payload) {
        if (c == '"' || c == '\\') req << '\\';
        req << c;
    }
    req << "\"}\n";
    msg = req.str();
    if (send(fd, msg.data(), msg.size(), 0) <= 0) { close(fd); return false; }
    close(fd);
    return true;
}

bool EnsureGameCodeDir() {
    std::error_code ec;
    if (!std::filesystem::exists(kGameCodeDir)) {
        return std::filesystem::create_directories(kGameCodeDir, ec) || std::filesystem::exists(kGameCodeDir);
    }
    return true;
}

std::string ExtractJsonString(const std::string& body, const std::string& key) {
    try {
        auto parsed = nlohmann::json::parse(body);
        if (parsed.contains(key) && parsed[key].is_string()) return parsed[key].get<std::string>();
    } catch (...) {}
    return "";
}

void LoadGameCode() {
    if (!EnsureGameCodeDir()) return;
    for (auto& entry : std::filesystem::directory_iterator(kGameCodeDir)) {
        if (!entry.is_regular_file()) continue;
        auto path = entry.path();
        auto shipId = path.stem().string();
        std::string code = readFile(path.string());
        if (!code.empty()) {
            RunnerSetProgram(shipId, code);
        }
    }
}

// --------------------
// Global WebSocket client registry
// --------------------
#include <mutex>
#include <set>
static std::mutex g_wsMutex;
static std::set<int> g_wsClients;

void BroadcastShipState() {
    std::lock_guard<std::mutex> lock(g_wsMutex);
    std::vector<int> dead;
    for (int fd : g_wsClients) {
        // Build per-client payload: always include main, include player's if known
        std::ostringstream resp;
        {
            std::lock_guard<std::mutex> shipLock(g_shipMutex);
            auto mainIt = g_ships.find("main");
            if (mainIt != g_ships.end()) {
                const auto& s = mainIt->second;
                resp << "{\"ship\":{\"id\":\"main\",\"x\":" << s.x << ",\"y\":" << s.y
                     << ",\"heading\":" << s.heading << ",\"speed\":" << s.speed
                     << ",\"aground\":" << (s.aground ? "true" : "false") << "}";
            } else {
                resp << "{\"ship\":null";
            }
            // player ship if tracked per fd
            // we do not store fd->shipId; fall back to session "player"
            auto pit = g_ships.find("player");
            if (pit != g_ships.end()) {
                const auto& p = pit->second;
                resp << ",\"player\":{\"id\":\"player\",\"x\":" << p.x << ",\"y\":" << p.y
                     << ",\"heading\":" << p.heading << ",\"speed\":" << p.speed
                     << ",\"aground\":" << (p.aground ? "true" : "false") << "}";
            }
            resp << ",\"refresh\":\"" << g_refreshToken << "\"}";
        }
        auto frame = wsFrameText(resp.str());
        ssize_t n = send(fd, frame.data(), frame.size(), 0);
        if (n <= 0) dead.push_back(fd);
    }
    for (int fd : dead) g_wsClients.erase(fd);
}

void PhysicsLoop() {
    const double margin = 12000.0 * 0.8;
    auto last = std::chrono::steady_clock::now();
    while (!g_stop) {
        auto now = std::chrono::steady_clock::now();
        double dt = std::chrono::duration<double>(now - last).count();
        if (dt > 0.2) dt = 0.2;
        last = now;

        {
            std::lock_guard<std::mutex> lock(g_shipMutex);
            for (auto& kv : g_ships) {
                const std::string& shipId = kv.first;
                ShipState& ship = kv.second;

                // keys for this ship
                std::unordered_map<std::string, bool> keys;
                auto itKeys = g_keyStates.find(shipId);
                if (itKeys != g_keyStates.end()) keys = itKeys->second;

                ControlInput in{};
                ShipState updated = ship;
                auto itCtrl = g_controls.find(shipId);
                ControlInput fallback = (itCtrl != g_controls.end()) ? itCtrl->second : ControlInput{};

                if (g_forceIdle) {
                    updated.speed = 0;
                } else if (RunnerStep(shipId, ship, keys, dt, in, updated) && in.has) {
                    g_controls[shipId] = in;
                } else if (fallback.has) {
                    updated.heading = fallback.heading;
                    updated.speed = fallback.speed;
                }
                updated.x += std::cos(updated.heading) * updated.speed * dt;
                updated.y += std::sin(updated.heading) * updated.speed * dt;
                CheckAground(g_lakePoly, updated, margin);
                ship = updated;
            }
        }
        BroadcastShipState();
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
}

std::string httpResponse(const std::string& body, const std::string& status = "200 OK", const std::string& contentType = "application/json") {
    std::ostringstream ss;
    ss << "HTTP/1.1 " << status << "\r\n";
    ss << "Access-Control-Allow-Origin: *\r\n";
    ss << "Access-Control-Allow-Methods: GET, POST, OPTIONS\r\n";
    ss << "Access-Control-Allow-Headers: Content-Type, X-Session\r\n";
    ss << "Content-Type: " << contentType << "\r\n";
    ss << "Content-Length: " << body.size() << "\r\n";
    ss << "Connection: close\r\n\r\n";
    ss << body;
    return ss.str();
}

void handleClient(int clientFd, const sockaddr_in& peer) {
    constexpr size_t bufSize = 65536;
    std::vector<char> buf(bufSize);
    ssize_t n = recv(clientFd, buf.data(), buf.size(), 0);
    if (n <= 0) return;
    std::string req(buf.data(), static_cast<size_t>(n));

    // Very minimal parsing.
    auto posMethodEnd = req.find(' ');
    if (posMethodEnd == std::string::npos) return;
    std::string method = req.substr(0, posMethodEnd);
    auto posPathEnd = req.find(' ', posMethodEnd + 1);
    if (posPathEnd == std::string::npos) return;
    std::string path = req.substr(posMethodEnd + 1, posPathEnd - posMethodEnd - 1);

    // Handle WebSocket upgrade for realtime ship checks
    if (req.find("Upgrade: websocket") != std::string::npos && path == "/ws") {
        if (!websocketHandshake(clientFd, req)) return;
        {
            std::lock_guard<std::mutex> lock(g_wsMutex);
            g_wsClients.insert(clientFd);
        }
        // send refresh token immediately
        {
            std::ostringstream resp;
            resp << "{\"refresh\":\"" << g_refreshToken << "\"}";
            auto frame = wsFrameText(resp.str());
            send(clientFd, frame.data(), frame.size(), 0);
        }
        std::string payload;
        while (!g_stop && wsReadFrame(clientFd, payload)) {
            // Expect control message: {"control":{"heading":..., "speed":...}}
            auto posCtrl = payload.find("\"control\"");
            auto posInput = payload.find("\"input\"");
            std::string shipId = "player"; // default player ship
            if (posCtrl != std::string::npos) {
                auto findNum = [&](const char* key) -> double {
                    auto pos = payload.find(key, posCtrl);
                    if (pos == std::string::npos) return 0.0;
                    pos = payload.find(':', pos);
                    if (pos == std::string::npos) return 0.0;
                    return std::atof(payload.c_str() + pos + 1);
                };
                ControlInput ci;
                ci.heading = findNum("\"heading\"");
                ci.speed = findNum("\"speed\"");
                ci.has = true;
                g_controls[shipId] = ci;
            }
            if (posInput != std::string::npos) {
                // Parse input keys map: "keys":{"KeyW":true,...}
                auto posKeys = payload.find("\"keys\"", posInput);
                if (posKeys != std::string::npos) {
                    auto posBrace = payload.find('{', posKeys);
                    auto posEnd = payload.find('}', posKeys);
                        if (posBrace != std::string::npos && posEnd != std::string::npos && posEnd > posBrace) {
                        std::lock_guard<std::mutex> lock(g_inputMutex);
                        std::string keysStr = payload.substr(posBrace + 1, posEnd - posBrace - 1);
                        std::istringstream iss(keysStr);
                        std::string kv;
                        while (std::getline(iss, kv, ',')) {
                            auto colon = kv.find(':');
                            if (colon == std::string::npos) continue;
                            std::string key = kv.substr(0, colon);
                            std::string val = kv.substr(colon + 1);
                            key.erase(std::remove(key.begin(), key.end(), '"'), key.end());
                            bool pressed = (val.find("true") != std::string::npos || val.find("1") != std::string::npos);
                            g_keyStates[shipId][key] = pressed;
                        }
                        if (g_traceInput) {
                            std::cerr << "[input] ship=" << shipId << " keys=" << g_keyStates[shipId].size() << "\n";
                        }
                    }
                }
            }
        }
        std::lock_guard<std::mutex> lock(g_wsMutex);
        g_wsClients.erase(clientFd);
        return;
    }

    // Handle OPTIONS
    if (method == "OPTIONS") {
        std::string resp = httpResponse("", "204 No Content");
        send(clientFd, resp.data(), resp.size(), 0);
        return;
    }

    // Determine body
    std::string body;
    auto posCL = req.find("Content-Length:");
    if (posCL != std::string::npos) {
        size_t start = posCL + strlen("Content-Length:");
        while (start < req.size() && (req[start] == ' ' || req[start] == '\t')) start++;
        size_t end = req.find("\r\n", start);
        size_t len = static_cast<size_t>(std::stoul(req.substr(start, end - start)));
        auto posBody = req.find("\r\n\r\n");
        if (posBody != std::string::npos) {
            size_t bodyStart = posBody + 4;
            if (bodyStart + len <= req.size()) {
                body = req.substr(bodyStart, len);
            }
        }
    }

    auto serveFile = [&](const std::string& filePath) {
        std::string data = readFile(filePath);
        if (data.empty()) {
            if (filePath == GetFilePath()) data = defaultJson();
            else if (filePath == "state.json") data = "{}";
        }
        std::string resp = httpResponse(data);
        send(clientFd, resp.data(), resp.size(), 0);
    };

    auto writeBody = [&](const std::string& filePath) {
        if (body.empty()) {
            std::string resp = httpResponse(R"({"error":"empty body"})", "400 Bad Request");
            send(clientFd, resp.data(), resp.size(), 0);
            return;
        }
        if (!writeFile(filePath, body)) {
            std::string resp = httpResponse(R"({"error":"failed to write file"})", "500 Internal Server Error");
            send(clientFd, resp.data(), resp.size(), 0);
            return;
        }
        std::string resp = httpResponse(R"({"ok":true})");
        send(clientFd, resp.data(), resp.size(), 0);
    };

    if (method == "GET" && path == "/tiles") { serveFile(GetFilePath()); return; }
    if (method == "POST" && path == "/tiles") { writeBody(GetFilePath()); return; }
    if (method == "GET" && path == "/cards") { serveFile("cards.json"); return; }
    if (method == "POST" && path == "/cards") { writeBody("cards.json"); return; }
    if (method == "GET" && path == "/draw") { serveFile("draw.json"); return; }
    if (method == "POST" && path == "/draw") { writeBody("draw.json"); return; }
    if (method == "GET" && path == "/state") { serveFile("state.json"); return; }
    if (method == "POST" && path == "/state") { writeBody("state.json"); return; }
    if (method == "POST" && path == "/program") {
        // forward ship program to runner
        if (body.empty()) {
            std::string resp = httpResponse(R"({"error":"empty body"})", "400 Bad Request");
            send(clientFd, resp.data(), resp.size(), 0);
            return;
        }
        std::string shipId = ExtractJsonString(body, "shipId");
        if (shipId.empty()) shipId = "player";
        std::string codeStr = ExtractJsonString(body, "code");
        if (codeStr.empty()) codeStr = body;
        EnsureGameCodeDir();
        writeFile(std::string(kGameCodeDir) + "/" + shipId + ".js", codeStr);
        bool ok = RunnerSetProgram(shipId, codeStr);
        if (g_debugLogs) {
            std::cerr << "[program] ship=" << shipId << " len=" << codeStr.size()
                      << " ok=" << ok << "\n";
        }
        std::string resp = ok ? httpResponse(R"({"ok":true})")
                              : httpResponse(R"({"error":"runner unavailable"})", "500 Internal Server Error");
        send(clientFd, resp.data(), resp.size(), 0);
        return;
    }

    if (method == "POST" && path == "/register") {
        std::ostringstream line;
        line << "{";
        line << "\"ip\":\"" << clientIp(peer) << "\",";
        // naive session header parse
        auto posSess = req.find("X-Session:");
        if (posSess != std::string::npos) {
            size_t start = posSess + strlen("X-Session:");
            while (start < req.size() && (req[start] == ' ' || req[start] == '\t')) start++;
            size_t end = req.find("\r\n", start);
            line << "\"session\":\"" << req.substr(start, end - start) << "\",";
        }
        line << "\"body\":" << (body.empty() ? "\"\"" : body) << "}\n";
        std::ofstream out("register.log", std::ios::app);
        out << line.str();
        std::string resp = httpResponse(R"({"ok":true})");
        send(clientFd, resp.data(), resp.size(), 0);
        return;
    }

    std::string resp = httpResponse(R"({"error":"not found"})", "404 Not Found");
    send(clientFd, resp.data(), resp.size(), 0);
}

struct UdpRequest {
    std::string method;
    std::string path;
    std::string body;
};

struct UdpResponse {
    int status = 400;
    std::string body;
};

bool parseUdpRequest(const std::string& data, UdpRequest& req) {
    auto nl = data.find('\n');
    std::string header = (nl == std::string::npos) ? data : data.substr(0, nl);
    req.body = (nl == std::string::npos) ? "" : data.substr(nl + 1);
    auto space = header.find(' ');
    if (space == std::string::npos) return false;
    req.method = header.substr(0, space);
    req.path = header.substr(space + 1);
    if (!req.path.empty() && req.path.back() == '\r') req.path.pop_back();
    return true;
}

UdpResponse handleUdp(const UdpRequest& req, const ENetEvent& event) {
    UdpResponse res;
    auto serveFile = [&](const std::string& filePath) {
        std::string data = readFile(filePath);
        if (data.empty() && filePath == GetFilePath()) data = defaultJson();
        res.status = 200;
        res.body = data;
    };
    auto writeBody = [&](const std::string& filePath) {
        if (req.body.empty()) {
            res.status = 400;
            res.body = R"({"error":"empty body"})";
            return;
        }
        if (!writeFile(filePath, req.body)) {
            res.status = 500;
            res.body = R"({"error":"failed to write file"})";
            return;
        }
        res.status = 200;
        res.body = R"({"ok":true})";
    };

    if (req.method == "GET" && req.path == "/tiles") { serveFile(GetFilePath()); return res; }
    if (req.method == "POST" && req.path == "/tiles") { writeBody(GetFilePath()); return res; }
    if (req.method == "GET" && req.path == "/cards") { serveFile("cards.json"); return res; }
    if (req.method == "POST" && req.path == "/cards") { writeBody("cards.json"); return res; }
    if (req.method == "GET" && req.path == "/draw") { serveFile("draw.json"); return res; }
    if (req.method == "POST" && req.path == "/draw") { writeBody("draw.json"); return res; }
    if (req.method == "GET" && req.path == "/state") { serveFile("state.json"); return res; }
    if (req.method == "POST" && req.path == "/state") { writeBody("state.json"); return res; }

    if (req.method == "POST" && req.path == "/register") {
        std::ofstream out("register_udp.log", std::ios::app);
        out << "{\"ip\":\"" << peerIp(event.peer->address) << "\",\"body\":"
            << (req.body.empty() ? "\"\"" : req.body) << "}\n";
        res.status = 200;
        res.body = R"({"ok":true})";
        return res;
    }

    res.status = 404;
    res.body = R"({"error":"not found"})";
    return res;
}

void sendUdpResponse(ENetPeer* peer, const UdpResponse& res) {
    std::ostringstream ss;
    ss << "STATUS " << res.status << "\n" << res.body;
    auto str = ss.str();
    ENetPacket* packet = enet_packet_create(str.data(), str.size(), ENET_PACKET_FLAG_RELIABLE);
    enet_peer_send(peer, 0, packet);
    enet_host_flush(peer->host);
}

void runUdpServer(int port) {
    if (enet_initialize() != 0) {
        std::cerr << "Failed to initialize ENet\n";
        return;
    }
    ENetAddress address;
    address.host = ENET_HOST_ANY;
    address.port = port;
    ENetHost* host = enet_host_create(&address, 32, 2, 0, 0);
    if (host == nullptr) {
        std::cerr << "Failed to create ENet host on port " << port << "\n";
        enet_deinitialize();
        return;
    }
    std::cout << "UDP tile server listening on 0.0.0.0:" << port << " file=" << GetFilePath() << "\n";
    ENetEvent event;
    while (!g_stop) {
        while (enet_host_service(host, &event, 50) > 0) {
            switch (event.type) {
                case ENET_EVENT_TYPE_CONNECT:
                    enet_host_flush(host);
                    break;
                case ENET_EVENT_TYPE_RECEIVE: {
                    std::string data(reinterpret_cast<char*>(event.packet->data),
                                     event.packet->dataLength);
                    UdpRequest req;
                    if (parseUdpRequest(data, req)) {
                        auto resp = handleUdp(req, event);
                        sendUdpResponse(event.peer, resp);
                    }
                    enet_packet_destroy(event.packet);
                    break;
                }
                case ENET_EVENT_TYPE_DISCONNECT:
                    break;
                default:
                    break;
            }
        }
        usleep(1000);
    }
    enet_host_destroy(host);
    enet_deinitialize();
}

int main() {
    signal(SIGPIPE, SIG_IGN);
    signal(SIGTERM, HandleSignal);
    signal(SIGINT, HandleSignal);
    const int port = GetPort();
    {
        std::ostringstream ss;
        ss << "r-" << std::chrono::duration_cast<std::chrono::milliseconds>(
                            std::chrono::system_clock::now().time_since_epoch()).count();
        g_refreshToken = ss.str();
    }

    std::thread udpThread(runUdpServer, port);
    std::thread physicsThread(PhysicsLoop);
    LoadGameCode();
    int serverFd = socket(AF_INET, SOCK_STREAM, 0);
    if (serverFd < 0) {
        std::cerr << "socket error: " << strerror(errno) << "\n";
        g_stop = 1;
        udpThread.join();
        physicsThread.join();
        return 1;
    }

    int opt = 1;
    setsockopt(serverFd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    sockaddr_in addr{};
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons(port);

    if (bind(serverFd, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)) < 0) {
        std::cerr << "bind error: " << strerror(errno) << "\n";
        close(serverFd);
        return 1;
    }
    if (listen(serverFd, 16) < 0) {
        std::cerr << "listen error: " << strerror(errno) << "\n";
        close(serverFd);
        return 1;
    }

    std::cout << "Tile server listening on 0.0.0.0:" << port << " file=" << GetFilePath() << "\n";
    while (!g_stop) {
        sockaddr_in client{};
        socklen_t len = sizeof(client);
        int clientFd = accept(serverFd, reinterpret_cast<sockaddr*>(&client), &len);
        if (clientFd < 0) {
            if (errno == EINTR) continue;
            std::cerr << "accept error: " << strerror(errno) << "\n";
            continue;
        }
        std::thread([clientFd, client]() {
            handleClient(clientFd, client);
            close(clientFd);
        }).detach();
    }

    close(serverFd);
    g_stop = 1;
    udpThread.join();
    physicsThread.join();
    if (__gcov_flush) __gcov_flush();
    return 0;
}
bool EnsureGameCodeDir();
std::string ExtractField(const std::string& body, const std::string& key);
void LoadGameCode();
