#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

#include <cerrno>
#include <csignal>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

// Minimal HTTP server to persist tile positions.
// Endpoints:
//   GET /tiles    -> returns JSON with stored positions (or default if none)
//   POST /tiles   -> body is JSON; saved to disk
// CORS: Access-Control-Allow-Origin: *
// Storage file: tiles.json in working directory.

static const int kDefaultPort = 8081;
static const char* kDefaultFile = "tiles.json";
static volatile sig_atomic_t g_stop = 0;

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

std::string httpResponse(const std::string& body, const std::string& status = "200 OK", const std::string& contentType = "application/json") {
    std::ostringstream ss;
    ss << "HTTP/1.1 " << status << "\r\n";
    ss << "Access-Control-Allow-Origin: *\r\n";
    ss << "Access-Control-Allow-Methods: GET, POST, OPTIONS\r\n";
    ss << "Access-Control-Allow-Headers: Content-Type\r\n";
    ss << "Content-Type: " << contentType << "\r\n";
    ss << "Content-Length: " << body.size() << "\r\n";
    ss << "Connection: close\r\n\r\n";
    ss << body;
    return ss.str();
}

void handleClient(int clientFd) {
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

    if (method == "GET" && path == "/tiles") {
        std::string data = readFile(GetFilePath());
        if (data.empty()) data = defaultJson();
        std::string resp = httpResponse(data);
        send(clientFd, resp.data(), resp.size(), 0);
        return;
    }

    if (method == "POST" && path == "/tiles") {
        if (!body.empty()) {
          if (!writeFile(GetFilePath(), body)) {
              std::string resp = httpResponse(R"({"error":"failed to write file"})", "500 Internal Server Error");
              send(clientFd, resp.data(), resp.size(), 0);
              return;
          }
          std::string resp = httpResponse(R"({"ok":true})");
          send(clientFd, resp.data(), resp.size(), 0);
        } else {
          std::string resp = httpResponse(R"({"error":"empty body"})", "400 Bad Request");
          send(clientFd, resp.data(), resp.size(), 0);
        }
        return;
    }

    std::string resp = httpResponse(R"({"error":"not found"})", "404 Not Found");
    send(clientFd, resp.data(), resp.size(), 0);
}

int main() {
    signal(SIGPIPE, SIG_IGN);
    signal(SIGTERM, HandleSignal);
    signal(SIGINT, HandleSignal);
    const int port = GetPort();
    int serverFd = socket(AF_INET, SOCK_STREAM, 0);
    if (serverFd < 0) {
        std::cerr << "socket error: " << strerror(errno) << "\n";
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
        handleClient(clientFd);
        close(clientFd);
    }

    close(serverFd);
    if (__gcov_flush) __gcov_flush();
    return 0;
}
