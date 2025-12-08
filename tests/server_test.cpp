#include <gtest/gtest.h>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <unistd.h>

#include <chrono>
#include <csignal>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

namespace {

struct HttpResponse {
  int status = 0;
  std::string body;
};

HttpResponse HttpRequest(int port, const std::string& method,
                         const std::string& path,
                         const std::string& body = "") {
  int fd = socket(AF_INET, SOCK_STREAM, 0);
  if (fd < 0) throw std::runtime_error("socket failed");

  timeval tv{};
  tv.tv_sec = 5;
  setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
  setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));

  sockaddr_in addr{};
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  inet_pton(AF_INET, "127.0.0.1", &addr.sin_addr);
  if (connect(fd, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)) < 0) {
    close(fd);
    throw std::runtime_error("connect failed");
  }

  std::ostringstream req;
  req << method << " " << path << " HTTP/1.1\r\n";
  req << "Host: 127.0.0.1\r\n";
  req << "Content-Length: " << body.size() << "\r\n";
  req << "Content-Type: application/json\r\n";
  req << "Connection: close\r\n\r\n";
  req << body;
  auto payload = req.str();
  send(fd, payload.data(), payload.size(), 0);

  std::string resp;
  char buf[4096];
  ssize_t n;
  while ((n = recv(fd, buf, sizeof(buf), 0)) > 0) {
    resp.append(buf, buf + n);
  }
  close(fd);

  HttpResponse out;
  auto posStatusEnd = resp.find("\r\n");
  if (posStatusEnd != std::string::npos) {
    std::istringstream statusLine(resp.substr(0, posStatusEnd));
    std::string httpVer;
    statusLine >> httpVer >> out.status;
  }
  auto posBody = resp.find("\r\n\r\n");
  if (posBody != std::string::npos) {
    out.body = resp.substr(posBody + 4);
  }
  return out;
}

class ServerFixture : public ::testing::Test {
 protected:
  void SetUp() override {
    port_ = 18081;
    file_ = "/tmp/tiles_test.json";
    std::filesystem::remove(file_);
    setenv("PORT", std::to_string(port_).c_str(), 1);
    setenv("TILES_FILE", file_.c_str(), 1);
    pid_ = fork();
    if (pid_ == 0) {
      execl("./server", "server", nullptr);
      _exit(1);
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(300));
  }

  void TearDown() override {
    if (pid_ > 0) {
      kill(pid_, SIGTERM);
      waitpid(pid_, nullptr, 0);
    }
    std::filesystem::remove(file_);
  }

  int port_{0};
  std::string file_;
  pid_t pid_{-1};
};

}  // namespace

TEST_F(ServerFixture, ReturnsDefaultOnFirstGet) {
  auto resp = HttpRequest(port_, "GET", "/tiles");
  EXPECT_EQ(200, resp.status);
  EXPECT_NE(std::string::npos, resp.body.find("\"tiles\""))
      << "Body should contain tiles array";
  EXPECT_NE(std::string::npos, resp.body.find("Realm"))
      << "Default tiles should include Realm";
}

TEST_F(ServerFixture, PersistsAndReadsBackTiles) {
  std::string payload = R"({"tiles":[{"id":"A","x":1,"y":2},{"id":"B","x":3,"y":4}]})";
  auto post = HttpRequest(port_, "POST", "/tiles", payload);
  EXPECT_EQ(200, post.status);

  auto get = HttpRequest(port_, "GET", "/tiles");
  EXPECT_EQ(200, get.status);
  EXPECT_NE(std::string::npos, get.body.find("\"A\""));
  EXPECT_NE(std::string::npos, get.body.find("\"B\""));
  EXPECT_NE(std::string::npos, get.body.find("\"x\":3"));
}
