package com.webforj.plugin.foundation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

class WatchSocketServerTest {

  @Test
  @Timeout(10)
  void shouldBindAFreeLoopbackPort() throws Exception {
    try (WatchSocketServer server = new WatchSocketServer()) {
      server.start();

      assertTrue(server.getPort() > 0);
    }
  }

  @Test
  @Timeout(10)
  void shouldReplayBufferedLinesToTheFirstClientThenStreamLive() throws Exception {
    try (WatchSocketServer server = new WatchSocketServer()) {
      server.start();
      // No client has connected yet, so this line is buffered and replayed on the first connect.
      server.send("LOG initial build");

      try (Socket client = new Socket()) {
        client.connect(new InetSocketAddress("127.0.0.1", server.getPort()), 2000);
        BufferedReader reader = new BufferedReader(
            new InputStreamReader(client.getInputStream(), StandardCharsets.UTF_8));

        // The blocking read waits for the buffered line to be replayed once the server registers
        // this connection, which also proves the client is connected before the next live send.
        assertEquals("LOG initial build", reader.readLine());

        server.send("REBUILD app.css");
        assertEquals("REBUILD app.css", reader.readLine());
      }
    }
  }
}
