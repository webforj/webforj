package com.webforj.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.gson.Gson;
import com.webforj.devtools.livereload.message.ConnectedMessage;
import com.webforj.devtools.livereload.message.HeartbeatAckMessage;
import com.webforj.devtools.livereload.message.ReloadMessage;
import java.io.IOException;
import java.net.ServerSocket;
import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class LiveReloadServerTest {

  private LiveReloadServer server;
  private WebSocket mockConnection;
  private ClientHandshake mockHandshake;
  private final Gson gson = new Gson();

  @BeforeEach
  void setUp() throws IOException {
    server = new LiveReloadServer(findAvailablePort());
    mockConnection = mock(WebSocket.class);
    mockHandshake = mock(ClientHandshake.class);
    when(mockConnection.isOpen()).thenReturn(true);
  }

  @AfterEach
  void tearDown() throws InterruptedException {
    if (server.isRunning()) {
      server.stop();
    }
  }

  @Test
  void shouldReportNotRunningBeforeStart() {
    assertFalse(server.isRunning());
  }

  @Test
  void shouldReportRunningBetweenStartAndStop() throws InterruptedException {
    server.start();
    assertTrue(server.isRunning());

    server.stop();
    assertFalse(server.isRunning());
  }

  @Test
  void shouldAcceptConnectionsAndSendTheHandshake() {
    server.onOpen(mockConnection, mockHandshake);

    assertEquals(1, server.getConnectionCount());
    verify(mockConnection).send(gson.toJson(new ConnectedMessage()));
  }

  @Test
  void shouldRemoveConnectionWhenClientDisconnects() {
    server.onOpen(mockConnection, mockHandshake);
    assertEquals(1, server.getConnectionCount());

    server.onClose(mockConnection, 1001, "Going away", true);
    assertEquals(0, server.getConnectionCount());
  }

  @Test
  void shouldRespondToHeartbeatPing() {
    server.onOpen(mockConnection, mockHandshake);
    server.onMessage(mockConnection, "ping");

    verify(mockConnection).send(gson.toJson(new HeartbeatAckMessage()));
  }

  @Test
  void shouldIgnoreNonPingMessages() {
    server.onOpen(mockConnection, mockHandshake);
    server.onMessage(mockConnection, "hello");
    server.onMessage(mockConnection, "test");

    verify(mockConnection, times(1)).send(any(String.class));
  }

  @Test
  void shouldBroadcastReloadToEveryConnection() {
    WebSocket conn1 = openConnection();
    WebSocket conn2 = openConnection();

    server.sendReloadMessage();

    String expectedReload = gson.toJson(new ReloadMessage());
    verify(conn1).send(expectedReload);
    verify(conn2).send(expectedReload);
  }

  @Test
  void shouldBroadcastResourceUpdateToEveryConnection() {
    WebSocket conn1 = openConnection();
    WebSocket conn2 = openConnection();

    server.sendResourceUpdateMessage("css", "styles/main.css", null);

    verify(conn1).send(contains("styles/main.css"));
    verify(conn2).send(contains("styles/main.css"));
  }

  @Test
  void shouldRemoveConnectionOnError() {
    server.onOpen(mockConnection, mockHandshake);
    assertEquals(1, server.getConnectionCount());

    server.onError(mockConnection, new RuntimeException("Connection error"));
    assertEquals(0, server.getConnectionCount());
  }

  @Test
  void shouldSurviveAnErrorWithoutAConnection() {
    server.onError(null, new RuntimeException("Server error"));

    assertEquals(0, server.getConnectionCount());
  }

  @Test
  void shouldDropABrokenConnectionWhenSendingFails() {
    WebSocket brokenConn = mock(WebSocket.class);
    when(brokenConn.isOpen()).thenReturn(true);
    doAnswer(invocation -> {
      String message = invocation.getArgument(0);
      if (message.contains("reload")) {
        throw new RuntimeException("Send failed");
      }

      return null;
    }).when(brokenConn).send(anyString());

    server.onOpen(brokenConn, mockHandshake);
    server.sendReloadMessage();

    verify(brokenConn, times(2)).send(anyString());
  }

  @Test
  void shouldTrackMultipleConnectionsIndependently() {
    WebSocket conn1 = openConnection();
    WebSocket conn2 = openConnection();
    WebSocket conn3 = openConnection();
    assertEquals(3, server.getConnectionCount());

    server.onClose(conn2, 1000, "Normal closure", true);
    assertEquals(2, server.getConnectionCount());

    server.sendReloadMessage();
    verify(conn1).send(contains("reload"));
    verify(conn3).send(contains("reload"));
    verify(conn2, never()).send(contains("reload"));
  }

  private WebSocket openConnection() {
    WebSocket conn = mock(WebSocket.class);
    when(conn.isOpen()).thenReturn(true);
    server.onOpen(conn, mockHandshake);

    return conn;
  }

  private static int findAvailablePort() throws IOException {
    try (ServerSocket socket = new ServerSocket(0)) {
      return socket.getLocalPort();
    }
  }
}
