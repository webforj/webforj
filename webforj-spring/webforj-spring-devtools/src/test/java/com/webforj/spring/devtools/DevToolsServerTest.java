package com.webforj.spring.devtools;

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
import com.webforj.spring.devtools.message.ConnectedMessage;
import com.webforj.spring.devtools.message.HeartbeatAckMessage;
import com.webforj.spring.devtools.message.ReloadMessage;
import java.io.IOException;
import java.net.ServerSocket;
import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DevToolsServerTest {

  private DevToolsServer server;
  private WebSocket mockConnection;
  private ClientHandshake mockHandshake;
  private Gson gson = new Gson();
  private int port;

  @BeforeEach
  void setUp() throws IOException {
    port = findAvailablePort();
    server = new DevToolsServer(port);
    mockConnection = mock(WebSocket.class);
    mockHandshake = mock(ClientHandshake.class);
    when(mockConnection.isOpen()).thenReturn(true);
  }

  @AfterEach
  void tearDown() throws InterruptedException {
    if (server != null && server.isOpen()) {
      server.stop();
    }
  }

  private int findAvailablePort() throws IOException {
    try (ServerSocket socket = new ServerSocket(0)) {
      return socket.getLocalPort();
    }
  }

  @Test
  void shouldAcceptNewWebSocketConnectionsAndSendHandshake() {
    server.onOpen(mockConnection, mockHandshake);
    assertEquals(1, server.getConnectionCount());
    String expectedMessage = gson.toJson(new ConnectedMessage());
    verify(mockConnection).send(expectedMessage);
  }

  @Test
  void shouldRemoveConnectionWhenClientDisconnects() {
    server.onOpen(mockConnection, mockHandshake);
    assertEquals(1, server.getConnectionCount());
    server.onClose(mockConnection, 1001, "Going away", true);
    assertEquals(0, server.getConnectionCount());
  }

  @Test
  void shouldRespondToHeartbeatPingMessages() {
    server.onOpen(mockConnection, mockHandshake);
    server.onMessage(mockConnection, "ping");
    String expectedAck = gson.toJson(new HeartbeatAckMessage());
    verify(mockConnection).send(expectedAck);
  }

  @Test
  void shouldIgnoreNonPingMessages() {
    server.onOpen(mockConnection, mockHandshake);
    server.onMessage(mockConnection, "hello");
    server.onMessage(mockConnection, "test");
    verify(mockConnection, times(1)).send(any(String.class));
  }

  @Test
  void shouldBroadcastReloadMessageToAllConnections() {
    WebSocket conn1 = mock(WebSocket.class);
    WebSocket conn2 = mock(WebSocket.class);
    WebSocket conn3 = mock(WebSocket.class);
    when(conn1.isOpen()).thenReturn(true);
    when(conn2.isOpen()).thenReturn(true);
    when(conn3.isOpen()).thenReturn(true);

    server.onOpen(conn1, mockHandshake);
    server.onOpen(conn2, mockHandshake);
    server.onOpen(conn3, mockHandshake);
    server.sendReloadMessage();
    String expectedReload = gson.toJson(new ReloadMessage());
    verify(conn1).send((String) expectedReload);
    verify(conn2).send((String) expectedReload);
    verify(conn3).send((String) expectedReload);
  }

  @Test
  void shouldHandleClosedConnectionsDuringBroadcast() {
    WebSocket openConn = mock(WebSocket.class);
    WebSocket closedConn = mock(WebSocket.class);
    when(openConn.isOpen()).thenReturn(true);
    when(closedConn.isOpen()).thenReturn(false);

    server.onOpen(openConn, mockHandshake);
    server.onOpen(closedConn, mockHandshake);
    server.sendReloadMessage();
    verify(openConn, times(2)).send(anyString()); // connected + reload messages
    verify(closedConn).send(contains("connected")); // connected message before closing
  }

  @Test
  void shouldBroadcastResourceUpdateMessages() {
    WebSocket conn1 = mock(WebSocket.class);
    WebSocket conn2 = mock(WebSocket.class);
    when(conn1.isOpen()).thenReturn(true);
    when(conn2.isOpen()).thenReturn(true);

    server.onOpen(conn1, mockHandshake);
    server.onOpen(conn2, mockHandshake);
    server.sendResourceUpdateMessage("css", "styles/main.css", null);
    verify(conn1, times(2)).send(anyString()); // connected + update
    verify(conn2, times(2)).send(anyString()); // connected + update
    verify(conn1).send(contains("styles/main.css"));
    verify(conn2).send(contains("styles/main.css"));
  }

  @Test
  void shouldRemoveConnectionsOnError() {
    server.onOpen(mockConnection, mockHandshake);
    assertEquals(1, server.getConnectionCount());
    server.onError(mockConnection, new RuntimeException("Connection error"));
    assertEquals(0, server.getConnectionCount());
  }

  @Test
  void shouldHandleServerErrorsWithoutConnection() {
    server.onError(null, new RuntimeException("Server error"));
    assertTrue(server.isOpen());
  }

  @Test
  void shouldRemoveBrokenConnectionsWhenSendingFails() {
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
    assertEquals(1, server.getConnectionCount());
    server.sendReloadMessage();
    verify(brokenConn, times(2)).send(anyString()); // connected + reload attempt
  }

  @Test
  void shouldTrackMultipleConnectionsIndependently() {
    WebSocket conn1 = mock(WebSocket.class);
    WebSocket conn2 = mock(WebSocket.class);
    WebSocket conn3 = mock(WebSocket.class);
    when(conn1.isOpen()).thenReturn(true);
    when(conn2.isOpen()).thenReturn(true);
    when(conn3.isOpen()).thenReturn(true);

    server.onOpen(conn1, mockHandshake);
    server.onOpen(conn2, mockHandshake);
    server.onOpen(conn3, mockHandshake);
    assertEquals(3, server.getConnectionCount());
    server.onClose(conn2, 1000, "Normal closure", true);
    assertEquals(2, server.getConnectionCount());
    server.sendReloadMessage();
    verify(conn1).send(contains("reload"));
    verify(conn3).send(contains("reload"));
    verify(conn2, never()).send(contains("reload"));
  }

  @Test
  void shouldUpdateServerStateOnStartAndStop() throws InterruptedException {
    server.onStart();
    assertTrue(server.isOpen());
    server.stop();
    assertFalse(server.isOpen());
  }
}
