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
import com.webforj.devtools.livereload.message.RestartingMessage;
import java.io.IOException;
import java.net.ServerSocket;
import java.util.Set;
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
  void shouldHandTheDeclaredHotswapStateToTheConnectingClient() {
    System.setProperty(LiveReloadServer.HOTSWAP_TOOL_PROPERTY, "hotswapAgent");
    System.setProperty(LiveReloadServer.HOTSWAP_LEVEL_PROPERTY, "limited");

    try {
      server.onOpen(mockConnection, mockHandshake);

      verify(mockConnection).send(gson.toJson(new ConnectedMessage("hotswapAgent", "limited")));
    } finally {
      System.clearProperty(LiveReloadServer.HOTSWAP_TOOL_PROPERTY);
      System.clearProperty(LiveReloadServer.HOTSWAP_LEVEL_PROPERTY);
    }
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
  void shouldBroadcastRestartingToEveryConnection() {
    WebSocket conn1 = openConnection();
    WebSocket conn2 = openConnection();

    server.sendRestartingMessage();

    String expectedRestarting = gson.toJson(new RestartingMessage());
    verify(conn1).send(expectedRestarting);
    verify(conn2).send(expectedRestarting);
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
  void shouldReloadTheConnectingPageServedBeforeTheLastReloadCommand() {
    // The command fires while nobody is connected, exactly the moment a browser is between pages.
    server.sendReloadMessage();

    WebSocket conn = openConnection();
    server.onMessage(conn, helloMessage(System.currentTimeMillis() - 60_000));

    verify(conn).send(gson.toJson(new ReloadMessage()));
  }

  @Test
  void shouldNotReloadTheConnectingPageServedAfterTheLastReloadCommand() {
    server.sendReloadMessage();

    WebSocket conn = openConnection();
    server.onMessage(conn, helloMessage(System.currentTimeMillis() + 60_000));

    verify(conn, never()).send(gson.toJson(new ReloadMessage()));
  }

  @Test
  void shouldNotReloadTheConnectingPageWhenNoReloadCommandEverFired() {
    WebSocket conn = openConnection();
    server.onMessage(conn, helloMessage(System.currentTimeMillis() - 60_000));

    verify(conn, never()).send(gson.toJson(new ReloadMessage()));
  }

  @Test
  void shouldIgnoreTheHelloWithoutTheServedStamp() {
    // A page without a stamp must never be reloaded on connect, or every reconnect would loop.
    server.sendReloadMessage();

    WebSocket conn = openConnection();
    server.onMessage(conn, "{\"type\":\"hello\",\"pageServedAt\":0}");

    verify(conn, never()).send(gson.toJson(new ReloadMessage()));
  }

  @Test
  void shouldReplayTheResourceUpdateTheConnectingPageMissed() {
    // The update goes out while nobody is connected, exactly the moment a browser is between
    // pages. The page must receive it in place on connect, never as a page reload.
    server.sendResourceUpdateMessage("css", "styles/app.css", null);

    WebSocket conn = openConnection();
    server.onMessage(conn, helloMessage(System.currentTimeMillis() - 60_000));

    verify(conn).send(contains("styles/app.css"));
    verify(conn, never()).send(gson.toJson(new ReloadMessage()));
  }

  @Test
  void shouldNotReplayToThePageServedAfterTheUpdate() {
    server.sendResourceUpdateMessage("css", "styles/app.css", null);

    WebSocket conn = openConnection();
    server.onMessage(conn, helloMessage(System.currentTimeMillis() + 60_000));

    verify(conn, never()).send(contains("styles/app.css"));
  }

  @Test
  void shouldReplayOnlyTheLatestUpdatePerPath() {
    server.sendResourceUpdateMessage("css", "styles/app.css", null);
    server.sendResourceUpdateMessage("css", "styles/app.css", null);
    server.sendResourceUpdateMessage("image", "logo.png", null);

    WebSocket conn = openConnection();
    server.onMessage(conn, helloMessage(System.currentTimeMillis() - 60_000));

    verify(conn, times(1)).send(contains("styles/app.css"));
    verify(conn, times(1)).send(contains("logo.png"));
  }

  @Test
  void shouldBroadcastTheClassUpdateToEveryConnection() {
    WebSocket conn1 = openConnection();
    WebSocket conn2 = openConnection();

    server.sendClassUpdateMessage(Set.of("com.example.DashboardView"));

    verify(conn1).send(contains("com.example.DashboardView"));
    verify(conn2).send(contains("com.example.DashboardView"));
  }

  @Test
  void shouldSendNothingForTheEmptyClassUpdate() {
    WebSocket conn = openConnection();

    server.sendClassUpdateMessage(Set.of());

    // Only the connected handshake reaches the client.
    verify(conn, times(1)).send(any(String.class));
  }

  @Test
  void shouldBroadcastTheClassUpdateRejectionToEveryConnection() {
    WebSocket conn1 = openConnection();
    WebSocket conn2 = openConnection();

    server.sendClassUpdateErrorMessage(Set.of("com.example.DashboardView"),
        "attempted to change the schema (add/remove fields)");

    verify(conn1).send(contains("class-update-error"));
    verify(conn1).send(contains("attempted to change the schema"));
    verify(conn2).send(contains("class-update-error"));
  }

  @Test
  void shouldNotReplayTheRejectionToTheConnectingPage() {
    // The rejection describes one moment. A page connecting later starts from the served code
    // anyway, so a replayed rejection would only report a problem that page never had.
    server.sendClassUpdateErrorMessage(Set.of("com.example.DashboardView"), "rejected");

    WebSocket conn = openConnection();
    server.onMessage(conn, helloMessage(System.currentTimeMillis() - 60_000));

    verify(conn, never()).send(contains("class-update-error"));
  }

  @Test
  void shouldReplayTheClassUpdateTheConnectingPageMissed() {
    // The update goes out while nobody is connected, exactly the moment a browser is between
    // pages. The page must receive the class names on connect, so its application instance can
    // rebuild the affected part instead of reloading blindly.
    server.sendClassUpdateMessage(Set.of("com.example.DashboardView"));

    WebSocket conn = openConnection();
    server.onMessage(conn, helloMessage(System.currentTimeMillis() - 60_000));

    verify(conn).send(contains("com.example.DashboardView"));
    verify(conn, never()).send(gson.toJson(new ReloadMessage()));
  }

  @Test
  void shouldNotReplayTheClassUpdateToThePageServedAfterIt() {
    server.sendClassUpdateMessage(Set.of("com.example.DashboardView"));

    WebSocket conn = openConnection();
    server.onMessage(conn, helloMessage(System.currentTimeMillis() + 60_000));

    verify(conn, never()).send(contains("class-update"));
  }

  @Test
  void shouldReplayTheMissedClassesInOneMessage() {
    server.sendClassUpdateMessage(Set.of("com.example.DashboardView"));
    server.sendClassUpdateMessage(Set.of("com.example.MainLayout"));

    WebSocket conn = openConnection();
    server.onMessage(conn, helloMessage(System.currentTimeMillis() - 60_000));

    verify(conn, times(1)).send(contains("class-update"));
    verify(conn).send(contains("com.example.DashboardView"));
    verify(conn).send(contains("com.example.MainLayout"));
  }

  @Test
  void shouldPreferTheReloadWhenThePageMissedBothTheReloadAndTheClassUpdate() {
    server.sendReloadMessage();
    server.sendClassUpdateMessage(Set.of("com.example.DashboardView"));

    WebSocket conn = openConnection();
    server.onMessage(conn, helloMessage(System.currentTimeMillis() - 60_000));

    verify(conn).send(gson.toJson(new ReloadMessage()));
    verify(conn, never()).send(contains("class-update"));
  }

  @Test
  void shouldPreferTheReloadWhenThePageMissedBoth() {
    // The reloaded page fetches every resource fresh, so replaying on top would be noise.
    server.sendReloadMessage();
    server.sendResourceUpdateMessage("css", "styles/app.css", null);

    WebSocket conn = openConnection();
    server.onMessage(conn, helloMessage(System.currentTimeMillis() - 60_000));

    verify(conn).send(gson.toJson(new ReloadMessage()));
    verify(conn, never()).send(contains("styles/app.css"));
  }

  @Test
  void shouldSurviveAnUnreadableClientMessage() {
    WebSocket conn = openConnection();

    server.onMessage(conn, "{broken json");
    server.onMessage(conn, "{\"type\":\"other\"}");

    verify(conn, times(1)).send(any(String.class));
  }

  @Test
  void shouldRemoveConnectionOnError() {
    server.onOpen(mockConnection, mockHandshake);
    assertEquals(1, server.getConnectionCount());

    server.onError(mockConnection, new RuntimeException("Connection error"));
    assertEquals(0, server.getConnectionCount());
  }

  @Test
  void shouldSurviveAnErrorWithoutAnyConnection() {
    server.onError(null, new RuntimeException("Server error"));

    assertEquals(0, server.getConnectionCount());
  }

  @Test
  void shouldDropTheBrokenConnectionWhenSendingFails() {
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

  private static String helloMessage(long pageServedAt) {
    return "{\"type\":\"hello\",\"pageServedAt\":" + pageServedAt + "}";
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
