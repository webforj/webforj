package com.webforj.devtools.livereload.receiver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.devtools.livereload.LiveReloadServer;
import com.webforj.plugin.foundation.WatchPortFile;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.CountDownLatch;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class WatchReceiverTest {

  @Test
  void shouldHotSwapStylesheetChange() {
    LiveReloadServer server = runningServer();

    new WatchReceiver(server, true).handleLine("REBUILD styles/app.css");

    verify(server).sendResourceUpdateMessage(eq("css"), eq("styles/app.css"), isNull());
    verify(server, never()).sendReloadMessage();
  }

  @Test
  void shouldHotSwapAnImageChange() {
    LiveReloadServer server = runningServer();

    new WatchReceiver(server, true).handleLine("REBUILD assets/logo.png");

    verify(server).sendResourceUpdateMessage(eq("image"), eq("assets/logo.png"), isNull());
  }

  @Test
  void shouldReloadOnScriptChange() {
    LiveReloadServer server = runningServer();

    new WatchReceiver(server, true).handleLine("REBUILD main.js");

    verify(server).sendReloadMessage();
    verify(server, never()).sendResourceUpdateMessage(any(), any(), any());
  }

  @Test
  void shouldReloadWhenAnyChangedPathIsUnswappable() {
    LiveReloadServer server = runningServer();

    new WatchReceiver(server, true).handleLine("REBUILD app.css main.js");

    verify(server).sendReloadMessage();
    verify(server, never()).sendResourceUpdateMessage(any(), any(), any());
  }

  @Test
  void shouldReloadStylesheetChangeWhenHotSwapIsOff() {
    LiveReloadServer server = runningServer();

    new WatchReceiver(server, false).handleLine("REBUILD app.css");

    verify(server).sendReloadMessage();
    verify(server, never()).sendResourceUpdateMessage(any(), any(), any());
  }

  @Test
  void shouldReLogBundlerOutputWithoutTouchingTheServer() {
    LiveReloadServer server = mock(LiveReloadServer.class);

    new WatchReceiver(server, true).handleLine("LOG bundling main.js");

    verifyNoInteractions(server);
  }

  @Test
  void shouldReLogWarningsWithoutTouchingTheServer() {
    LiveReloadServer server = mock(LiveReloadServer.class);

    new WatchReceiver(server, true).handleLine("WARN duplicate package declaration");

    verifyNoInteractions(server);
  }

  @Test
  void shouldIgnoreAnEmptyRebuild() {
    LiveReloadServer server = mock(LiveReloadServer.class);

    new WatchReceiver(server, true).handleLine("REBUILD ");

    verify(server, never()).sendReloadMessage();
    verify(server, never()).sendResourceUpdateMessage(any(), any(), any());
  }

  @Test
  void shouldDoNothingWhenTheServerIsNotRunning() {
    LiveReloadServer server = mock(LiveReloadServer.class);
    when(server.isRunning()).thenReturn(false);

    new WatchReceiver(server, true).handleLine("REBUILD app.css");

    verify(server, never()).sendReloadMessage();
    verify(server, never()).sendResourceUpdateMessage(any(), any(), any());
  }

  @Test
  void shouldReadThePortFromTheDiscoveryFile(@TempDir Path tmp) throws Exception {
    Path portFile = tmp.resolve("watch.port");
    Files.writeString(portFile, "41234", StandardCharsets.UTF_8);

    assertEquals(41234, WatchReceiver.readPort(portFile).intValue());
  }

  @Test
  void shouldReturnNullPortForMissingFile(@TempDir Path tmp) {
    assertNull(WatchReceiver.readPort(tmp.resolve("absent.port")));
  }

  @Test
  void shouldReturnNullPortForMalformedFile(@TempDir Path tmp) throws Exception {
    Path portFile = tmp.resolve("watch.port");
    Files.writeString(portFile, "not-a-number", StandardCharsets.UTF_8);

    assertNull(WatchReceiver.readPort(portFile));
  }

  @Test
  @Timeout(10)
  void shouldStartAndStopIdempotently() {
    WatchReceiver receiver = new WatchReceiver(mock(LiveReloadServer.class), true);

    receiver.start();
    receiver.start();
    assertTrue(receiver.isRunning());

    receiver.stop();
    receiver.stop();
    assertFalse(receiver.isRunning());
  }

  @Test
  void shouldSendTheContentOfStylesheetWhenHotSwapIsOff() {
    LiveReloadServer server = runningServer();

    new WatchReceiver(server, false).handleLine("REBUILD watch-receiver/served.css");

    verify(server).sendResourceUpdateMessage("css", "watch-receiver/served.css",
        "body { color: red; }\n");
    verify(server, never()).sendReloadMessage();
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldReloadForStylesheetOutsideTheServedFolder(boolean hotSwap) {
    LiveReloadServer server = runningServer();

    new WatchReceiver(server, hotSwap).handleLine("REBUILD ../outside.css");

    verify(server).sendReloadMessage();
    verify(server, never()).sendResourceUpdateMessage(any(), any(), any());
  }

  @Test
  void shouldReloadOnceForStylesheetAndAnImageWhenHotSwapIsOff() {
    LiveReloadServer server = runningServer();

    new WatchReceiver(server, false)
        .handleLine("REBUILD watch-receiver/served.css assets/logo.png");

    verify(server, times(1)).sendReloadMessage();
    verify(server, never()).sendResourceUpdateMessage(any(), any(), any());
  }

  @Test
  void shouldReloadOnceForAnUnreadableStylesheetWhenHotSwapIsOff() {
    LiveReloadServer server = runningServer();

    new WatchReceiver(server, false)
        .handleLine("REBUILD watch-receiver/served.css watch-receiver/absent.css");

    verify(server, times(1)).sendReloadMessage();
    verify(server, never()).sendResourceUpdateMessage(any(), any(), any());
  }

  @Test
  @Timeout(10)
  void shouldConnectToTheWatchOfTheProjectRoot(@TempDir Path tmp) throws Exception {
    String projectRoot = tmp.toAbsolutePath().toString();
    Path portFile = WatchPortFile.resolve(projectRoot);
    WatchReceiver receiver =
        new WatchReceiver(mock(LiveReloadServer.class), true, projectRoot, () -> {
        });

    try (ServerSocket watch = new ServerSocket(0)) {
      Files.writeString(portFile, Integer.toString(watch.getLocalPort()), StandardCharsets.UTF_8);
      receiver.start();

      try (Socket connection = watch.accept()) {
        assertTrue(connection.isConnected());
      }
    } finally {
      receiver.stop();
      Files.deleteIfExists(portFile);
    }
  }

  @Test
  @Timeout(10)
  void shouldReportTheEndOfTheWatchWhenItsConnectionCloses(@TempDir Path tmp) throws Exception {
    String projectRoot = tmp.toAbsolutePath().toString();
    Path portFile = WatchPortFile.resolve(projectRoot);
    CountDownLatch ended = new CountDownLatch(1);
    WatchReceiver receiver =
        new WatchReceiver(mock(LiveReloadServer.class), true, projectRoot, ended::countDown);

    try (ServerSocket watch = new ServerSocket(0)) {
      Files.writeString(portFile, Integer.toString(watch.getLocalPort()), StandardCharsets.UTF_8);
      receiver.start();
      watch.accept().close();
      ended.await();

      assertFalse(receiver.isRunning(), "the receiver should stop once the watch ended");
    } finally {
      receiver.stop();
      Files.deleteIfExists(portFile);
    }
  }

  @Test
  @Timeout(10)
  void shouldNotReportTheEndOfTheWatchWhenStopped(@TempDir Path tmp) throws Exception {
    String projectRoot = tmp.toAbsolutePath().toString();
    Path portFile = WatchPortFile.resolve(projectRoot);
    CountDownLatch ended = new CountDownLatch(1);
    WatchReceiver receiver =
        new WatchReceiver(mock(LiveReloadServer.class), true, projectRoot, ended::countDown);

    try (ServerSocket watch = new ServerSocket(0)) {
      Files.writeString(portFile, Integer.toString(watch.getLocalPort()), StandardCharsets.UTF_8);
      receiver.start();
      try (Socket connection = watch.accept()) {
        receiver.stop();

        assertEquals(-1, connection.getInputStream().read());
      }

      assertEquals(1, ended.getCount(), "a stop should not count as the watch ending");
    } finally {
      receiver.stop();
      Files.deleteIfExists(portFile);
    }
  }

  private static LiveReloadServer runningServer() {
    LiveReloadServer server = mock(LiveReloadServer.class);
    when(server.isRunning()).thenReturn(true);

    return server;
  }
}
