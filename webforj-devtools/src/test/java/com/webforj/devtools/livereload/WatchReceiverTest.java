package com.webforj.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.io.TempDir;

class WatchReceiverTest {

  @Test
  void shouldHotSwapAStylesheetChange() {
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
  void shouldReloadOnAScriptChange() {
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
  void shouldReloadAStylesheetChangeWhenHotSwapIsOff() {
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
  void shouldReturnNullPortForAMissingFile(@TempDir Path tmp) {
    assertNull(WatchReceiver.readPort(tmp.resolve("absent.port")));
  }

  @Test
  void shouldReturnNullPortForAMalformedFile(@TempDir Path tmp) throws Exception {
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

  private static LiveReloadServer runningServer() {
    LiveReloadServer server = mock(LiveReloadServer.class);
    when(server.isRunning()).thenReturn(true);

    return server;
  }
}
