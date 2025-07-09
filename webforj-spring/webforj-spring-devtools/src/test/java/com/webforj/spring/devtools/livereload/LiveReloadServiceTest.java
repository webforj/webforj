package com.webforj.spring.devtools.livereload;

import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

class LiveReloadServiceTest {

  private LiveReloadService reloadService;

  @Mock
  private LiveReloadServer mockWebSocketServer;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
    reloadService = new LiveReloadService();
  }

  @Test
  void shouldTriggerReloadWhenWebSocketServerIsAvailable() {
    reloadService.setWebSocketServer(mockWebSocketServer);
    reloadService.triggerReload();
    verify(mockWebSocketServer).sendReloadMessage();
  }

  @Test
  void shouldHandleReloadGracefullyWhenWebSocketServerNotInitialized() {
    reloadService.triggerReload();
    verify(mockWebSocketServer, never()).sendReloadMessage();
  }

  @Test
  void shouldAllowMultipleReloadRequestsInSequence() {
    reloadService.setWebSocketServer(mockWebSocketServer);
    reloadService.triggerReload();
    reloadService.triggerReload();
    reloadService.triggerReload();
    verify(mockWebSocketServer, times(3)).sendReloadMessage();
  }

  @Test
  void shouldUpdateWebSocketServerReference() {
    LiveReloadServer firstServer = org.mockito.Mockito.mock(LiveReloadServer.class);
    reloadService.setWebSocketServer(firstServer);
    reloadService.setWebSocketServer(mockWebSocketServer);
    reloadService.triggerReload();
    verify(mockWebSocketServer).sendReloadMessage();
    verify(firstServer, never()).sendReloadMessage();
  }
}
