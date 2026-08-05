package com.webforj.devtools.hotswap;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.devtools.livereload.receiver.HotswapAgentReceiver;
import com.webforj.devtools.livereload.receiver.HotswapAgentReceiverFixture;
import com.webforj.devtools.livereload.LiveReloadServer;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

class WebforjHotswapPluginTest {

  private static final String REDEFINED_CLASS = WebforjHotswapPluginTest.class.getName();

  @Test
  @Timeout(10)
  void shouldForwardTheRedefinitionToTheReceiver() {
    LiveReloadServer server = mock(LiveReloadServer.class);
    when(server.isRunning()).thenReturn(true);
    CountDownLatch update = new CountDownLatch(1);
    doAnswer(invocation -> {
      update.countDown();
      return null;
    }).when(server).sendClassUpdateMessage(any());

    HotswapAgentReceiver receiver = HotswapAgentReceiverFixture.withAgentDetected(server);
    receiver.start();

    try {
      WebforjHotswapPlugin.onClassRedefinition(REDEFINED_CLASS, new byte[] {1, 2, 3},
          getClass().getClassLoader());

      assertTrue(await(update));
    } finally {
      receiver.stop();
    }
  }

  @Test
  @Timeout(10)
  void shouldStandDownWhenTheReceiverIsAbsent() {
    ClassLoader emptyClassLoader = new URLClassLoader(new URL[0], null);

    assertDoesNotThrow(() -> WebforjHotswapPlugin.onClassRedefinition(REDEFINED_CLASS,
        new byte[] {1, 2, 3}, emptyClassLoader));
  }

  @Test
  @Timeout(10)
  void shouldIgnoreTheBootstrapClassloader() {
    assertDoesNotThrow(() -> WebforjHotswapPlugin.onClassRedefinition(REDEFINED_CLASS,
        new byte[] {1, 2, 3}, null));
  }

  private static boolean await(CountDownLatch latch) {
    try {
      return latch.await(5, TimeUnit.SECONDS);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();

      return false;
    }
  }
}
