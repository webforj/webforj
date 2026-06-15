package com.webforj.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.mock;

import jakarta.servlet.ServletContextEvent;
import org.junit.jupiter.api.Test;

class WatchContextListenerTest {

  @Test
  void shouldStayInertWhenLiveReloadIsNotConfigured() {
    WatchContextListener listener = new WatchContextListener();

    // No webforj.conf enabling live reload is on the test classpath, so the lifecycle starts
    // nothing and the deployment callbacks complete without binding a port.
    assertDoesNotThrow(() -> listener.contextInitialized(mock(ServletContextEvent.class)));
    assertDoesNotThrow(() -> listener.contextDestroyed(mock(ServletContextEvent.class)));
  }

  @Test
  void shouldDestroyHarmlesslyWithoutInit() {
    assertDoesNotThrow(
        () -> new WatchContextListener().contextDestroyed(mock(ServletContextEvent.class)));
  }
}
