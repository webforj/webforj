package com.webforj.jbang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

import com.webforj.App;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class JbangShutdownListenerTest {

  private JbangShutdownListener listener;

  @BeforeEach
  void setUp() {
    listener = new JbangShutdownListener();
    JbangShutdownListener.resetState();
  }

  @AfterEach
  void tearDown() {
    System.clearProperty("webforj.jbang.auto-shutdown");
  }

  @Test
  void shouldNotEnableWhenSystemPropertyIsFalse() {
    System.setProperty("webforj.jbang.auto-shutdown", "false");
    App mockApp = mock(App.class);
    listener.onDidRun(mockApp);

    assertEquals(0, JbangShutdownListener.getActiveAppsCount());
  }

  @Test
  void shouldIncrementActiveAppsOnDidRun() {
    System.setProperty("webforj.jbang.auto-shutdown", "true");
    App mockApp = mock(App.class);
    listener.onDidRun(mockApp);

    assertEquals(1, JbangShutdownListener.getActiveAppsCount());
  }

  @Test
  void shouldDecrementActiveAppsOnDidTerminate() {
    System.setProperty("webforj.jbang.auto-shutdown", "true");
    App mockApp = mock(App.class);
    listener.onDidRun(mockApp);
    assertEquals(1, JbangShutdownListener.getActiveAppsCount());

    listener.onDidTerminate(mockApp);
    assertEquals(0, JbangShutdownListener.getActiveAppsCount());
  }

  @Test
  void shouldTrackMultipleApps() {
    System.setProperty("webforj.jbang.auto-shutdown", "true");
    App mockApp1 = mock(App.class);
    App mockApp2 = mock(App.class);

    listener.onDidRun(mockApp1);
    listener.onDidRun(mockApp2);
    assertEquals(2, JbangShutdownListener.getActiveAppsCount());

    listener.onDidTerminate(mockApp1);
    assertEquals(1, JbangShutdownListener.getActiveAppsCount());

    listener.onDidTerminate(mockApp2);
    assertEquals(0, JbangShutdownListener.getActiveAppsCount());
  }
}
