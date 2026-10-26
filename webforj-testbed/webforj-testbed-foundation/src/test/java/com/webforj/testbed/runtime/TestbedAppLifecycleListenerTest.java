package com.webforj.testbed.runtime;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotSame;

import com.webforj.App;
import com.webforj.Bootstrap;
import com.webforj.Environment;
import com.webforj.router.Router;
import com.webforj.router.history.BrowserHistory;
import com.webforj.testbed.WebforjTestbed;
import com.webforj.testbed.WebforjTestbedOptions;
import com.webforj.testbed.fixture.TestApp;
import org.junit.jupiter.api.Test;

class TestbedAppLifecycleListenerTest {
  @Test
  void shouldLeaveBrowserHistoryWhenNoTestbedRuntimeIsActive() throws Exception {
    String key = "com.basis.noBBjServices";
    String previous = System.getProperty(key);
    System.setProperty(key, "true");
    try {
      App app = Bootstrap.init(new BBjApiMock().getApi(), 0, TestApp.class.getName());
      try {
        assertInstanceOf(BrowserHistory.class, Router.getCurrent().getHistory());
      } finally {
        app.terminate();
      }
    } finally {
      Environment.cleanup();
      if (previous == null) {
        System.clearProperty(key);
      } else {
        System.setProperty(key, previous);
      }
    }
  }

  @Test
  void shouldCreateSeparateHistoryForEachTestbedSession() {
    Object first;
    WebforjTestbedOptions options = new WebforjTestbedOptions().setApp(TestApp.class);
    try {
      first = WebforjTestbed.setup(options).getRouter().orElseThrow().getHistory();
    } finally {
      WebforjTestbed.tearDown();
    }
    try {
      Object second = WebforjTestbed.setup(options).getRouter().orElseThrow().getHistory();
      assertNotSame(first, second);
    } finally {
      WebforjTestbed.tearDown();
    }
  }
}
