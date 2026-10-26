package com.webforj.testbed;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.App;
import com.webforj.bridge.AppAccessor;
import com.webforj.router.Router;
import com.webforj.router.history.MemoryHistory;
import com.webforj.testbed.fixture.AboutView;
import com.webforj.testbed.fixture.StartupHistoryApp;
import org.junit.jupiter.api.Test;

class StartupHistoryTest extends TestbedTestSupport {

  @Override
  protected WebforjTestbedOptions options() {
    return new WebforjTestbedOptions().setApp(StartupHistoryApp.class);
  }

  @Test
  void shouldUseMemoryHistoryDuringApplicationStartup() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    StartupHistoryApp app = (StartupHistoryApp) context.getApp().orElseThrow();
    Router router = context.getRouter().orElseThrow();

    assertSame(app.getStartupRouter(), router);
    assertInstanceOf(MemoryHistory.class, router.getHistory());
    assertEquals("/about", router.getHistory().getLocation().orElseThrow().getFullURI());
    assertEquals(1, router.getHistory().size());
    AboutView view =
        (AboutView) router.getRenderer().getRenderedComponent(AboutView.class).orElseThrow();
    assertEquals(1, view.getEnterCount());
  }

  @Test
  void shouldRejectHistoryConfigurationAfterStartup() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Router router = context.getRouter().orElseThrow();
    Object history = router.getHistory();
    AppAccessor accessor = AppAccessor.getDefault();
    App app = context.getApp().orElseThrow();
    MemoryHistory replacementHistory = new MemoryHistory();

    assertThrows(IllegalStateException.class,
        () -> accessor.setRouterHistory(app, replacementHistory));

    assertSame(history, router.getHistory());
  }
}
