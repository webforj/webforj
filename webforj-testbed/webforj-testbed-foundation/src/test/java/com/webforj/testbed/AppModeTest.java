package com.webforj.testbed;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.App;
import com.webforj.testbed.fixture.AboutView;
import com.webforj.testbed.fixture.MainView;
import com.webforj.testbed.fixture.TestApp;
import org.junit.jupiter.api.Test;

class AppModeTest extends TestbedTestSupport {

  @Override
  protected WebforjTestbedOptions options() {
    return new WebforjTestbedOptions().setApp(TestApp.class);
  }

  @Test
  void shouldLaunchTheApplication() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    App app = context.getApp().orElseThrow();

    assertInstanceOf(TestApp.class, app);
    assertTrue(((TestApp) app).hasRun());
  }

  @Test
  void shouldCreateTheFirstFrame() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    assertFalse(App.getFrames().isEmpty());
    assertNotNull(context.getFrame());
  }

  @Test
  void shouldDiscoverRoutesFromTheRoutifyAnnotation() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    MainView view = context.navigate(MainView.class);

    assertNotNull(view);
    assertEquals("Hello", view.getButton().getText());
  }

  @Test
  void shouldNavigateWithMemoryBackedHistory() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    context.navigate(AboutView.class);

    assertEquals("/about",
        context.getRouter().orElseThrow().getHistory().getLocation().orElseThrow().getFullURI());
  }

  @Test
  void shouldDispatchLifecycleEventsOnlyOncePerNavigation() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    AboutView view = context.navigate(AboutView.class);

    assertEquals(1, view.getEnterCount());
  }
}
