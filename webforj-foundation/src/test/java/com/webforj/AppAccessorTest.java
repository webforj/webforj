package com.webforj;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjWebManager;
import com.typesafe.config.ConfigFactory;
import com.webforj.annotation.Routify;
import com.webforj.bridge.AppAccessor;
import com.webforj.environment.ObjectTable;
import com.webforj.environment.StringTable;
import com.webforj.router.Router;
import com.webforj.router.history.BrowserHistory;
import com.webforj.router.history.MemoryHistory;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AppAccessorTest {

  @Test
  void shouldUseConfiguredHistoryBeforeApplicationHooksRun() throws Exception {
    MemoryHistory history = new MemoryHistory();
    RoutedApp app = new RoutedApp();
    AppAccessor.getDefault().setRouterHistory(app, history);

    initialize(app);

    assertSame(history, app.routerAtWillRun.getHistory());
    assertSame(app.routerAtWillRun, app.routerAtRun);
  }

  @Test
  void shouldUseBrowserHistoryByDefault() throws Exception {
    RoutedApp app = new RoutedApp();

    initialize(app);

    assertInstanceOf(BrowserHistory.class, app.routerAtRun.getHistory());
  }

  @Test
  void shouldRejectChangesDuringAndAfterInitialization() throws Exception {
    RoutedApp app = new RoutedApp();
    MemoryHistory history = new MemoryHistory();
    AppAccessor.getDefault().setRouterHistory(app, history);
    app.checkHistoryLocked = true;

    initialize(app);

    AppAccessor accessor = AppAccessor.getDefault();
    MemoryHistory replacement = new MemoryHistory();
    assertThrows(IllegalStateException.class, () -> accessor.setRouterHistory(app, replacement));
    assertSame(history, app.routerAtRun.getHistory());
  }

  @Test
  void shouldRejectNullHistory() {
    AppAccessor accessor = AppAccessor.getDefault();
    RoutedApp app = new RoutedApp();

    assertThrows(NullPointerException.class, () -> accessor.setRouterHistory(app, null));
  }

  @Test
  void shouldKeepHistoryConfigurationOnItsApplication() throws Exception {
    RoutedApp configured = new RoutedApp();
    MemoryHistory history = new MemoryHistory();
    AppAccessor.getDefault().setRouterHistory(configured, history);
    initialize(configured);
    RoutedApp ordinary = new RoutedApp();

    initialize(ordinary);

    assertSame(history, configured.routerAtRun.getHistory());
    assertInstanceOf(BrowserHistory.class, ordinary.routerAtRun.getHistory());
  }

  private void initialize(RoutedApp app) throws Exception {
    Environment environment = mock(Environment.class);
    when(environment.getConfig()).thenReturn(ConfigFactory.empty());
    BBjAPI api = mock(BBjAPI.class);
    when(environment.getBBjAPI()).thenReturn(api);
    when(api.getWebManager()).thenReturn(mock(BBjWebManager.class));
    Page page = mock(Page.class);
    Map<String, Object> objects = new HashMap<>();
    try (MockedStatic<Environment> environments = mockStatic(Environment.class);
        MockedStatic<Page> pages = mockStatic(Page.class);
        MockedStatic<ObjectTable> objectTable = mockStatic(ObjectTable.class);
        MockedStatic<StringTable> stringTable = mockStatic(StringTable.class)) {
      environments.when(Environment::getCurrent).thenReturn(environment);
      environments.when(Environment::getContextPath).thenReturn("/");
      pages.when(Page::getCurrent).thenReturn(page);
      objectTable.when(() -> ObjectTable.put(anyString(), any())).thenAnswer(call -> {
        objects.put(call.getArgument(0), call.getArgument(1));
        return call.getArgument(1);
      });
      objectTable.when(() -> ObjectTable.contains(anyString()))
          .thenAnswer(call -> objects.containsKey(call.getArgument(0)));
      objectTable.when(() -> ObjectTable.get(anyString()))
          .thenAnswer(call -> objects.get(call.getArgument(0)));

      app.initialize();
    }
  }

  @Routify(packages = "com.webforj.accessortest.empty", initializeFrame = false,
      manageFramesVisibility = false)
  static class RoutedApp extends App {
    private Router routerAtWillRun;
    private Router routerAtRun;
    private boolean checkHistoryLocked;

    @Override
    protected void onWillRun() {
      routerAtWillRun = Router.getCurrent();
      if (checkHistoryLocked) {
        AppAccessor accessor = AppAccessor.getDefault();
        MemoryHistory replacement = new MemoryHistory();
        assertThrows(IllegalStateException.class,
            () -> accessor.setRouterHistory(this, replacement));
      }
    }

    @Override
    public void run() {
      routerAtRun = Router.getCurrent();
    }
  }
}
