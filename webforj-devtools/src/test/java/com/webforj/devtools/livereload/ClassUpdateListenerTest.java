package com.webforj.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.component.window.Window;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.conceiver.DefaultConceiver;
import com.webforj.event.page.PageEvent;
import com.webforj.event.page.PageEventOptions;
import com.webforj.router.RouteOutlet;
import com.webforj.router.RouteRegistry;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.router.history.MemoryHistory;
import com.webforj.router.observer.RouteRendererObserver;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class ClassUpdateListenerTest {

  private ClassUpdateListener listener;
  private Router router;
  private Page page;
  private MockedStatic<ConceiverProvider> mockedConceiverProvider;
  private MockedStatic<Environment> mockedEnvironment;
  private MockedStatic<Page> mockedPage;

  @BeforeEach
  void setUp() throws BBjException {
    mockedConceiverProvider = mockStatic(ConceiverProvider.class);
    when(ConceiverProvider.getCurrent()).thenReturn(new DefaultConceiver());

    RouteRegistry registry = new RouteRegistry();
    registry.register("/layout", MainLayoutView.class);
    registry.register("/layout/dashboard", DashboardView.class, MainLayoutView.class);
    registry.register("/other", OtherView.class);
    router = new Router(registry, new MemoryHistory());

    page = mock(Page.class);
    mockedPage = mockStatic(Page.class);
    mockedPage.when(Page::getCurrent).thenReturn(page);

    mockedEnvironment = mockStatic(Environment.class);
    mockedEnvironment.when(Environment::getCurrent).thenReturn(mock(Environment.class));
    BBjSysGui sysGui = mock(BBjSysGui.class);
    when(Environment.getCurrent().getSysGui()).thenReturn(sysGui);

    Frame frame = mock(Frame.class);
    when(frame.getName()).thenReturn("frame1");
    BBjTopLevelWindow window = mock(BBjTopLevelWindow.class);
    when(window.getUserData()).thenReturn(frame);
    when(sysGui.getWindows()).thenReturn(new BBjVector(List.of(window)));

    listener = new ClassUpdateListener();
  }

  @AfterEach
  void tearDown() {
    mockedEnvironment.close();
    mockedPage.close();
    mockedConceiverProvider.close();
  }

  @Test
  void shouldRebuildTheRenderedViewInPlace() {
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);
    Component layout = renderedComponent(MainLayoutView.class);

    listener.apply(Set.of(DashboardView.class.getName()), router, page);

    assertTrue(firstView.isDestroyed());
    assertNotSame(firstView, renderedComponent(DashboardView.class));
    assertFalse(layout.isDestroyed());
    assertSame(layout, renderedComponent(MainLayoutView.class));
    verify(page, never()).reload();
  }

  @Test
  void shouldRebuildFromTheTopmostAffectedNode() {
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);
    Component firstLayout = renderedComponent(MainLayoutView.class);

    listener.apply(Set.of(DashboardView.class.getName(), MainLayoutView.class.getName()), router,
        page);

    assertTrue(firstLayout.isDestroyed());
    assertTrue(firstView.isDestroyed());
    assertNotSame(firstLayout, renderedComponent(MainLayoutView.class));
    assertNotSame(firstView, renderedComponent(DashboardView.class));
    verify(page, never()).reload();
  }

  @Test
  void shouldReloadForTheClassOutsideTheRouteTree() {
    // Nothing is guessed about a class the route tree cannot account for, such as a service.
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);

    listener.apply(Set.of("com.example.InvoiceService"), router, page);

    verify(page).reload();
    assertFalse(firstView.isDestroyed());
    assertSame(firstView, renderedComponent(DashboardView.class));
  }

  @Test
  void shouldReloadWhenTheChangeTouchesTheTreeAndSomethingOutsideIt() {
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);

    listener.apply(Set.of(DashboardView.class.getName(), "com.example.InvoiceService"), router,
        page);

    verify(page).reload();
    assertFalse(firstView.isDestroyed());
  }

  @Test
  void shouldDoNothingWhenTheChangedRouteIsNotRenderedHere() {
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);

    listener.apply(Set.of(OtherView.class.getName()), router, page);

    verify(page, never()).reload();
    assertFalse(firstView.isDestroyed());
    assertSame(firstView, renderedComponent(DashboardView.class));
  }

  @Test
  void shouldReloadWhenTheRebuildIsVetoed() {
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);

    router.getRenderer().addObserver((component, event, context, cb) -> {
      if (component instanceof DashboardView
          && event == RouteRendererObserver.LifecycleEvent.BEFORE_DESTROY) {
        cb.accept(false);
      } else {
        cb.accept(true);
      }
    });

    listener.apply(Set.of(DashboardView.class.getName()), router, page);

    verify(page).reload();
    assertFalse(firstView.isDestroyed());
  }

  @Test
  void shouldReloadWhenTheApplicationHasNoRouter() {
    listener.apply(Set.of(DashboardView.class.getName()), null, page);

    verify(page).reload();
  }

  @Test
  void shouldReloadWhenNoRouteIsRendered() {
    listener.apply(Set.of(DashboardView.class.getName()), router, page);

    verify(page).reload();
  }

  @Test
  void shouldRegisterTheListenerWhenLiveReloadIsOn() {
    listener.register(new LiveReloadOptions().setEnabled(true), page);

    verify(page).addEventListener(eq(ClassUpdateListener.EVENT_TYPE), any(),
        any(PageEventOptions.class));
  }

  @Test
  void shouldNotRegisterWhenLiveReloadIsOff() {
    listener.register(new LiveReloadOptions(), page);

    verify(page, never()).addEventListener(any(), any(), any(PageEventOptions.class));
  }

  @Test
  void shouldHandTheParsedClassNamesOn() {
    ClassUpdateListener spied = spy(listener);
    doNothing().when(spied).apply(any(), any(), any());

    PageEvent event = mock(PageEvent.class);
    when(event.getData()).thenReturn(
        Map.of(ClassUpdateListener.DATA_KEY, "[\"" + DashboardView.class.getName() + "\"]"));

    try (MockedStatic<Router> mockedRouter = mockStatic(Router.class)) {
      mockedRouter.when(Router::getCurrent).thenReturn(router);

      spied.handleUpdate(event);
    }

    verify(spied).apply(eq(Set.of(DashboardView.class.getName())), eq(router), eq(page));
  }

  @Test
  void shouldIgnoreTheUnreadableUpdate() {
    ClassUpdateListener spied = spy(listener);

    PageEvent event = mock(PageEvent.class);
    when(event.getData()).thenReturn(Map.of(ClassUpdateListener.DATA_KEY, "{broken"));

    assertDoesNotThrow(() -> spied.handleUpdate(event));
    verify(spied, never()).apply(any(), any(), any());
  }

  @Test
  void shouldIgnoreTheUpdateWithoutClassNames() {
    ClassUpdateListener spied = spy(listener);

    PageEvent event = mock(PageEvent.class);
    when(event.getData()).thenReturn(Map.of());

    assertDoesNotThrow(() -> spied.handleUpdate(event));
    verify(spied, never()).apply(any(), any(), any());
  }

  private Component renderedComponent(Class<? extends Component> componentClass) {
    return router.getRenderer().getRenderedComponent(componentClass).orElseThrow();
  }

  public static class MainLayoutView extends Component implements RouteOutlet {
    @Override
    protected void onCreate(Window window) {
      // Do nothing
    }

    @Override
    protected void onDestroy() {
      // Do nothing
    }

    @Override
    public void showRouteContent(Component component) {
      // Do nothing
    }

    @Override
    public void removeRouteContent(Component component) {
      // Do nothing
    }
  }

  public static class DashboardView extends Component {
    @Override
    protected void onCreate(Window window) {
      // Do nothing
    }

    @Override
    protected void onDestroy() {
      // Do nothing
    }
  }

  public static class OtherView extends Component {
    @Override
    protected void onCreate(Window window) {
      // Do nothing
    }

    @Override
    protected void onDestroy() {
      // Do nothing
    }
  }
}
