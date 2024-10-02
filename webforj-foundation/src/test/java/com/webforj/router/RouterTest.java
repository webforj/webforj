package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
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
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.window.Frame;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.conceiver.DefaultConceiver;
import com.webforj.dispatcher.EventListener;
import com.webforj.router.event.DidEnterEvent;
import com.webforj.router.event.DidLeaveEvent;
import com.webforj.router.event.NavigateEvent;
import com.webforj.router.event.WillEnterEvent;
import com.webforj.router.event.WillLeaveEvent;
import com.webforj.router.exception.NotFoundException;
import com.webforj.router.history.History;
import com.webforj.router.history.Location;
import com.webforj.router.history.MemoryHistory;
import com.webforj.router.history.ParametersBag;
import com.webforj.router.observer.DidEnterObserver;
import com.webforj.router.observer.DidLeaveObserver;
import com.webforj.router.observer.WillEnterObserver;
import com.webforj.router.observer.WillLeaveObserver;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;


class RouterTest {

  MockedStatic<ConceiverProvider> mockedConceiverProvider;
  History history;
  RouteRegistry registry;
  MockedStatic<Environment> mockedEnvironment;
  MockedStatic<Page> mockedPage;
  Router router;

  @BeforeEach
  void setUp() throws BBjException {
    mockedConceiverProvider = mockStatic(ConceiverProvider.class);
    when(ConceiverProvider.getCurrent()).thenReturn(new DefaultConceiver());

    history = new MemoryHistory();
    registry = new RouteRegistry();
    registry.register("/main", MainView.class);
    registry.register("/about", AboutView.class);
    registry.register("/main/:id", PageView.class, MainView.class);
    router = new Router(registry, history);

    mockedPage = mockStatic(Page.class);
    mockedPage.when(Page::getCurrent).thenReturn(mock(Page.class));

    mockedEnvironment = mockStatic(Environment.class);
    mockedEnvironment.when(Environment::getCurrent).thenReturn(mock(Environment.class));
    BBjSysGui sysGui = mock(BBjSysGui.class);
    when(Environment.getCurrent().getSysGui()).thenReturn(sysGui);

    // Setup mock windows and their user data
    Frame f1 = mock(Frame.class);
    Frame f2 = mock(Frame.class);

    when(f1.getName()).thenReturn("frame1");
    when(f2.getName()).thenReturn("frame2");

    BBjTopLevelWindow w1 = mock(BBjTopLevelWindow.class);
    BBjTopLevelWindow w2 = mock(BBjTopLevelWindow.class);
    when(w1.getUserData()).thenReturn(f1);
    when(w2.getUserData()).thenReturn(f2);

    // Mock the windows returned by sysGui
    when(sysGui.getWindows()).thenReturn(new BBjVector(List.of(w1, w2)));
  }

  @AfterEach
  public void teardown() {
    mockedEnvironment.close();
    mockedPage.close();
    mockedConceiverProvider.close();
  }

  @Nested
  class LocationNavigation {
    @Test
    void shouldThrowExceptionWhenNavigateToInvalidLocation() {
      assertThrows(NotFoundException.class, () -> router.navigate(new Location("/invalidPath")));
    }

    @Test
    void shouldNavigateToValidLocation() {
      AtomicBoolean onCompleteCalled = new AtomicBoolean(false);
      AtomicReference<Location> currentLocation = new AtomicReference<>();

      Location location = new Location("/main");
      Consumer<Optional<? extends Component>> onComplete = component -> {
        onCompleteCalled.set(true);
        currentLocation.set(history.getLocation().orElse(null));
      };

      router.navigate(location, onComplete);

      assertTrue(onCompleteCalled.get());
      assertEquals("/main", currentLocation.get().getFullURI());
    }

    @Test
    void shouldNavigateToLocationWithOptionsAndCallback() {
      AtomicBoolean onCompleteCalled = new AtomicBoolean(false);
      NavigationOptions options = new NavigationOptions();
      Consumer<Optional<? extends Component>> onComplete = component -> onCompleteCalled.set(true);

      Location location = new Location("/main");
      router.navigate(location, options, onComplete);

      assertTrue(onCompleteCalled.get());
      assertEquals("/main", history.getLocation().orElseThrow().getFullURI());
    }

    @Test
    void shouldNavigateToLocationWithOptionsOnly() {
      NavigationOptions options = new NavigationOptions();

      Location location = new Location("/main");
      router.navigate(location, options);

      assertEquals("/main", history.getLocation().orElseThrow().getFullURI());
    }

    @Test
    void shouldNavigateToLocationWithCallbackOnly() {
      AtomicBoolean onCompleteCalled = new AtomicBoolean(false);
      Consumer<Optional<? extends Component>> onComplete = component -> onCompleteCalled.set(true);

      Location location = new Location("/main");
      router.navigate(location, onComplete);

      assertTrue(onCompleteCalled.get());
      assertEquals("/main", history.getLocation().orElseThrow().getFullURI());
    }

    @Test
    void shouldNavigateToLocationWithoutAdditionalOptions() {
      Location location = new Location("/main");
      router.navigate(location);

      assertEquals("/main", history.getLocation().orElseThrow().getFullURI());
    }
  }

  @Nested
  class ComponentNavigation {
    @Test
    void shouldNavigateToComponentWithParamsAndOptionsAndCallback() {
      AtomicBoolean onCompleteCalled = new AtomicBoolean(false);
      ParametersBag params = new ParametersBag();
      params.put("id", "123");


      NavigationOptions options = new NavigationOptions();
      Consumer<Optional<PageView>> onComplete = component -> onCompleteCalled.set(true);

      router.navigate(PageView.class, options, params, onComplete);

      assertTrue(onCompleteCalled.get());
      assertEquals("/main/123", history.getLocation().orElseThrow().getFullURI());
    }

    @Test
    void shouldNavigateToComponentWithParamsAndOptions() {
      ParametersBag params = new ParametersBag();
      params.put("id", "123");

      NavigationOptions options = new NavigationOptions();

      router.navigate(PageView.class, options, params);

      assertEquals("/main/123", history.getLocation().orElseThrow().getFullURI());
    }

    @Test
    void shouldNavigateToComponentWithParamsAndCallback() {
      AtomicBoolean onCompleteCalled = new AtomicBoolean(false);
      ParametersBag params = new ParametersBag();
      params.put("id", "123");

      Consumer<Optional<PageView>> onComplete = component -> onCompleteCalled.set(true);

      router.navigate(PageView.class, params, onComplete);

      assertTrue(onCompleteCalled.get());
      assertEquals("/main/123", history.getLocation().orElseThrow().getFullURI());
    }

    @Test
    void shouldNavigateToComponentWithParamsOnly() {
      ParametersBag params = new ParametersBag();
      params.put("id", "123");

      router.navigate(PageView.class, params);

      assertEquals("/main/123", history.getLocation().orElseThrow().getFullURI());
    }

    @Test
    void shouldNavigateToComponentWithOptionsAndCallback() {
      AtomicBoolean onCompleteCalled = new AtomicBoolean(false);
      NavigationOptions options = new NavigationOptions();
      Consumer<Optional<MainView>> onComplete = component -> onCompleteCalled.set(true);

      router.navigate(MainView.class, options, onComplete);

      assertTrue(onCompleteCalled.get());
      assertEquals("/main", history.getLocation().orElseThrow().getFullURI());
    }

    @Test
    void shouldNavigateToComponentWithOptionsOnly() {
      NavigationOptions options = new NavigationOptions();

      router.navigate(MainView.class, options);

      assertEquals("/main", history.getLocation().orElseThrow().getFullURI());
    }

    @Test
    void shouldNavigateToComponentWithCallbackOnly() {
      AtomicBoolean onCompleteCalled = new AtomicBoolean(false);
      Consumer<Optional<MainView>> onComplete = component -> onCompleteCalled.set(true);

      router.navigate(MainView.class, onComplete);

      assertTrue(onCompleteCalled.get());
      assertEquals("/main", history.getLocation().orElseThrow().getFullURI());
    }

    @Test
    void shouldNavigateToComponentWithoutAdditionalOptions() {
      router.navigate(MainView.class);

      assertEquals("/main", history.getLocation().orElseThrow().getFullURI());
    }
  }

  @Nested
  class Events {
    @Test
    void shouldHandleEnterListeners() {
      EventListener<WillEnterEvent> willEnterListener = mock(EventListener.class);
      EventListener<DidEnterEvent> didEnterListener = mock(EventListener.class);
      router.onWillEnter(willEnterListener);
      router.onDidEnter(didEnterListener);

      Location location = new Location("/main");
      Consumer<Optional<? extends Component>> onComplete = component -> {
        verify(willEnterListener, atLeast(1)).onEvent(any(WillEnterEvent.class));
        verify(didEnterListener, atLeast(1)).onEvent(any(DidEnterEvent.class));
      };

      router.navigate(location, onComplete);
    }

    @Test
    void shouldHandleLeaveListeners() {
      router.navigate(new Location("/about"), c -> {
        EventListener<WillLeaveEvent> willLeaveListener = mock(EventListener.class);
        EventListener<DidLeaveEvent> didLeaveListener = mock(EventListener.class);
        router.onWillLeave(willLeaveListener);
        router.onDidLeave(didLeaveListener);

        Location location = new Location("/main");
        Consumer<Optional<? extends Component>> onComplete = component -> {
          verify(willLeaveListener, atLeast(1)).onEvent(any(WillLeaveEvent.class));
          verify(didLeaveListener, atLeast(1)).onEvent(any(DidLeaveEvent.class));
        };

        router.navigate(location, onComplete);
      });
    }

    @Test
    void shouldHandleNavigateListeners() {
      EventListener<NavigateEvent> didNavigateListener = mock(EventListener.class);
      router.onNavigate(didNavigateListener);


      Location location = new Location("/main");
      Consumer<Optional<? extends Component>> onComplete = component -> {
        verify(didNavigateListener, atLeast(1)).onEvent(any(NavigateEvent.class));
      };

      router.navigate(location, onComplete);
    }
  }

  @Nested
  class Observers {
    @Test
    void shouldHandleObservers() {
      AtomicReference<PageView> pageViewRef = new AtomicReference<>();
      router.navigate(new Location("/main/123"), c -> {
        PageView pageView = (PageView) c.get();
        pageViewRef.set(pageView);

        assertTrue(pageView.onWillEnterInvoked);
        assertTrue(pageView.onDidEnterInvoked);
      });

      router.navigate(new Location("/main"), c -> {
        assertTrue(pageViewRef.get().onWillLeaveInvoked);
        assertTrue(pageViewRef.get().onDidLeaveInvoked);
      });
    }
  }

  @Nested
  class NavigationOptionsBehavior {
    @BeforeEach
    void setup() {
      history = spy(history);
      router = new Router(registry, history);
    }

    @Test
    void shouldPushToHistoryWhenNavigationTypeIsPush() {
      NavigationOptions options =
          new NavigationOptions().setNavigationType(NavigationOptions.NavigationType.PUSH);

      router.navigate(new Location("/main"), options);

      assertEquals("/main", history.getLocation().orElseThrow().getFullURI());
      verify(history).pushState(any(), any());
    }

    @Test
    void shouldReplaceHistoryWhenNavigationTypeIsReplace() {
      NavigationOptions options =
          new NavigationOptions().setNavigationType(NavigationOptions.NavigationType.REPLACE);

      router.navigate(new Location("/main"), options);
      assertEquals("/main", history.getLocation().orElseThrow().getFullURI());
      verify(history).replaceState(any(), any());
    }

    @Test
    void shouldNotFireEventsWhenFireEventsIsFalse() {
      AtomicBoolean willEnterEventFired = new AtomicBoolean(false);
      AtomicBoolean didEnterEventFired = new AtomicBoolean(false);
      AtomicBoolean didNavigateEventFired = new AtomicBoolean(false);
      AtomicBoolean willLeaveEventFired = new AtomicBoolean(false);
      AtomicBoolean didLeaveEventFired = new AtomicBoolean(false);

      router.onWillEnter(event -> willEnterEventFired.set(true));
      router.onDidEnter(event -> didEnterEventFired.set(true));
      router.onNavigate(event -> didNavigateEventFired.set(true));
      router.onWillLeave(event -> willLeaveEventFired.set(true));
      router.onDidLeave(event -> didLeaveEventFired.set(true));

      NavigationOptions options = new NavigationOptions().setFireEvents(false);

      router.navigate(new Location("/main"), options);

      assertFalse(willEnterEventFired.get());
      assertFalse(didEnterEventFired.get());
      assertFalse(didNavigateEventFired.get());

      assertFalse(didLeaveEventFired.get());
    }

    @Test
    void shouldNotInvokeObserversWhenInvokeObserversIsFalse() {
      NavigationOptions options = new NavigationOptions().setInvokeObservers(false);

      ParametersBag params = new ParametersBag();
      params.put("id", "123");

      router.navigate(PageView.class, options, params, c -> {
        PageView pageView = (PageView) c.get();
        assertFalse(pageView.onWillEnterInvoked);
        assertFalse(pageView.onDidEnterInvoked);
        assertFalse(pageView.onWillLeaveInvoked);
        assertFalse(pageView.onDidLeaveInvoked);
      });
    }

    @Test
    void shouldNotUpdateHistoryWhenUpdateHistoryIsFalse() {
      NavigationOptions options = new NavigationOptions().setUpdateHistory(false);

      router.navigate(new Location("/about"));
      router.navigate(new Location("/main"), options);

      assertEquals("/about", history.getLocation().orElseThrow().getFullURI());
    }

    @Test
    void shouldPassStateObjectToHistory() {
      Object state = new Object();
      NavigationOptions options = new NavigationOptions().setState(state)
          .setNavigationType(NavigationOptions.NavigationType.PUSH);

      router.navigate(new Location("/main"), options);

      verify(history).pushState(eq(state), any());
    }
  }

  @Nested
  class RouteRetrieval {

    @Test
    void shouldReturnRoutePatternForValidLocation() {
      Location location = new Location("/main");
      Optional<RoutePattern> routePattern = router.getRoutePatternByLocation(location);
      assertTrue(routePattern.isPresent());
      assertEquals("/main", routePattern.get().getPattern());
    }

    @Test
    void shouldReturnEmptyOptionalForInvalidLocation() {
      Location location = new Location("/invalidPath");
      Optional<RoutePattern> routePattern = router.getRoutePatternByLocation(location);
      assertFalse(routePattern.isPresent());
    }

    @Test
    void shouldReturnLocationForComponentWithParams() {
      ParametersBag params = new ParametersBag();
      params.put("id", "123");

      Optional<Location> location = router.getLocation(PageView.class, params);
      assertTrue(location.isPresent());
      assertEquals("/main/123", location.get().getFullURI());
    }

    @Test
    void shouldReturnLocationForComponentWithoutParams() {
      Optional<Location> location = router.getLocation(MainView.class);
      assertTrue(location.isPresent());
      assertEquals("/main", location.get().getFullURI());
    }

    @Test
    void shouldReturnEmptyOptionalForComponentWithoutRoute() {
      Optional<Location> location = router.getLocation(NonRegisteredView.class);
      assertFalse(location.isPresent());
    }

    @Test
    void shouldReturnUriForComponentWithParams() {
      ParametersBag params = new ParametersBag();
      params.put("id", "123");

      Optional<String> uri = router.getUri(PageView.class, params);
      assertTrue(uri.isPresent());
      assertEquals("/main/123", uri.get());
    }

    @Test
    void shouldReturnUriForComponentWithoutParams() {
      Optional<String> uri = router.getUri(MainView.class);
      assertTrue(uri.isPresent());
      assertEquals("/main", uri.get());
    }

    @Test
    void shouldReturnEmptyOptionalForComponentWithoutRouteUri() {
      Optional<String> uri = router.getUri(NonRegisteredView.class);
      assertFalse(uri.isPresent());
    }

    @Test
    void shouldReturnLastResolvedLocation() {
      Location location = new Location("/main");
      router.navigate(location);
      Optional<Location> resolvedLocation = router.getResolvedLocation();
      assertTrue(resolvedLocation.isPresent());
      assertEquals("/main", resolvedLocation.get().getFullURI());
    }

    @Test
    void shouldReturnEmptyOptionalWhenNoResolvedLocation() {
      Optional<Location> resolvedLocation = router.getResolvedLocation();
      assertFalse(resolvedLocation.isPresent());
    }
  }

  @NodeName("view-nonregistered")
  public static class NonRegisteredView extends ElementCompositeContainer {
    // Mock implementation
  }


  @NodeName("view-about")
  public static class AboutView extends ElementCompositeContainer {
    // Mock implementation
  }

  @NodeName("view-main")
  public static class MainView extends ElementCompositeContainer {
    // Mock implementation
  }

  @NodeName("view-page")
  public static class PageView extends ElementCompositeContainer
      implements WillEnterObserver, DidEnterObserver, WillLeaveObserver, DidLeaveObserver {
    boolean onDidLeaveInvoked = false;
    boolean onWillLeaveInvoked = false;
    boolean onDidEnterInvoked = false;
    boolean onWillEnterInvoked = false;

    @Override
    public void onDidEnter(DidEnterEvent event, ParametersBag parameters) {
      onDidEnterInvoked = true;
    }

    @Override
    public void onWillEnter(WillEnterEvent event, ParametersBag parameters) {
      onWillEnterInvoked = true;
      event.accept();
    }

    @Override
    public void onDidLeave(DidLeaveEvent event, ParametersBag parameters) {
      onDidLeaveInvoked = true;
    }

    @Override
    public void onWillLeave(WillLeaveEvent event, ParametersBag parameters) {
      onWillLeaveInvoked = true;
      event.accept();
    }
  }
}
