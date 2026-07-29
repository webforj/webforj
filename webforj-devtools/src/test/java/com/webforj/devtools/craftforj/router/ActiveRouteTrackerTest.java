package com.webforj.devtools.craftforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.router.RoutePattern;
import com.webforj.router.RouteRegistry;
import com.webforj.router.Router;
import com.webforj.router.event.NavigateEvent;
import com.webforj.router.history.Location;
import com.webforj.router.history.ParametersBag;
import com.webforj.router.history.SegmentsBag;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class ActiveRouteTrackerTest {

  private ActiveRouteTracker tracker;
  private Router router;
  private RouteRegistry registry;
  private ListenerRegistration<NavigateEvent> registration;

  @SuppressWarnings("unchecked")
  @BeforeEach
  void setUp() {
    tracker = new ActiveRouteTracker();
    router = mock(Router.class);
    registry = mock(RouteRegistry.class);
    registration = mock(ListenerRegistration.class);

    when(router.getRegistry()).thenReturn(registry);
    when(router.addNavigateListener(any())).thenReturn(registration);
  }

  @Nested
  @DisplayName("attach")
  class Attach {

    @Test
    @DisplayName("Should not throw when router is null")
    void shouldNotThrowWhenRouterIsNull() {
      tracker.attach(null);
      assertNull(tracker.getCurrentState());
    }

    @Test
    @DisplayName("Should register navigate listener")
    void shouldRegisterNavigateListener() {
      when(router.getResolvedLocation()).thenReturn(Optional.empty());

      tracker.attach(router);

      verify(router).addNavigateListener(any());
    }

    @Test
    @DisplayName("Should initialize state from current location")
    void shouldInitializeStateFromCurrentLocation() {
      Location location = new Location(new SegmentsBag("/products/123"),
          new ParametersBag(Map.of("filter", "active")), "section1");

      RoutePattern pattern = mock(RoutePattern.class);
      when(pattern.getPattern()).thenReturn("/products/:id");
      when(pattern.getParameters("/products/123")).thenReturn(Map.of("id", "123"));

      when(router.getResolvedLocation()).thenReturn(Optional.of(location));
      when(router.getRoutePatternByLocation(location)).thenReturn(Optional.of(pattern));
      when(registry.getComponentByRoute("/products/:id")).thenReturn(Optional.empty());

      tracker.attach(router);

      assertNotNull(tracker.getCurrentState());
      assertEquals("/products/123?filter=active#section1",
          tracker.getCurrentState().getCurrentPath());
      assertEquals("section1", tracker.getCurrentState().getFragment());
      assertEquals("active", tracker.getCurrentState().getQueryParams().get("filter"));
    }

    @Test
    @DisplayName("Should extract wildcard params with key without asterisk")
    void shouldExtractWildcardParams() {
      Location location =
          new Location(new SegmentsBag("/docs/getting-started/intro"), new ParametersBag(), null);

      RoutePattern pattern = new RoutePattern("/docs/:path*");

      when(router.getResolvedLocation()).thenReturn(Optional.of(location));
      when(router.getRoutePatternByLocation(location)).thenReturn(Optional.of(pattern));
      when(registry.getComponentByRoute("/docs/:path*")).thenReturn(Optional.of(Component.class));
      when(registry.getComponentHierarchy(Component.class)).thenReturn(Optional.empty());

      tracker.attach(router);

      assertNotNull(tracker.getCurrentState());
      // Key should be "path" without asterisk
      assertEquals("getting-started/intro", tracker.getCurrentState().getParams().get("path"));
    }

    @Test
    @DisplayName("Should extract wildcard params with layout prefixes in pattern")
    void shouldExtractWildcardParamsWithLayoutPrefixes() {
      Location location = new Location(new SegmentsBag("/docs/hyyan"), new ParametersBag(), null);

      // Pattern includes layout prefixes which are filtered out by RoutePattern
      RoutePattern pattern = new RoutePattern("/@stress/@sub-stress/docs/:path*");

      when(router.getResolvedLocation()).thenReturn(Optional.of(location));
      when(router.getRoutePatternByLocation(location)).thenReturn(Optional.of(pattern));
      when(registry.getComponentByRoute("/@stress/@sub-stress/docs/:path*"))
          .thenReturn(Optional.of(Component.class));
      when(registry.getComponentHierarchy(Component.class)).thenReturn(Optional.empty());

      tracker.attach(router);

      assertNotNull(tracker.getCurrentState());
      assertEquals("hyyan", tracker.getCurrentState().getParams().get("path"));
    }

    @Test
    @DisplayName("Should have empty state when no resolved location")
    void shouldHaveEmptyStateWhenNoResolvedLocation() {
      when(router.getResolvedLocation()).thenReturn(Optional.empty());

      tracker.attach(router);

      assertNull(tracker.getCurrentState());
    }
  }

  @Nested
  @DisplayName("detach")
  class Detach {

    @Test
    @DisplayName("Should remove listener registration")
    void shouldRemoveListenerRegistration() {
      when(router.getResolvedLocation()).thenReturn(Optional.empty());

      tracker.attach(router);
      tracker.detach();

      verify(registration).remove();
    }

    @Test
    @DisplayName("Should clear current state")
    void shouldClearCurrentState() {
      Location location = new Location(new SegmentsBag("/test"), new ParametersBag(), null);
      RoutePattern pattern = mock(RoutePattern.class);
      when(pattern.getPattern()).thenReturn("/test");
      when(pattern.getParameters("/test")).thenReturn(Map.of());

      when(router.getResolvedLocation()).thenReturn(Optional.of(location));
      when(router.getRoutePatternByLocation(location)).thenReturn(Optional.of(pattern));
      when(registry.getComponentByRoute("/test")).thenReturn(Optional.empty());

      tracker.attach(router);
      assertNotNull(tracker.getCurrentState());

      tracker.detach();
      assertNull(tracker.getCurrentState());
    }

    @Test
    @DisplayName("Should not throw when not attached")
    void shouldNotThrowWhenNotAttached() {
      tracker.detach();
      assertNull(tracker.getCurrentState());
    }
  }
}
