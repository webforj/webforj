package com.webforj.devtools.craftforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.devtools.craftforj.router.model.ActiveRouteState;
import com.webforj.devtools.craftforj.router.model.RouteInfo;
import com.webforj.devtools.craftforj.router.model.RouteType;
import com.webforj.devtools.craftforj.router.model.SecurityAccess;
import com.webforj.router.RouteEntry;
import com.webforj.router.RouteRegistry;
import com.webforj.router.Router;
import com.webforj.router.annotation.FrameTitle;
import com.webforj.router.annotation.Route;
import com.webforj.router.annotation.RouteAlias;
import com.webforj.router.event.DidEnterEvent;
import com.webforj.router.event.WillEnterEvent;
import com.webforj.router.history.ParametersBag;
import com.webforj.router.observer.DidEnterObserver;
import com.webforj.router.observer.WillEnterObserver;
import com.webforj.router.security.annotation.AnonymousAccess;
import jakarta.annotation.security.DenyAll;
import jakarta.annotation.security.PermitAll;
import jakarta.annotation.security.RolesAllowed;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class RouteCollectorTest {

  private Router mockRouter;
  private RouteRegistry mockRegistry;

  @BeforeEach
  void setUp() {
    mockRouter = mock(Router.class);
    mockRegistry = mock(RouteRegistry.class);
    when(mockRouter.getRegistry()).thenReturn(mockRegistry);
  }

  private RouteEntry createEntry(String path, Class<? extends Component> component) {
    return new RouteEntry(path, component);
  }

  @SuppressWarnings("unchecked")
  private RouteEntry createEntry(String path, Class<? extends Component> component,
      Class<?> outlet) {
    return new RouteEntry(path, component, (Class<? extends Component>) outlet, 0);
  }

  @Nested
  @DisplayName("collectRoutes")
  class CollectRoutes {

    @Test
    @DisplayName("Should collect single route")
    void shouldCollectSingleRoute() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/home", SimpleView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(1, routes.size());
      assertEquals("/home", routes.get(0).getPath());
      assertEquals("SimpleView", routes.get(0).getDisplayName());
    }

    @Test
    @DisplayName("Should build parent-child tree")
    void shouldBuildParentChildTree() {
      RouteEntry layoutEntry = createEntry("@/main", MainLayout.class);
      RouteEntry viewEntry = createEntry("/dashboard", DashboardView.class, MainLayout.class);

      when(mockRegistry.getAvailableRouteEntires()).thenReturn(List.of(layoutEntry, viewEntry));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(1, routes.size());
      RouteInfo layout = routes.get(0);
      assertEquals("@/main", layout.getPath());
      assertEquals(1, layout.getChildren().size());
      assertEquals("/dashboard", layout.getChildren().get(0).getPath());
    }

    @Test
    @DisplayName("Should sort routes by path")
    void shouldSortRoutesByPath() {
      when(mockRegistry.getAvailableRouteEntires()).thenReturn(List.of(
          createEntry("/z-route", SimpleView.class), createEntry("/a-route", AnotherView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(2, routes.size());
      assertEquals("/a-route", routes.get(0).getPath());
      assertEquals("/z-route", routes.get(1).getPath());
    }

    @Test
    @DisplayName("Should return empty list when registry has no routes")
    void shouldReturnEmptyWhenNoRoutes() {
      when(mockRegistry.getAvailableRouteEntires()).thenReturn(Collections.emptyList());

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertTrue(routes.isEmpty());
    }

    @Test
    @DisplayName("Should attach children to every entry of a duplicated layout class")
    void shouldAttachChildrenToAllDuplicateParents() {
      RouteEntry layoutA = createEntry("@/a", MainLayout.class);
      RouteEntry layoutB = createEntry("@/b", MainLayout.class);
      RouteEntry child = createEntry("/dashboard", DashboardView.class, MainLayout.class);

      when(mockRegistry.getAvailableRouteEntires()).thenReturn(List.of(layoutA, layoutB, child));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(2, routes.size());
      for (RouteInfo layout : routes) {
        assertEquals(1, layout.getChildren().size());
        assertEquals("/dashboard", layout.getChildren().get(0).getPath());
      }
    }
  }

  @Nested
  @DisplayName("Active route detection")
  class ActiveRouteDetection {

    @Test
    @DisplayName("Should mark a route active when the tracker reports its id")
    void shouldMarkActiveRoute() {
      when(mockRegistry.getAvailableRouteEntires()).thenReturn(List
          .of(createEntry("/home", SimpleView.class), createEntry("/other", AnotherView.class)));

      ActiveRouteTracker tracker = mock(ActiveRouteTracker.class);
      ActiveRouteState state = new ActiveRouteState();
      state.setActiveRouteIds(List.of(SimpleView.class.getName() + ":/home"));
      when(tracker.getCurrentState()).thenReturn(state);

      RouteCollector collector = new RouteCollector(mockRouter, tracker);
      List<RouteInfo> routes = collector.collectRoutes();

      assertTrue(routes.stream().filter(r -> r.getPath().equals("/home")).findFirst().orElseThrow()
          .isActive());
      assertFalse(routes.stream().filter(r -> r.getPath().equals("/other")).findFirst()
          .orElseThrow().isActive());
    }

    @Test
    @DisplayName("Should report inactive without a tracker")
    void shouldReportInactiveWithoutTracker() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/home", SimpleView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertFalse(routes.get(0).isActive());
    }
  }

  @Nested
  @DisplayName("Route type extraction")
  class RouteTypeExtraction {

    @Test
    @DisplayName("Should detect layout from @ prefix")
    void shouldDetectLayoutFromPrefix() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("@/layout", MainLayout.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(RouteType.LAYOUT, routes.get(0).getType());
    }

    @Test
    @DisplayName("Should detect view for regular path")
    void shouldDetectViewForRegularPath() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/page", SimpleView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(RouteType.VIEW, routes.get(0).getType());
    }

    @Test
    @DisplayName("Should detect layout from Route.Type.LAYOUT annotation")
    void shouldDetectLayoutFromAnnotation() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/annotated-layout", AnnotatedComponent.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(RouteType.LAYOUT, routes.get(0).getType());
    }
  }

  @Nested
  @DisplayName("Security extraction")
  class SecurityExtraction {

    @Test
    @DisplayName("Should detect @PermitAll annotation")
    void shouldDetectPermitAll() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/public", PermitAllView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(SecurityAccess.PERMIT_ALL, routes.get(0).getSecurity());
    }

    @Test
    @DisplayName("Should detect @DenyAll annotation")
    void shouldDetectDenyAll() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/denied", DenyAllView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(SecurityAccess.DENY_ALL, routes.get(0).getSecurity());
    }

    @Test
    @DisplayName("Should detect @RolesAllowed annotation and extract roles")
    void shouldDetectRolesAllowed() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/admin", RolesAllowedView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(SecurityAccess.ROLES_ALLOWED, routes.get(0).getSecurity());
      assertEquals(Arrays.asList("ADMIN", "MANAGER"), routes.get(0).getAllowedRoles());
    }

    @Test
    @DisplayName("Should detect @AnonymousAccess annotation")
    void shouldDetectAnonymousAccess() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/open", AnonymousView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(SecurityAccess.ANONYMOUS, routes.get(0).getSecurity());
    }

    @Test
    @DisplayName("Should return NONE when no security annotation")
    void shouldReturnNoneWhenNoAnnotation() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/page", SimpleView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(SecurityAccess.NONE, routes.get(0).getSecurity());
      assertTrue(routes.get(0).getAllowedRoles().isEmpty());
    }
  }

  @Nested
  @DisplayName("Frame title extraction")
  class FrameTitleExtraction {

    @Test
    @DisplayName("Should extract @FrameTitle annotation")
    void shouldExtractFrameTitle() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/titled", TitledView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals("My Page Title", routes.get(0).getFrameTitle());
    }

    @Test
    @DisplayName("Should return null when no @FrameTitle")
    void shouldReturnNullWhenNoFrameTitle() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/page", SimpleView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertNull(routes.get(0).getFrameTitle());
    }
  }

  @Nested
  @DisplayName("Alias extraction")
  class AliasExtraction {

    @Test
    @DisplayName("Should extract @RouteAlias annotations")
    void shouldExtractAliases() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/main", AliasedView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(2, routes.get(0).getAliases().size());
      assertEquals("/alias1", routes.get(0).getAliases().get(0).getPath());
      assertEquals("/alias2", routes.get(0).getAliases().get(1).getPath());
    }

    @Test
    @DisplayName("Should return empty list when no aliases")
    void shouldReturnEmptyWhenNoAliases() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/page", SimpleView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertTrue(routes.get(0).getAliases().isEmpty());
    }
  }

  @Nested
  @DisplayName("Observer detection")
  class ObserverDetection {

    @Test
    @DisplayName("Should detect WillEnterObserver and DidEnterObserver")
    void shouldDetectObservers() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/observed", ObserverView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertTrue(routes.get(0).hasWillEnterObserver());
      assertTrue(routes.get(0).hasDidEnterObserver());
    }

    @Test
    @DisplayName("Should return false when no observers implemented")
    void shouldReturnFalseWhenNoObservers() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/page", SimpleView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertFalse(routes.get(0).hasWillEnterObserver());
      assertFalse(routes.get(0).hasDidEnterObserver());
      assertFalse(routes.get(0).hasWillLeaveObserver());
      assertFalse(routes.get(0).hasDidLeaveObserver());
      assertFalse(routes.get(0).hasActivateObserver());
    }
  }

  @Nested
  @DisplayName("Route parameters")
  class RouteParameters {

    @Test
    @DisplayName("Should parse route parameters")
    void shouldParseRouteParams() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/users/:id/posts/:postId?", SimpleView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertEquals(2, routes.get(0).getParams().size());
      assertEquals("id", routes.get(0).getParams().get(0).getName());
      assertFalse(routes.get(0).getParams().get(0).isOptional());
      assertEquals("postId", routes.get(0).getParams().get(1).getName());
      assertTrue(routes.get(0).getParams().get(1).isOptional());
    }
  }

  @Nested
  @DisplayName("Route ID generation")
  class RouteIdGeneration {

    @Test
    @DisplayName("Should generate correct route ID")
    void shouldGenerateCorrectRouteId() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/home", SimpleView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      String expectedId = SimpleView.class.getName() + ":/home";
      assertEquals(expectedId, routes.get(0).getId());
    }
  }

  // Test component classes
  @NodeName("simple-view")
  static class SimpleView extends ElementCompositeContainer {
  }

  @kotlin.Metadata
  @NodeName("kotlin-view")
  static class KotlinView extends ElementCompositeContainer {
  }

  @NodeName("another-view")
  static class AnotherView extends ElementCompositeContainer {
  }

  @NodeName("main-layout")
  static class MainLayout extends ElementCompositeContainer {
  }

  @NodeName("annotated-layout")
  @Route(value = "/annotated-layout", type = Route.Type.LAYOUT)
  static class AnnotatedComponent extends ElementCompositeContainer {
  }

  @NodeName("dashboard-view")
  static class DashboardView extends ElementCompositeContainer {
  }

  @NodeName("permit-all-view")
  @PermitAll
  static class PermitAllView extends ElementCompositeContainer {
  }

  @NodeName("deny-all-view")
  @DenyAll
  static class DenyAllView extends ElementCompositeContainer {
  }

  @NodeName("roles-allowed-view")
  @RolesAllowed({"ADMIN", "MANAGER"})
  static class RolesAllowedView extends ElementCompositeContainer {
  }

  @NodeName("anonymous-view")
  @AnonymousAccess
  static class AnonymousView extends ElementCompositeContainer {
  }

  @NodeName("titled-view")
  @FrameTitle("My Page Title")
  static class TitledView extends ElementCompositeContainer {
  }

  @NodeName("aliased-view")
  @RouteAlias("/alias1")
  @RouteAlias("/alias2")
  static class AliasedView extends ElementCompositeContainer {
  }

  @NodeName("observer-view")
  static class ObserverView extends ElementCompositeContainer
      implements WillEnterObserver, DidEnterObserver {
    @Override
    public void onWillEnter(WillEnterEvent event, ParametersBag parameters) {
      // No-op
    }

    @Override
    public void onDidEnter(DidEnterEvent event, ParametersBag parameters) {
      // No-op
    }
  }

  @Nested
  @DisplayName("kotlin")
  class Kotlin {

    @Test
    @DisplayName("Should flag a route whose class was compiled from Kotlin")
    void shouldFlagKotlinRoute() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/kotlin", KotlinView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertTrue(routes.get(0).isKotlin());
    }

    @Test
    @DisplayName("Should leave a Java route unflagged")
    void shouldLeaveJavaRouteUnflagged() {
      when(mockRegistry.getAvailableRouteEntires())
          .thenReturn(List.of(createEntry("/home", SimpleView.class)));

      RouteCollector collector = new RouteCollector(mockRouter);
      List<RouteInfo> routes = collector.collectRoutes();

      assertFalse(routes.get(0).isKotlin());
    }
  }
}
