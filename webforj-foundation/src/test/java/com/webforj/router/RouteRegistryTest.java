package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.component.window.Window;
import com.webforj.data.tree.Vnode;
import com.webforj.router.annotation.Route;
import com.webforj.router.annotation.RouteAlias;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class RouteRegistryTest {

  private RouteRegistry routeRegistry;

  @BeforeEach
  void setUp() {
    routeRegistry = new RouteRegistry();
  }

  @Nested
  class RegisteringRoutes {
    @Test
    void shouldRegisterRouteWithComponentAndTargetAndFrameId() {
      Class<TestView> view = TestView.class;
      Class<Frame> target = Frame.class;
      routeRegistry.register("test", view, target, "frame1");

      assertEquals(view, routeRegistry.getComponentByRoute("test").get());
      assertEquals(target, routeRegistry.getTarget(TestView.class).get());
      assertEquals("frame1", routeRegistry.getFrameRouteId(view).get());
    }

    @Test
    void shouldRegisterRouteWithComponentAndTarget() {
      Class<TestView> view = TestView.class;
      Class<TargetView> target = TargetView.class;
      routeRegistry.register("test", view, target);

      assertEquals(view, routeRegistry.getComponentByRoute("test").get());
      assertEquals(target, routeRegistry.getTarget(TestView.class).get());
      assertFalse(routeRegistry.getFrameRouteId(TestView.class).isPresent());
    }

    @Test
    void shouldRegisterRouteWithComponent() {
      Class<TestView> view = TestView.class;
      routeRegistry.register("test", view);

      assertEquals(view, routeRegistry.getComponentByRoute("test").get());
      assertEquals(Frame.class, routeRegistry.getTarget(TestView.class).get());
    }

    @Test
    void shouldRegisterAnnotatedRoute() {
      routeRegistry.registerAnnotated(TestView.class);

      String path = "target/test";
      assertTrue(routeRegistry.getComponentByRoute(path).isPresent());
      assertEquals(TestView.class, routeRegistry.getComponentByRoute(path).get());
    }

    @Test
    void shouldRegisterAnnotatedRouteWithAlias() {
      routeRegistry.registerAnnotated(TargetView.class);

      assertTrue(routeRegistry.getComponentByRoute("alias1").isPresent());
      assertEquals(TargetView.class, routeRegistry.getComponentByRoute("alias1").get());

      assertTrue(routeRegistry.getComponentByRoute("alias2").isPresent());
      assertEquals(TargetView.class, routeRegistry.getComponentByRoute("alias2").get());
    }
  }

  @Nested
  class QueryRoutes {
    @Test
    void shouldGetComponentByRoute() {
      routeRegistry.register("test", TestView.class);

      assertEquals(TestView.class, routeRegistry.getComponentByRoute("test").get());
      assertFalse(routeRegistry.getComponentByRoute("/nonexistent").isPresent());
    }

    @Test
    void shouldGetRouteByComponent() {
      routeRegistry.register("test", TestView.class);

      assertEquals("test", routeRegistry.getRouteByComponent(TestView.class).get());
      assertFalse(routeRegistry.getRouteByComponent(TargetView.class).isPresent());
    }

    @Test
    void shouldGetResolvedComponentByRoute() {
      routeRegistry.register("parent", TargetView.class);
      routeRegistry.register("parent/child", TestView.class, TargetView.class);

      Class<? extends Component> rootNode = routeRegistry.getComponentByRoute("parent/child").get();

      assertEquals(TestView.class, rootNode);
    }

    @Test
    void shouldGetResolvedRoutes() {
      routeRegistry.register("parent", TargetView.class);
      routeRegistry.register("parent/child", TestView.class, TargetView.class);

      List<RouteEntry> resolvedRoutes = routeRegistry.getAvailableRoutes();
      List<String> routePaths = resolvedRoutes.stream().map(RouteEntry::getPath).toList();

      assertEquals(2, resolvedRoutes.size());
      assertTrue(routePaths.contains("parent"));
      assertTrue(routePaths.contains("parent/child"));
    }

    @Test
    void shouldGetComponentsTree() {
      routeRegistry.register("parent", TargetView.class);
      routeRegistry.register("parent/child", TestView.class, TargetView.class);

      Optional<Vnode<Class<? extends Component>>> tree =
          routeRegistry.getComponentsTree(TestView.class);

      assertTrue(tree.isPresent());
      Vnode<Class<? extends Component>> rootNode = tree.get();

      assertEquals(Frame.class, rootNode.getData());
      assertEquals(1, rootNode.getChildren().size());
      assertEquals(TargetView.class, rootNode.getChildren().get(0).getData());
      assertEquals(TestView.class, rootNode.getChildren().get(0).getChildren().get(0).getData());
    }
  }

  @Test
  void shouldClearRegistry() {
    routeRegistry.register("test", TestView.class);

    routeRegistry.clear();

    assertFalse(routeRegistry.getComponentByRoute("test").isPresent());
    assertFalse(routeRegistry.getTarget(TestView.class).isPresent());
    assertFalse(routeRegistry.getFrameRouteId(TestView.class).isPresent());
    assertTrue(routeRegistry.getAvailableRoutes().isEmpty());
  }

  @Route(value = "test", target = TargetView.class)
  static class TestView extends Component {

    @Override
    protected void onCreate(Window window) {
      // no-op
    }

    @Override
    protected void onDestroy() {
      // no-op
    }
  }

  @Route
  @RouteAlias("alias1")
  @RouteAlias("alias2")
  static class TargetView extends Component {
    @Override
    protected void onCreate(Window window) {
      // no-op
    }

    @Override
    protected void onDestroy() {
      // no-op
    }
  }
}
