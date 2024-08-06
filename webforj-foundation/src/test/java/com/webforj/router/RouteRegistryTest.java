package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.component.window.Window;
import com.webforj.data.tree.Vnode;
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
      Class<TestComponent> view = TestComponent.class;
      Class<Frame> target = Frame.class;
      routeRegistry.register("test", view, target, "frame1");

      assertEquals(view, routeRegistry.getComponentByRoute("test").get());
      assertEquals(target, routeRegistry.getTarget(TestComponent.class).get());
      assertEquals("frame1", routeRegistry.getFrameRouteId(view).get());
    }

    @Test
    void shouldRegisterRouteWithComponentAndTarget() {
      Class<TestComponent> view = TestComponent.class;
      Class<TestTargetComponent> target = TestTargetComponent.class;
      routeRegistry.register("test", view, target);

      assertEquals(view, routeRegistry.getComponentByRoute("test").get());
      assertEquals(target, routeRegistry.getTarget(TestComponent.class).get());
      assertFalse(routeRegistry.getFrameRouteId(TestComponent.class).isPresent());
    }

    @Test
    void shouldRegisterRouteWithComponent() {
      Class<TestComponent> view = TestComponent.class;
      routeRegistry.register("test", view);

      assertEquals(view, routeRegistry.getComponentByRoute("test").get());
      assertEquals(Frame.class, routeRegistry.getTarget(TestComponent.class).get());
    }
  }

  @Nested
  class QueryRoutes {
    @Test
    void shouldGetComponentByRoute() {
      routeRegistry.register("test", TestComponent.class);

      assertEquals(TestComponent.class, routeRegistry.getComponentByRoute("test").get());
      assertFalse(routeRegistry.getComponentByRoute("/nonexistent").isPresent());
    }

    @Test
    void shouldGetRouteByComponent() {
      routeRegistry.register("test", TestComponent.class);

      assertEquals("test", routeRegistry.getRouteByComponent(TestComponent.class).get());
      assertFalse(routeRegistry.getRouteByComponent(TestTargetComponent.class).isPresent());
    }

    @Test
    void shouldGetResolvedComponentByRoute() {
      routeRegistry.register("parent", TestTargetComponent.class);
      routeRegistry.register("parent/child", TestComponent.class, TestTargetComponent.class);

      Class<? extends Component> rootNode = routeRegistry.getComponentByRoute("parent/child").get();

      assertEquals(TestComponent.class, rootNode);
    }

    @Test
    void shouldGetResolvedRoutes() {
      routeRegistry.register("parent", TestTargetComponent.class);
      routeRegistry.register("parent/child", TestComponent.class, TestTargetComponent.class);

      List<RouteEntry> resolvedRoutes = routeRegistry.getAvailableRoutes();
      List<String> routePaths = resolvedRoutes.stream().map(RouteEntry::getPath).toList();

      assertEquals(2, resolvedRoutes.size());
      assertTrue(routePaths.contains("parent"));
      assertTrue(routePaths.contains("parent/child"));
    }

    @Test
    void shouldGetComponentsTree() {
      routeRegistry.register("parent", TestTargetComponent.class);
      routeRegistry.register("parent/child", TestComponent.class, TestTargetComponent.class);

      Optional<Vnode<Class<? extends Component>>> tree =
          routeRegistry.getComponentsTree(TestComponent.class);

      assertTrue(tree.isPresent());
      Vnode<Class<? extends Component>> rootNode = tree.get();

      assertEquals(Frame.class, rootNode.getData());
      assertEquals(1, rootNode.getChildren().size());
      assertEquals(TestTargetComponent.class, rootNode.getChildren().get(0).getData());
      assertEquals(TestComponent.class,
          rootNode.getChildren().get(0).getChildren().get(0).getData());
    }
  }

  @Test
  void shouldClearRegistry() {
    routeRegistry.register("test", TestComponent.class);

    routeRegistry.clear();

    assertFalse(routeRegistry.getComponentByRoute("test").isPresent());
    assertFalse(routeRegistry.getTarget(TestComponent.class).isPresent());
    assertFalse(routeRegistry.getFrameRouteId(TestComponent.class).isPresent());
    assertTrue(routeRegistry.getAvailableRoutes().isEmpty());
  }

  static class TestComponent extends Component {

    @Override
    protected void onCreate(Window window) {
      // no-op
    }

    @Override
    protected void onDestroy() {
      // no-op
    }
  }

  static class TestTargetComponent extends Component {
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
