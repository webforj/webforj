package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
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

      assertEquals(view, routeRegistry.getComponentByRoute("test"));
      assertEquals(target, routeRegistry.getTarget(TestComponent.class));
      assertEquals("frame1", routeRegistry.getFrameRouteId(view));
    }

    @Test
    void shouldRegisterRouteWithComponentAndTarget() {
      Class<TestComponent> view = TestComponent.class;
      Class<TestTargetComponent> target = TestTargetComponent.class;
      routeRegistry.register("test", view, target);

      assertEquals(view, routeRegistry.getComponentByRoute("test"));
      assertEquals(target, routeRegistry.getTarget(TestComponent.class));
      assertNull(routeRegistry.getFrameRouteId(TestComponent.class));
    }

    @Test
    void shouldRegisterRouteWithComponent() {
      Class<TestComponent> view = TestComponent.class;
      routeRegistry.register("test", view);

      assertEquals(view, routeRegistry.getComponentByRoute("test"));
      assertEquals(Frame.class, routeRegistry.getTarget(TestComponent.class));
    }
  }

  @Nested
  class QueryRoutes {
    @Test
    void shouldGetComponentByRoute() {
      routeRegistry.register("test", TestComponent.class);

      assertEquals(TestComponent.class, routeRegistry.getComponentByRoute("test"));
      assertNull(routeRegistry.getComponentByRoute("/nonexistent"));
    }

    @Test
    void shouldGetRouteByComponent() {
      routeRegistry.register("test", TestComponent.class);

      assertEquals("test", routeRegistry.getRouteByComponent(TestComponent.class));
      assertNull(routeRegistry.getRouteByComponent(TestTargetComponent.class));
    }

    @Test
    void shouldGetResolvedRouteByComponent() {
      routeRegistry.register("parent", TestTargetComponent.class);
      routeRegistry.register("child", TestComponent.class, TestTargetComponent.class);

      assertEquals("parent/child", routeRegistry.getResolvedRouteByComponent(TestComponent.class));
    }

    @Test
    void shouldGetResolvedComponentByRoute() {
      routeRegistry.register("parent", TestTargetComponent.class);
      routeRegistry.register("child", TestComponent.class, TestTargetComponent.class);

      Class<? extends Component> rootNode =
          routeRegistry.getResolvedComponentByRoute("parent/child");

      assertEquals(TestComponent.class, rootNode);
    }

    @Test
    void shouldGetResolvedRoutes() {
      routeRegistry.register("parent", TestTargetComponent.class);
      routeRegistry.register("child", TestComponent.class, TestTargetComponent.class);

      List<String> resolvedRoutes = routeRegistry.getResolvedRoutes();

      assertEquals(2, resolvedRoutes.size());
      assertTrue(resolvedRoutes.contains("parent"));
      assertTrue(resolvedRoutes.contains("parent/child"));
    }

    @Test
    void shouldGetComponentsTree() {
      routeRegistry.register("parent", TestTargetComponent.class);
      routeRegistry.register("child", TestComponent.class, TestTargetComponent.class);

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

    assertNull(routeRegistry.getComponentByRoute("test"));
    assertNull(routeRegistry.getTarget(TestComponent.class));
    assertNull(routeRegistry.getFrameRouteId(TestComponent.class));
    assertTrue(routeRegistry.getResolvedRoutes().isEmpty());
  }

  static class TestComponent extends Component {

    @Override
    protected void onCreate(Window window) {}

    @Override
    protected void onDestroy() {}
  }

  static class TestTargetComponent extends Component {
    @Override
    protected void onCreate(Window window) {}

    @Override
    protected void onDestroy() {}
  }
}
