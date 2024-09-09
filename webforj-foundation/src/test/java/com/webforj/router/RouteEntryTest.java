package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.component.window.Window;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class RouteEntryTest {

  @Nested
  class Constructors {
    @Test
    void shouldCreateRouteEntryWithAllParameters() {
      String path = "test";
      Class<? extends Component> component = TestComponent.class;
      Class<? extends Component> target = TestTargetComponent.class;
      String frameId = "frame1";
      int priority = 5;

      RouteEntry entry = new RouteEntry(path, component, target, frameId, priority);

      assertEquals(path, entry.getPath());
      assertEquals(component, entry.getComponent());
      assertEquals(target, entry.getOutlet());
      assertFalse(entry.getFrameId().isPresent());
      assertEquals(priority, entry.getPriority());
    }

    @Test
    void shouldCreateRouteEntryWithoutFrameId() {
      String path = "test";
      Class<? extends Component> component = TestComponent.class;
      Class<? extends Component> target = TestTargetComponent.class;
      int priority = 5;

      RouteEntry entry = new RouteEntry(path, component, target, priority);

      assertEquals(path, entry.getPath());
      assertEquals(component, entry.getComponent());
      assertEquals(target, entry.getOutlet());
      assertFalse(entry.getFrameId().isPresent());
      assertEquals(priority, entry.getPriority());
    }

    @Test
    void shouldCreateRouteEntryWithDefaultTarget() {
      String path = "test";
      Class<? extends Component> component = TestComponent.class;
      int priority = 5;

      RouteEntry entry = new RouteEntry(path, component, priority);

      assertEquals(path, entry.getPath());
      assertEquals(component, entry.getComponent());
      assertEquals(Frame.class, entry.getOutlet());
      assertFalse(entry.getFrameId().isPresent());
      assertEquals(priority, entry.getPriority());
    }

    @Test
    void shouldCreateRouteEntryWithFrameId() {
      String path = "test";
      Class<? extends Component> component = TestComponent.class;
      int priority = 5;
      String frameId = "frame1";

      RouteEntry entry = new RouteEntry(path, component, priority, frameId);

      assertEquals(path, entry.getPath());
      assertEquals(component, entry.getComponent());
      assertEquals(Frame.class, entry.getOutlet());
      assertFalse(entry.getFrameId().isPresent());
      assertEquals(priority, entry.getPriority());
    }

    @Test
    void shouldCreateRouteEntryWithDefaultPriority() {
      String path = "test";
      Class<? extends Component> component = TestComponent.class;

      RouteEntry entry = new RouteEntry(path, component);

      assertEquals(path, entry.getPath());
      assertEquals(component, entry.getComponent());
      assertEquals(Frame.class, entry.getOutlet());
      assertFalse(entry.getFrameId().isPresent());
      assertEquals(10, entry.getPriority());
    }
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
