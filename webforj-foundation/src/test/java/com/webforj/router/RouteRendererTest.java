package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.Environment;
import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.component.window.Window;
import com.webforj.concern.HasComponents;
import com.webforj.router.exception.RouteNotFoundException;
import com.webforj.router.observer.RouteRendererObserver;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class RouteRendererTest {

  private RouteRegistry routeRegistry;
  private RouteRenderer routeRenderer;
  private MockedStatic<Environment> mockedEnvironment;

  @BeforeEach
  void setUp() throws BBjException {
    routeRegistry = new RouteRegistry();
    routeRenderer = spy(new RouteRenderer(routeRegistry));

    mockedEnvironment = mockStatic(Environment.class);
    mockedEnvironment.when(Environment::getCurrent).thenReturn(mock(Environment.class));
    BBjSysGui sysGui = mock(BBjSysGui.class);
    when(Environment.getCurrent().getSysGui()).thenReturn(sysGui);

    // Setup mock windows and their user data
    Frame f1 = mock(Frame.class);
    Frame f2 = mock(Frame.class);

    when(f1.getFrameId()).thenReturn("frame1");
    when(f2.getFrameId()).thenReturn("frame2");

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
  }

  @Test
  void shouldNavigateToRegisteredComponent() {
    routeRegistry.register("test", TestComponent.class);

    AtomicBoolean resultReceived = new AtomicBoolean(false);
    routeRenderer.navigate(TestComponent.class, result -> {
      assertNotNull(result);
      assertEquals(TestComponent.class, result.getClass());
      resultReceived.set(true);
    });

    assertTrue(resultReceived.get());
  }

  @Test
  void shouldFailToNavigateToUnregisteredComponent() {
    assertThrows(RouteNotFoundException.class, () -> {
      routeRenderer.navigate(UnregisteredComponent.class);
    });
  }

  @Test
  void shouldAddChildComponentToParent() {
    routeRegistry.register("/parent", ParentComponent.class);
    routeRegistry.register("/child", ChildComponent.class, ParentComponent.class);

    AtomicBoolean parentResultReceived = new AtomicBoolean(false);
    routeRenderer.navigate(ParentComponent.class, parentResult -> {
      assertNotNull(parentResult);
      assertEquals(ParentComponent.class, parentResult.getClass());
      parentResultReceived.set(true);

      routeRenderer.navigate(ChildComponent.class, childResult -> {
        assertNotNull(childResult);
        assertEquals(ChildComponent.class, childResult.getClass());
      });
    });

    assertTrue(parentResultReceived.get());
  }

  @Nested
  class DestroyComponents {
    @Test
    void shouldInvokeLifecycleObserversBeforeDestroy() {
      routeRegistry.register("/test", TestComponent.class);
      routeRegistry.register("/unregistered", UnregisteredComponent.class);
      routeRenderer.navigate(TestComponent.class, result -> {
      });

      AtomicBoolean observerCalled = new AtomicBoolean(false);
      routeRenderer.addObserver((component, event, cb) -> {
        if (event == RouteRendererObserver.LifecycleEvent.BEFORE_DESTROY) {
          observerCalled.set(true);
        }
        cb.accept(true);
      });

      routeRenderer.navigate(UnregisteredComponent.class, result -> {
        assertTrue(observerCalled.get());
        assertNotNull(result);
      });
    }

    @Test
    void shouldInvokeLifecycleObserversAfterDestroy() {
      routeRegistry.register("/test", TestComponent.class);
      routeRegistry.register("/unregistered", UnregisteredComponent.class);
      routeRenderer.navigate(TestComponent.class, result -> {
      });

      AtomicBoolean observerCalled = new AtomicBoolean(false);
      routeRenderer.addObserver((component, event, cb) -> {
        if (event == RouteRendererObserver.LifecycleEvent.AFTER_DESTROY) {
          observerCalled.set(true);
        }
        cb.accept(true);
      });

      routeRenderer.navigate(UnregisteredComponent.class, result -> {
        assertTrue(observerCalled.get());
        assertNotNull(result);
      });
    }

    @Test
    void shouldNotRemoveComponentIfBeforeDestroyVetoed() {
      routeRegistry.register("/test", TestComponent.class);
      routeRegistry.register("/unregistered", UnregisteredComponent.class);
      routeRenderer.navigate(TestComponent.class, result -> {
      });

      AtomicReference<Component> componentRef = new AtomicReference<>();
      routeRenderer.addObserver((component, event, cb) -> {
        componentRef.set(component);
        if (event == RouteRendererObserver.LifecycleEvent.BEFORE_DESTROY) {
          cb.accept(false);
        } else {
          cb.accept(true);
        }
      });

      routeRenderer.navigate(UnregisteredComponent.class);

      assertFalse(componentRef.get().isDestroyed());
    }
  }

  @Nested
  class CreateComponents {
    @Test
    void shouldInvokeLifecycleObserversBeforeCreate() {
      routeRegistry.register("/test", TestComponent.class);

      AtomicBoolean observerCalled = new AtomicBoolean(false);
      routeRenderer.addObserver((component, event, cb) -> {
        if (event == RouteRendererObserver.LifecycleEvent.BEFORE_CREATE) {
          observerCalled.set(true);
        }
        cb.accept(true);
      });

      routeRenderer.navigate(TestComponent.class, result -> {
        assertTrue(observerCalled.get());
        assertNotNull(result);
      });
    }

    @Test
    void shouldInvokeLifecycleObserversAfterCreate() {
      routeRegistry.register("/test", TestComponent.class);

      AtomicBoolean observerCalled = new AtomicBoolean(false);
      routeRenderer.addObserver((component, event, cb) -> {
        if (event == RouteRendererObserver.LifecycleEvent.AFTER_CREATE) {
          observerCalled.set(true);
        }
        cb.accept(true);
      });

      routeRenderer.navigate(TestComponent.class, result -> {
        assertTrue(observerCalled.get());
        assertNotNull(result);
      });
    }

    // @Test
    // void shouldNotCreateComponentIfBeforeCreateVetoed() {
    // routeRegistry.register("/test", TestComponent.class);

    // AtomicReference<Component> componentRef = new AtomicReference<>();
    // routeRenderer.addLifecycleObserver((component, event, cb) -> {
    // componentRef.set(component);
    // if (event == RouteRendererLifecycleObserver.LifecycleEvent.BEFORE_CREATE) {
    // cb.accept(false);
    // } else {
    // cb.accept(true);
    // }
    // });

    // routeRenderer.navigate(TestComponent.class);
    // assertFalse(componentRef.get().isAttached());
    // }
  }

  // Mock TestComponent class used in tests
  static class TestComponent extends Component implements HasComponents {
    @Override
    protected void onCreate(Window window) {
      // Do nothing
    }

    @Override
    protected void onDestroy() {
      // Do nothing
    }
  }

  // Mock UnregisteredComponent class used in tests
  static class UnregisteredComponent extends Component implements RouteTarget {
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

  // Mock ParentComponent class used in tests
  static class ParentComponent extends Component implements RouteTarget {
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

  // Mock ChildComponent class used in tests
  static class ChildComponent extends Component implements RouteTarget {
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
}
