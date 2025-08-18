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
import com.webforj.component.Composite;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.window.Frame;
import com.webforj.component.window.Window;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.conceiver.DefaultConceiver;
import com.webforj.concern.HasComponents;
import com.webforj.router.exception.NotFoundException;
import com.webforj.router.observer.RouteRendererObserver;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class RouteRendererTest {

  private RouteRegistry routeRegistry;
  private RouteRenderer routeRenderer;
  private MockedStatic<Environment> mockedEnvironment;
  private MockedStatic<ConceiverProvider> mockedConceiverProvider;

  @BeforeEach
  void setUp() throws BBjException {

    mockedConceiverProvider = mockStatic(ConceiverProvider.class);
    when(ConceiverProvider.getCurrent()).thenReturn(new DefaultConceiver());

    routeRegistry = new RouteRegistry();
    routeRenderer = spy(new RouteRenderer(routeRegistry));

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
  void teardown() {
    mockedEnvironment.close();
    mockedConceiverProvider.close();
  }

  @Test
  void shouldThrowNotFoundExceptionIfComponentIsNull() {
    assertThrows(NotFoundException.class, () -> routeRenderer.render(null));
  }

  @Test
  void shouldThrowNotFoundExceptionIfComponentHasNoOutlet() {
    assertThrows(NotFoundException.class, () -> routeRenderer.render(Component.class));
  }

  @Test
  void shouldNavigateToRegisteredComponent() {
    routeRegistry.register("test", TestComponent.class);

    AtomicBoolean resultReceived = new AtomicBoolean(false);
    routeRenderer.render(TestComponent.class, result -> {
      assertNotNull(result);
      assertEquals(TestComponent.class, result.get().getClass());
      resultReceived.set(true);
    });

    assertTrue(resultReceived.get());
  }

  @Test
  void shouldAddChildComponentToParent() {
    routeRegistry.register("/home", HomeView.class);
    routeRegistry.register("/helloworld", HelloWorldView.class, HomeView.class);

    AtomicBoolean parentResultReceived = new AtomicBoolean(false);
    routeRenderer.render(HomeView.class, parentResult -> {
      assertNotNull(parentResult);
      assertEquals(HomeView.class, parentResult.get().getClass());
      parentResultReceived.set(true);

      routeRenderer.render(HelloWorldView.class, childResult -> {
        assertNotNull(childResult);
        assertEquals(HelloWorldView.class, childResult.get().getClass());
      });
    });

    assertTrue(parentResultReceived.get());
  }

  @Test
  void shouldCallOnCompleteIfRemovingIsVetoed() {
    routeRegistry.register("/home", HomeView.class);
    routeRegistry.register("/helloworld", HelloWorldView.class, HomeView.class);
    routeRegistry.register("/about", AboutView.class, HomeView.class);

    routeRenderer.addObserver(new RouteRendererObserver() {
      @Override
      public void onRouteRendererLifecycleEvent(Component component, LifecycleEvent event,
          NavigationContext context, Consumer<Boolean> continueCallback) {
        if (component instanceof HelloWorldView && event == LifecycleEvent.BEFORE_DESTROY) {
          continueCallback.accept(false);
        } else {
          continueCallback.accept(true);
        }
      }
    });

    AtomicBoolean resultReceived = new AtomicBoolean(false);
    routeRenderer.render(HelloWorldView.class, view -> {
      assertNotNull(view);
      assertEquals(HelloWorldView.class, view.get().getClass());

      routeRenderer.render(AboutView.class, aboutViewResult -> {
        assertTrue(aboutViewResult.isEmpty());
        assertFalse(view.get().isDestroyed());
        resultReceived.set(true);
      });
    });

    assertTrue(resultReceived.get());
  }

  @Test
  void shouldUpdateContextWithComponents() {
    routeRegistry.register("/home", HomeView.class);
    routeRegistry.register("/helloworld", HelloWorldView.class, HomeView.class);

    NavigationContext context = new NavigationContext();
    routeRenderer.render(HelloWorldView.class, context);

    Set<Component> components = context.getAllComponents();
    assertNotNull(components);
    assertEquals(2, components.size());

    Component[] componentArray = components.toArray(new Component[0]);
    assertEquals(HomeView.class, componentArray[0].getClass());
    assertEquals(HelloWorldView.class, componentArray[1].getClass());
  }

  @Nested
  class DestroyComponents {

    @Test
    void shouldInvokeLifecycleObserversBeforeDestroy() {
      routeRegistry.register("/test", TestComponent.class);
      routeRegistry.register("/contact", ContactView.class);
      routeRenderer.render(TestComponent.class);

      AtomicBoolean observerCalled = new AtomicBoolean(false);
      routeRenderer.addObserver((component, event, context, cb) -> {
        if (event == RouteRendererObserver.LifecycleEvent.BEFORE_DESTROY) {
          observerCalled.set(true);
        }

        cb.accept(true);
      });

      routeRenderer.render(ContactView.class, result -> {
        assertTrue(observerCalled.get());
        assertFalse(result.isEmpty());
      });
    }

    @Test
    void shouldInvokeLifecycleObserversAfterDestroy() {
      routeRegistry.register("/test", TestComponent.class);
      routeRegistry.register("/contact", ContactView.class);
      routeRenderer.render(TestComponent.class, result -> {
      });

      AtomicBoolean observerCalled = new AtomicBoolean(false);
      routeRenderer.addObserver((component, event, context, cb) -> {
        if (event == RouteRendererObserver.LifecycleEvent.AFTER_DESTROY) {
          observerCalled.set(true);
        }

        cb.accept(true);
      });

      routeRenderer.render(ContactView.class, result -> {
        assertTrue(observerCalled.get());
        assertFalse(result.isEmpty());
      });
    }

    @Test
    void shouldNotRemoveComponentIfBeforeDestroyVetoed() {
      routeRegistry.register("/test", TestComponent.class);
      routeRegistry.register("/contact", ContactView.class);
      routeRenderer.render(TestComponent.class);

      AtomicReference<Component> componentRef = new AtomicReference<>();
      routeRenderer.addObserver((component, event, context, cb) -> {
        componentRef.set(component);
        if (event == RouteRendererObserver.LifecycleEvent.BEFORE_DESTROY) {
          cb.accept(false);
        } else {
          cb.accept(true);
        }
      });

      routeRenderer.render(ContactView.class);

      assertFalse(componentRef.get().isDestroyed());
    }

    @Test
    void shouldRemoveFromHasComponentsParent() {
      routeRegistry.register("/parent", HasComponentsParentView.class);
      routeRegistry.register("/test", TestComponent.class, HasComponentsParentView.class);
      routeRegistry.register("/contact", ContactView.class, HasComponentsParentView.class);

      NavigationContext context = new NavigationContext();
      routeRenderer.render(TestComponent.class, context, result -> {
        assertTrue(result.isPresent());
        Set<Component> components = context.getAllComponents();
        HasComponentsParentView parent = (HasComponentsParentView) components.stream()
            .filter(HasComponentsParentView.class::isInstance).findFirst().get();

        routeRenderer.render(ContactView.class, compositeResult -> {
          assertFalse(parent.hasComponent(result.get()));
          assertTrue(parent.hasComponent(compositeResult.get()));
        });
      });
    }

    @Test
    void shouldRemoveFromCompositeParent() {
      routeRegistry.register("/parent", CompositeParentView.class);
      routeRegistry.register("/test", TestComponent.class, CompositeParentView.class);
      routeRegistry.register("/contact", ContactView.class, CompositeParentView.class);

      NavigationContext context = new NavigationContext();
      routeRenderer.render(TestComponent.class, context, result -> {
        assertTrue(result.isPresent());
        Set<Component> components = context.getAllComponents();
        CompositeParentView parent = (CompositeParentView) components.stream()
            .filter(CompositeParentView.class::isInstance).findFirst().get();

        routeRenderer.render(ContactView.class, compositeResult -> {
          assertFalse(parent.hasComponent(result.get()));
          assertTrue(parent.hasComponent(compositeResult.get()));
        });
      });
    }
  }

  @Nested
  class CreateComponents {
    @Test
    void shouldInvokeLifecycleObserversBeforeCreate() {
      routeRegistry.register("/test", TestComponent.class);

      AtomicBoolean observerCalled = new AtomicBoolean(false);
      routeRenderer.addObserver((component, event, context, cb) -> {
        if (event == RouteRendererObserver.LifecycleEvent.BEFORE_CREATE) {
          observerCalled.set(true);
        }
        cb.accept(true);
      });

      routeRenderer.render(TestComponent.class, result -> {
        assertTrue(observerCalled.get());
        assertTrue(result.isPresent());
      });
    }

    @Test
    void shouldInvokeLifecycleObserversAfterCreate() {
      routeRegistry.register("/test", TestComponent.class);

      AtomicBoolean observerCalled = new AtomicBoolean(false);
      routeRenderer.addObserver((component, event, context, cb) -> {
        if (event == RouteRendererObserver.LifecycleEvent.AFTER_CREATE) {
          observerCalled.set(true);
        }
        cb.accept(true);
      });

      routeRenderer.render(TestComponent.class, result -> {
        assertTrue(observerCalled.get());
        assertTrue(result.isPresent());
      });
    }

    @Test
    void shouldNotCreateComponentIfVetoedBeforeCreate() {
      routeRegistry.register("/test", TestComponent.class);

      routeRenderer.addObserver((component, event, context, cb) -> {
        if (event == RouteRendererObserver.LifecycleEvent.BEFORE_CREATE) {
          cb.accept(false);
        } else {
          cb.accept(true);
        }
      });

      routeRenderer.render(TestComponent.class, result -> {
        assertFalse(result.isPresent());
      });
    }

    @Test
    void shouldSkipAttachedComponents() {
      routeRegistry.register("/test", TestComponent.class);
      routeRegistry.register("/attached", AttachedView.class);

      routeRenderer.render(TestComponent.class, result -> {
        routeRenderer.render(AttachedView.class, attachedResult -> {
          assertFalse(attachedResult.isEmpty());
        });
      });
    }

    @Test
    void shouldAddToHasComponentsParent() {
      routeRegistry.register("/parent", HasComponentsParentView.class);
      routeRegistry.register("/test", TestComponent.class, HasComponentsParentView.class);

      routeRenderer.render(HasComponentsParentView.class, result -> {
        routeRenderer.render(TestComponent.class, compositeResult -> {
          assertFalse(compositeResult.isEmpty());
        });
      });
    }

    @Test
    void shouldAddToCompositeParent() {
      routeRegistry.register("/parent", CompositeParentView.class);
      routeRegistry.register("/test", TestComponent.class, CompositeParentView.class);

      routeRenderer.render(CompositeParentView.class, result -> {
        routeRenderer.render(TestComponent.class, compositeResult -> {
          assertFalse(compositeResult.isEmpty());
        });
      });
    }

    @Test
    void shouldInvokeLifecycleObserversActivate() {
      routeRegistry.register("/test", TestComponent.class);
      routeRegistry.register("/contact", ContactView.class);

      routeRenderer.render(TestComponent.class);

      AtomicBoolean observerCalled = new AtomicBoolean(false);
      routeRenderer.addObserver((component, event, context, cb) -> {
        if (event == RouteRendererObserver.LifecycleEvent.ACTIVATE) {
          observerCalled.set(true);
        }
        cb.accept(true);
      });

      // Navigate away then back to trigger ACTIVATE
      routeRenderer.render(ContactView.class);
      routeRenderer.render(TestComponent.class, result -> {
        assertTrue(observerCalled.get());
        assertTrue(result.isPresent());
      });
    }
  }

  @NodeName("test-component")
  public static class TestComponent extends ElementCompositeContainer {
  }

  // Mock UnregisteredComponent class used in tests
  public static class UnregisteredView extends Component implements RouteOutlet {
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

  public static class HomeView extends Component implements RouteOutlet {
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

  public static class HelloWorldView extends Component implements RouteOutlet {

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

  public static class AboutView extends Component implements RouteOutlet {

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

  public static class ContactView extends Component implements RouteOutlet {

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


  public static class AttachedView extends Component implements RouteOutlet {

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

    @Override
    public boolean isAttached() {
      return true;
    }
  }

  @NodeName("view-parent")
  public static class HasComponentsParentView extends ElementCompositeContainer {
    private List<Component> components = new ArrayList<>();

    @Override
    public void add(Component... components) {
      this.components.addAll(List.of(components));
    }

    @Override
    public void remove(Component... components) {
      this.components.removeAll(List.of(components));
    }

    @Override
    public void removeAll() {
      this.components.clear();
    }

    @Override
    public boolean hasComponent(Component component) {
      return this.components.contains(component);
    }

    @Override
    public List<Component> getComponents() {
      return this.components;
    }
  }

  @NodeName("view-composite")
  public static class CompositeParentView extends Composite<HasComponentsParentView>
      implements HasComponents {
    private List<Component> components = new ArrayList<>();

    @Override
    public void add(Component... components) {
      this.components.addAll(List.of(components));
    }

    @Override
    public void remove(Component... components) {
      this.components.removeAll(List.of(components));
    }

    @Override
    public void removeAll() {
      this.components.clear();
    }

    @Override
    public boolean hasComponent(Component component) {
      return this.components.contains(component);
    }

    @Override
    public List<Component> getComponents() {
      return this.components;
    }
  }
}
