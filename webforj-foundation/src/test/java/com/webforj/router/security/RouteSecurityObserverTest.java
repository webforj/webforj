package com.webforj.router.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.router.NavigationContext;
import com.webforj.router.observer.RouteRendererObserver.LifecycleEvent;
import java.util.function.Consumer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class RouteSecurityObserverTest {

  private RouteSecurityManager securityManager;
  private RouteSecurityObserver observer;
  private NavigationContext navigationContext;
  private Consumer<Boolean> continueCallback;

  @BeforeEach
  void setUp() {
    securityManager = mock(RouteSecurityManager.class);
    observer = new RouteSecurityObserver(securityManager);
    navigationContext = mock(NavigationContext.class);
    continueCallback = mock(Consumer.class);
  }

  @Test
  void shouldGetSecurityManager() {
    assertEquals(securityManager, observer.getSecurityManager());
  }

  @Test
  void shouldHavePriorityOfOne() {
    assertEquals(1, observer.getPriority());
  }

  @Test
  void shouldSkipNonBeforeCreateEvents() {
    TestComponent component = new TestComponent();

    observer.onRouteRendererLifecycleEvent(component, LifecycleEvent.AFTER_CREATE,
        navigationContext, continueCallback);

    verify(continueCallback).accept(true);
    verify(securityManager, never()).evaluate(any(), any());
  }

  @Test
  void shouldAllowAccessWhenGranted() {
    TestComponent component = new TestComponent();
    when(securityManager.evaluate(TestComponent.class, navigationContext))
        .thenReturn(RouteAccessDecision.grant());

    observer.onRouteRendererLifecycleEvent(component, LifecycleEvent.BEFORE_CREATE,
        navigationContext, continueCallback);

    verify(continueCallback).accept(true);
    verify(securityManager, never()).onAccessDenied(any(), any());
  }

  @Test
  void shouldDenyAccessWhenDenied() {
    TestComponent component = new TestComponent();
    RouteAccessDecision denial = RouteAccessDecision.denyAuthentication("Test reason");
    when(securityManager.evaluate(TestComponent.class, navigationContext)).thenReturn(denial);

    observer.onRouteRendererLifecycleEvent(component, LifecycleEvent.BEFORE_CREATE,
        navigationContext, continueCallback);

    verify(continueCallback).accept(false);
    verify(securityManager).onAccessDenied(denial, navigationContext);
  }

  @Test
  void shouldDenyAccessOnEvaluationError() {
    TestComponent component = new TestComponent();
    doThrow(new RuntimeException("Test error")).when(securityManager).evaluate(any(), any());

    observer.onRouteRendererLifecycleEvent(component, LifecycleEvent.BEFORE_CREATE,
        navigationContext, continueCallback);

    verify(continueCallback).accept(false);
    verify(securityManager, never()).onAccessDenied(any(), any());
  }

  private static class TestComponent extends Component {
    @Override
    protected void onCreate(com.webforj.component.window.Window window) {
      // No-op
    }

    @Override
    protected void onDestroy() {
      // No-op
    }
  }
}
