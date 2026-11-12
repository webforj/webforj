package com.webforj.router.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.router.NavigationContext;
import com.webforj.router.observer.RouteRendererObserver.LifecycleEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
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
  void shouldContinueBeforeCreateWithoutEvaluatingWhenGranted() {
    TestComponent component = new TestComponent();
    when(securityManager.evaluate(TestComponent.class, navigationContext))
        .thenReturn(RouteAccessDecision.grant());

    observer.onRouteRendererLifecycleEvent(component, LifecycleEvent.BEFORE_CREATE,
        navigationContext, continueCallback);

    verify(continueCallback).accept(true);
    verify(securityManager, never()).evaluate(any(), any());
  }

  @Test
  void shouldContinueBeforeCreateWithoutEvaluatingWhenDenied() {
    TestComponent component = new TestComponent();
    RouteAccessDecision denial = RouteAccessDecision.denyAuthentication("Test reason");
    when(securityManager.evaluate(TestComponent.class, navigationContext)).thenReturn(denial);

    observer.onRouteRendererLifecycleEvent(component, LifecycleEvent.BEFORE_CREATE,
        navigationContext, continueCallback);

    verify(continueCallback).accept(true);
    verify(securityManager, never()).evaluate(any(), any());
  }

  @Test
  void shouldContinueBeforeCreateWithoutEvaluatingOnEvaluationError() {
    TestComponent component = new TestComponent();
    doThrow(new RuntimeException("Test error")).when(securityManager).evaluate(any(), any());

    observer.onRouteRendererLifecycleEvent(component, LifecycleEvent.BEFORE_CREATE,
        navigationContext, continueCallback);

    verify(continueCallback).accept(true);
    verify(securityManager, never()).evaluate(any(), any());
  }

  @Test
  void shouldAllowInstantiationWhenGranted() {
    when(securityManager.evaluate(TestComponent.class, navigationContext))
        .thenReturn(RouteAccessDecision.grant());

    observer.onInstantiate(TestComponent.class, navigationContext, continueCallback);

    verify(continueCallback).accept(true);
    verify(securityManager).evaluate(TestComponent.class, navigationContext);
    verify(securityManager, never()).onAccessDenied(any(), any());
  }

  @Test
  void shouldDenyInstantiationWhenDenied() {
    RouteAccessDecision denial = RouteAccessDecision.denyAuthentication("Test reason");
    when(securityManager.evaluate(TestComponent.class, navigationContext)).thenReturn(denial);

    observer.onInstantiate(TestComponent.class, navigationContext, continueCallback);

    verify(continueCallback).accept(false);
    verify(securityManager).onAccessDenied(denial, navigationContext);
  }

  @Test
  void shouldDenyInstantiationOnEvaluationError() {
    doThrow(new RuntimeException("Test error")).when(securityManager).evaluate(any(), any());

    observer.onInstantiate(TestComponent.class, navigationContext, continueCallback);

    verify(continueCallback).accept(false);
    verify(securityManager, never()).onAccessDenied(any(), any());
  }

  @Test
  void shouldLogCauseOnEvaluationError() {
    RuntimeException error = new RuntimeException("Test error");
    doThrow(error).when(securityManager).evaluate(any(), any());
    List<LogRecord> records = new ArrayList<>();
    Handler handler = new Handler() {
      @Override
      public void publish(LogRecord logRecord) {
        records.add(logRecord);
      }

      @Override
      public void flush() {}

      @Override
      public void close() {}
    };
    Logger logger = Logger.getLogger(RouteSecurityObserver.class.getName());
    logger.addHandler(handler);

    try {
      observer.onInstantiate(TestComponent.class, navigationContext, continueCallback);
    } finally {
      logger.removeHandler(handler);
    }

    assertEquals(1, records.size());
    assertSame(error, records.get(0).getThrown());
    assertEquals("Error during security evaluation for route: " + TestComponent.class.getName(),
        records.get(0).getMessage());
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
