package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.PendingResult;
import com.webforj.component.Component;
import com.webforj.component.html.elements.Div;
import com.webforj.router.NavigationContext;
import com.webforj.router.observer.RouteRendererObserver.LifecycleEvent;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.function.Consumer;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class BBjSessionObserverTest {
  private final BBjAuthenticationService service = mock(BBjAuthenticationService.class);
  private final BBjSessionObserver observer = new BBjSessionObserver(service);
  private final NavigationContext first = mock(NavigationContext.class);
  private final NavigationContext second = mock(NavigationContext.class);
  private final Component component = mock(Component.class);
  private final List<LogRecord> records = new CopyOnWriteArrayList<>();
  private final Logger logger = Logger.getLogger(BBjSessionObserver.class.getName());
  private final Handler handler = new Handler() {
    @Override
    public void publish(LogRecord logRecord) {
      records.add(logRecord);
    }

    @Override
    public void flush() {
      // Not implemented
    }

    @Override
    public void close() {
      // Not implemented
    }
  };
  private Level previousLevel;

  @BeforeEach
  void init() {
    previousLevel = logger.getLevel();
    logger.setLevel(Level.ALL);
    logger.addHandler(handler);
  }

  @AfterEach
  void reset() {
    logger.removeHandler(handler);
    logger.setLevel(previousLevel);
  }

  @SuppressWarnings("unchecked")
  private static Consumer<Boolean> createCallback() {
    return mock(Consumer.class);
  }

  private List<String> getWarnings() {
    SimpleFormatter formatter = new SimpleFormatter();

    return records.stream().filter(it -> it.getLevel() == Level.WARNING)
        .map(formatter::formatMessage).toList();
  }

  @Test
  void shouldRejectMissingService() {
    assertThrows(NullPointerException.class, () -> new BBjSessionObserver(null));
  }

  @Test
  void shouldContinueInTheSameCallWhenTheCheckIsCompleted() {
    when(service.startCheck()).thenReturn(PendingResult.completedWith(null));
    Consumer<Boolean> callback = createCallback();

    observer.onInstantiate(Div.class, first, callback);

    verify(callback, times(1)).accept(true);
  }

  @Test
  void shouldHoldTheCallbackUntilTheCheckCompletes() {
    PendingResult<Void> check = new PendingResult<>();
    when(service.startCheck()).thenReturn(check);
    Consumer<Boolean> callback = createCallback();

    observer.onInstantiate(Div.class, first, callback);
    verifyNoInteractions(callback);
    check.complete(null);

    verify(callback, times(1)).accept(true);
  }

  @Test
  void shouldShareOneCheckBetweenTheLayoutAndTheViewOfOneNavigation() {
    PendingResult<Void> check = new PendingResult<>();
    when(service.startCheck()).thenReturn(check);
    Consumer<Boolean> layoutCallback = createCallback();
    Consumer<Boolean> viewCallback = createCallback();

    observer.onInstantiate(Div.class, first, layoutCallback);
    check.complete(null);
    observer.onInstantiate(Div.class, first, viewCallback);

    verify(layoutCallback, times(1)).accept(true);
    verify(viewCallback, times(1)).accept(true);
    verify(service, times(1)).startCheck();
  }

  @Test
  void shouldStopTheHeldNavigationWhenAnotherOneArrives() {
    PendingResult<Void> olderCheck = new PendingResult<>();
    PendingResult<Void> newerCheck = new PendingResult<>();
    when(service.startCheck()).thenReturn(olderCheck, newerCheck);
    Consumer<Boolean> olderCallback = createCallback();
    Consumer<Boolean> newerCallback = createCallback();

    observer.onInstantiate(Div.class, first, olderCallback);
    observer.onInstantiate(Div.class, second, newerCallback);

    verify(olderCallback, times(1)).accept(false);
    verifyNoInteractions(newerCallback);
    olderCheck.complete(null);

    verify(olderCallback, never()).accept(true);
    verifyNoInteractions(newerCallback);
    newerCheck.complete(null);

    verify(newerCallback, times(1)).accept(true);
    verify(newerCallback, never()).accept(false);
  }

  @Test
  void shouldContinueTheNewerNavigationWhenItsCheckIsCompleted() {
    PendingResult<Void> olderCheck = new PendingResult<>();
    when(service.startCheck()).thenReturn(olderCheck, PendingResult.completedWith(null));
    Consumer<Boolean> olderCallback = createCallback();
    Consumer<Boolean> newerCallback = createCallback();

    observer.onInstantiate(Div.class, first, olderCallback);
    observer.onInstantiate(Div.class, second, newerCallback);

    verify(olderCallback, times(1)).accept(false);
    verify(newerCallback, times(1)).accept(true);
  }

  @Test
  void shouldStopTheHeldNavigationOnLifecycleEventOfAnotherNavigation() {
    PendingResult<Void> check = new PendingResult<>();
    when(service.startCheck()).thenReturn(check);
    Consumer<Boolean> heldCallback = createCallback();
    Consumer<Boolean> eventCallback = createCallback();

    observer.onInstantiate(Div.class, first, heldCallback);
    observer.onRouteRendererLifecycleEvent(component, LifecycleEvent.ACTIVATE, second,
        eventCallback);
    check.complete(null);

    verify(heldCallback, times(1)).accept(false);
    verify(heldCallback, never()).accept(true);
    verify(eventCallback, times(1)).accept(true);
  }

  @Test
  void shouldKeepHoldingOnLifecycleEventOfTheHeldNavigation() {
    PendingResult<Void> check = new PendingResult<>();
    when(service.startCheck()).thenReturn(check);
    Consumer<Boolean> heldCallback = createCallback();
    Consumer<Boolean> eventCallback = createCallback();

    observer.onInstantiate(Div.class, first, heldCallback);
    observer.onRouteRendererLifecycleEvent(component, LifecycleEvent.ACTIVATE, first,
        eventCallback);

    verify(eventCallback, times(1)).accept(true);
    verifyNoInteractions(heldCallback);
    check.complete(null);

    verify(heldCallback, times(1)).accept(true);
  }

  @Test
  void shouldContinueLifecycleEventWhenNothingIsHeld() {
    Consumer<Boolean> eventCallback = createCallback();

    observer.onRouteRendererLifecycleEvent(component, LifecycleEvent.ACTIVATE, first,
        eventCallback);

    verify(eventCallback, times(1)).accept(true);
    verifyNoInteractions(service);
  }

  @Test
  void shouldContinueAndWarnWhenTheCheckCompletesExceptionally() {
    PendingResult<Void> check = new PendingResult<>();
    when(service.startCheck()).thenReturn(check);
    Consumer<Boolean> callback = createCallback();

    observer.onInstantiate(Div.class, first, callback);
    check.completeExceptionally(new IllegalStateException("alice"));

    assertEquals(List.of("The sign in check failed with java.lang.IllegalStateException"),
        getWarnings());
    verify(callback, times(1)).accept(true);
    verify(callback, never()).accept(false);
  }

  @Test
  void shouldContinueAndWarnWhenTheCheckCannotBeStarted() {
    when(service.startCheck()).thenThrow(new IllegalStateException("alice"));
    Consumer<Boolean> callback = createCallback();

    observer.onInstantiate(Div.class, first, callback);

    assertEquals(List.of("The sign in check failed with java.lang.IllegalStateException"),
        getWarnings());
    verify(callback, times(1)).accept(true);
  }

  @Test
  void shouldNotCheckAgainForTheNavigationWhoseCheckCouldNotBeStarted() {
    when(service.startCheck()).thenThrow(new IllegalStateException("alice"));
    Consumer<Boolean> callback = createCallback();

    observer.onInstantiate(Div.class, first, createCallback());
    observer.onInstantiate(Div.class, first, callback);

    verify(service, times(1)).startCheck();
    verify(callback, times(1)).accept(true);
  }

  @Test
  void shouldRunWithPriorityZero() {
    assertEquals(0, observer.getPriority());
  }
}
