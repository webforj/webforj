package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

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
  void shouldCheckAndContinueInTheSameCall() {
    Consumer<Boolean> callback = createCallback();

    observer.onInstantiate(Div.class, first, callback);

    verify(service, times(1)).check();
    verify(callback, times(1)).accept(true);
  }

  @Test
  void shouldShareOneCheckBetweenTheLayoutAndTheViewOfOneNavigation() {
    Consumer<Boolean> layoutCallback = createCallback();
    Consumer<Boolean> viewCallback = createCallback();

    observer.onInstantiate(Div.class, first, layoutCallback);
    observer.onInstantiate(Div.class, first, viewCallback);

    verify(layoutCallback, times(1)).accept(true);
    verify(viewCallback, times(1)).accept(true);
    verify(service, times(1)).check();
  }

  @Test
  void shouldCheckAgainForAnotherNavigation() {
    observer.onInstantiate(Div.class, first, createCallback());
    observer.onInstantiate(Div.class, second, createCallback());

    verify(service, times(2)).check();
  }

  @Test
  void shouldContinueLifecycleEventWithoutChecking() {
    Consumer<Boolean> eventCallback = createCallback();

    observer.onRouteRendererLifecycleEvent(component, LifecycleEvent.ACTIVATE, first,
        eventCallback);

    verify(eventCallback, times(1)).accept(true);
    verifyNoInteractions(service);
  }

  @Test
  void shouldContinueAndWarnWhenTheCheckThrows() {
    doThrow(new IllegalStateException("alice")).when(service).check();
    Consumer<Boolean> callback = createCallback();

    observer.onInstantiate(Div.class, first, callback);

    assertEquals(List.of("The sign in check failed with java.lang.IllegalStateException"),
        getWarnings());
    verify(callback, times(1)).accept(true);
  }

  @Test
  void shouldNotCheckAgainForTheNavigationWhoseCheckThrew() {
    doThrow(new IllegalStateException("alice")).when(service).check();
    Consumer<Boolean> callback = createCallback();

    observer.onInstantiate(Div.class, first, createCallback());
    observer.onInstantiate(Div.class, first, callback);

    verify(service, times(1)).check();
    verify(callback, times(1)).accept(true);
  }

  @Test
  void shouldRunWithPriorityZero() {
    assertEquals(0, observer.getPriority());
  }
}
