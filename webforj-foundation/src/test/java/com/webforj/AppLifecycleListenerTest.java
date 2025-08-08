package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.annotation.AppListenerPriority;
import com.webforj.exceptions.WebforjException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.ServiceLoader;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AppLifecycleListenerTest {

  @BeforeAll
  static void setupLocale() {
    // Use COMPAT locale provider to avoid CLDR initialization issues in tests
    System.setProperty("java.locale.providers", "COMPAT");
  }

  @Test
  void shouldInvokeListenersInPriorityOrder() {
    TestApp app = new TestApp();

    // Create listeners with different priorities
    HighPriorityListener highPriority = new HighPriorityListener();
    DefaultPriorityListener defaultPriority = new DefaultPriorityListener();
    LowPriorityListener lowPriority = new LowPriorityListener();

    // Add in random order
    List<AppLifecycleListener> listeners = new ArrayList<>();
    listeners.add(lowPriority);
    listeners.add(highPriority);
    listeners.add(defaultPriority);

    // Mock ServiceLoader to return our test listeners
    try (@SuppressWarnings("rawtypes")
    MockedStatic<ServiceLoader> serviceLoaderMock = mockStatic(ServiceLoader.class)) {
      @SuppressWarnings("unchecked")
      ServiceLoader<AppLifecycleListener> mockLoader = mock(ServiceLoader.class);
      when(mockLoader.iterator()).thenReturn(listeners.iterator());
      serviceLoaderMock.when(() -> ServiceLoader.load(AppLifecycleListener.class))
          .thenReturn(mockLoader);

      // Register listeners
      AppLifecycleListenerRegistry.registerListeners(app);

      // Get registered listeners
      Collection<AppLifecycleListener> registeredListeners =
          AppLifecycleListenerRegistry.getListeners(app);

      // Verify count
      assertEquals(3, registeredListeners.size());

      // Verify order (should be sorted by priority: 5, 10, 20)
      List<AppLifecycleListener> sortedList = new ArrayList<>(registeredListeners);
      assertTrue(sortedList.get(0) instanceof HighPriorityListener);
      assertTrue(sortedList.get(1) instanceof DefaultPriorityListener);
      assertTrue(sortedList.get(2) instanceof LowPriorityListener);
    }
  }

  @Test
  void shouldHandleListenersWithoutPriorityAnnotation() {
    TestApp app = new TestApp();
    ListenerWithoutPriority listener = new ListenerWithoutPriority();

    List<AppLifecycleListener> listeners = List.of(listener);

    try (@SuppressWarnings("rawtypes")
    MockedStatic<ServiceLoader> serviceLoaderMock = mockStatic(ServiceLoader.class)) {
      @SuppressWarnings("unchecked")
      ServiceLoader<AppLifecycleListener> mockLoader = mock(ServiceLoader.class);
      when(mockLoader.iterator()).thenReturn(listeners.iterator());
      serviceLoaderMock.when(() -> ServiceLoader.load(AppLifecycleListener.class))
          .thenReturn(mockLoader);

      AppLifecycleListenerRegistry.registerListeners(app);

      Collection<AppLifecycleListener> registeredListeners =
          AppLifecycleListenerRegistry.getListeners(app);

      assertEquals(1, registeredListeners.size());
      // Default priority should be applied
      assertTrue(registeredListeners.contains(listener));
    }
  }

  @Test
  void shouldReturnEmptyCollectionWhenNoListenersRegistered() {
    TestApp app = new TestApp();
    Collection<AppLifecycleListener> listeners = AppLifecycleListenerRegistry.getListeners(app);

    assertNotNull(listeners);
    assertTrue(listeners.isEmpty());
  }

  @Test
  void shouldUnregisterListeners() {
    TestApp app = new TestApp();
    TestListener listener = new TestListener(10);
    List<AppLifecycleListener> listeners = List.of(listener);

    try (@SuppressWarnings("rawtypes")
    MockedStatic<ServiceLoader> serviceLoaderMock = mockStatic(ServiceLoader.class)) {
      @SuppressWarnings("unchecked")
      ServiceLoader<AppLifecycleListener> mockLoader = mock(ServiceLoader.class);
      when(mockLoader.iterator()).thenReturn(listeners.iterator());
      serviceLoaderMock.when(() -> ServiceLoader.load(AppLifecycleListener.class))
          .thenReturn(mockLoader);

      // Register listeners
      AppLifecycleListenerRegistry.registerListeners(app);

      // Verify listener is registered
      Collection<AppLifecycleListener> registeredListeners =
          AppLifecycleListenerRegistry.getListeners(app);
      assertEquals(1, registeredListeners.size());

      // Unregister listeners
      AppLifecycleListenerRegistry.unregisterListeners(app);

      // Verify no listeners remain
      Collection<AppLifecycleListener> afterUnregister =
          AppLifecycleListenerRegistry.getListeners(app);
      assertTrue(afterUnregister.isEmpty());
    }
  }


  // Test implementation of App
  static class TestApp extends App {
    @Override
    public void run() throws WebforjException {
      // Test implementation
    }
  }

  // Test listener without priority annotation - uses priority from constructor for testing
  static class TestListener implements AppLifecycleListener {
    private final int priority;
    private final List<String> events = new ArrayList<>();

    TestListener(int expectedPriority) {
      this.priority = expectedPriority;
    }

    @Override
    public void onWillRun(App app) {
      events.add("onWillRun");
    }

    @Override
    public void onDidRun(App app) {
      events.add("onDidRun");
    }

    @Override
    public void onWillTerminate(App app) {
      events.add("onWillTerminate");
    }

    @Override
    public void onDidTerminate(App app) {
      events.add("onDidTerminate");
    }

    int getPriority() {
      return priority;
    }

    List<String> getEvents() {
      return events;
    }
  }

  // Test listener without priority annotation
  static class ListenerWithoutPriority implements AppLifecycleListener {
    // Uses default priority of 10
  }

  // High priority listener
  @AppListenerPriority(5)
  static class HighPriorityListener implements AppLifecycleListener {
    // Priority 5
  }

  // Default priority listener
  @AppListenerPriority(10)
  static class DefaultPriorityListener implements AppLifecycleListener {
    // Priority 10
  }

  // Low priority listener
  @AppListenerPriority(20)
  static class LowPriorityListener implements AppLifecycleListener {
    // Priority 20
  }
}
