package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.webforj.annotation.BootstrapListenerPriority;
import com.webforj.bridge.WebforjBBjBridge;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.ServiceLoader;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class BootstrapListenerTest {

  private BootstrapContext mockContext;
  private static final String TEST_ID = "test-bootstrap-id";

  @BeforeAll
  static void setupLocale() {
    // Use COMPAT locale provider to avoid CLDR initialization issues in tests
    System.setProperty("java.locale.providers", "COMPAT");
  }

  @BeforeEach
  void setUp() {
    mockContext =
        new BootstrapContext(mock(BBjAPI.class), mock(WebforjBBjBridge.class), 0, null, TEST_ID);
  }

  @Test
  void shouldRegisterListenersInPriorityOrder() {
    HighPriorityListener highPriority = new HighPriorityListener();
    DefaultPriorityListener defaultPriority = new DefaultPriorityListener();
    LowPriorityListener lowPriority = new LowPriorityListener();

    List<BootstrapListener> listeners = new ArrayList<>();
    listeners.add(lowPriority);
    listeners.add(highPriority);
    listeners.add(defaultPriority);

    try (@SuppressWarnings("rawtypes")
    MockedStatic<ServiceLoader> serviceLoaderMock = mockStatic(ServiceLoader.class)) {
      @SuppressWarnings("unchecked")
      ServiceLoader<BootstrapListener> mockLoader = mock(ServiceLoader.class);
      when(mockLoader.iterator()).thenReturn(listeners.iterator());
      serviceLoaderMock.when(() -> ServiceLoader.load(BootstrapListener.class))
          .thenReturn(mockLoader);

      BootstrapListenerRegistry.registerListeners(mockContext);

      Collection<BootstrapListener> registeredListeners =
          BootstrapListenerRegistry.getListeners(mockContext);

      assertEquals(3, registeredListeners.size());

      List<BootstrapListener> sortedList = new ArrayList<>(registeredListeners);
      assertTrue(sortedList.get(0) instanceof HighPriorityListener);
      assertTrue(sortedList.get(1) instanceof DefaultPriorityListener);
      assertTrue(sortedList.get(2) instanceof LowPriorityListener);
    }
  }

  @Test
  void shouldHandleListenersWithoutPriorityAnnotation() {
    ListenerWithoutPriority listener = new ListenerWithoutPriority();

    List<BootstrapListener> listeners = List.of(listener);

    try (@SuppressWarnings("rawtypes")
    MockedStatic<ServiceLoader> serviceLoaderMock = mockStatic(ServiceLoader.class)) {
      @SuppressWarnings("unchecked")
      ServiceLoader<BootstrapListener> mockLoader = mock(ServiceLoader.class);
      when(mockLoader.iterator()).thenReturn(listeners.iterator());
      serviceLoaderMock.when(() -> ServiceLoader.load(BootstrapListener.class))
          .thenReturn(mockLoader);

      BootstrapListenerRegistry.registerListeners(mockContext);

      Collection<BootstrapListener> registeredListeners =
          BootstrapListenerRegistry.getListeners(mockContext);

      assertEquals(1, registeredListeners.size());
      assertTrue(registeredListeners.contains(listener));
    }
  }

  @Test
  void shouldReturnEmptyCollectionWhenNoListenersRegistered() {
    // Create a fresh context to ensure no listeners are registered
    BootstrapContext freshContext =
        new BootstrapContext(mock(BBjAPI.class), mock(WebforjBBjBridge.class), 0, null, "fresh-id");

    Collection<BootstrapListener> listeners = BootstrapListenerRegistry.getListeners(freshContext);

    assertNotNull(listeners);
    assertTrue(listeners.isEmpty());
  }

  @Test
  void shouldUnregisterListeners() {
    TestListener listener = new TestListener();
    List<BootstrapListener> listeners = List.of(listener);

    try (@SuppressWarnings("rawtypes")
    MockedStatic<ServiceLoader> serviceLoaderMock = mockStatic(ServiceLoader.class)) {
      @SuppressWarnings("unchecked")
      ServiceLoader<BootstrapListener> mockLoader = mock(ServiceLoader.class);
      when(mockLoader.iterator()).thenReturn(listeners.iterator());
      serviceLoaderMock.when(() -> ServiceLoader.load(BootstrapListener.class))
          .thenReturn(mockLoader);

      BootstrapListenerRegistry.registerListeners(mockContext);

      Collection<BootstrapListener> registeredListeners =
          BootstrapListenerRegistry.getListeners(mockContext);
      assertEquals(1, registeredListeners.size());

      BootstrapListenerRegistry.unregisterListeners(mockContext);

      Collection<BootstrapListener> afterUnregister =
          BootstrapListenerRegistry.getListeners(mockContext);
      assertTrue(afterUnregister.isEmpty());
    }
  }

  @Test
  void shouldNotifyOnCleanup() {
    CleanupTrackingListener listener = new CleanupTrackingListener();

    try (@SuppressWarnings("rawtypes")
    MockedStatic<ServiceLoader> serviceLoaderMock = mockStatic(ServiceLoader.class)) {
      @SuppressWarnings("unchecked")
      ServiceLoader<BootstrapListener> mockLoader = mock(ServiceLoader.class);
      List<BootstrapListener> listenersList = List.of((BootstrapListener) listener);
      when(mockLoader.iterator()).thenReturn(listenersList.iterator());
      serviceLoaderMock.when(() -> ServiceLoader.load(BootstrapListener.class))
          .thenReturn(mockLoader);

      BootstrapListenerRegistry.registerListeners(mockContext);

      BootstrapListenerRegistry.notifyCleanup(mockContext);

      assertTrue(listener.wasCleanupCalled());
      assertSame(mockContext, listener.getCleanupContext());
    }
  }

  static class ListenerWithoutPriority implements BootstrapListener {
  }

  static class TestListener implements BootstrapListener {
  }

  @BootstrapListenerPriority(5)
  static class HighPriorityListener implements BootstrapListener {
  }

  @BootstrapListenerPriority(10)
  static class DefaultPriorityListener implements BootstrapListener {
  }

  @BootstrapListenerPriority(20)
  static class LowPriorityListener implements BootstrapListener {
  }

  static class CleanupTrackingListener implements BootstrapListener {
    private boolean cleanupCalled = false;
    private BootstrapContext cleanupContext = null;

    @Override
    public void onCleanup(BootstrapContext context) {
      cleanupCalled = true;
      cleanupContext = context;
    }

    public boolean wasCleanupCalled() {
      return cleanupCalled;
    }

    public BootstrapContext getCleanupContext() {
      return cleanupContext;
    }
  }
}
