package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.exceptions.WebforjException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.ServiceLoader;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;
import org.mockito.MockedStatic;

class AppLifecycleListenerTest {

  @BeforeAll
  static void setupLocaleProviders() {
    System.setProperty("java.locale.providers", "COMPAT,SPI");
  }

  @Nested
  class PriorityAndRegistration {

    @Test
    void shouldInvokeListenersInPriorityOrder() {
      TestApp app = new TestApp();

      HighPriorityListener highPriority = new HighPriorityListener();
      DefaultPriorityListener defaultPriority = new DefaultPriorityListener();
      LowPriorityListener lowPriority = new LowPriorityListener();

      List<AppLifecycleListener> listeners = new ArrayList<>();
      listeners.add(lowPriority);
      listeners.add(highPriority);
      listeners.add(defaultPriority);
      try (@SuppressWarnings("rawtypes")
      MockedStatic<ServiceLoader> serviceLoaderMock = mockStatic(ServiceLoader.class)) {
        @SuppressWarnings("unchecked")
        ServiceLoader<AppLifecycleListener> mockLoader = mock(ServiceLoader.class);
        when(mockLoader.iterator()).thenReturn(listeners.iterator());
        serviceLoaderMock.when(() -> ServiceLoader.load(AppLifecycleListener.class))
            .thenReturn(mockLoader);

        Collection<AppLifecycleListener> discoveredListeners =
            AppLifecycleListenerRegistry.discoverListeners();
        AppLifecycleListenerRegistry.registerListeners(app, discoveredListeners);

        Collection<AppLifecycleListener> registeredListeners =
            AppLifecycleListenerRegistry.getListeners(app);

        assertEquals(3, registeredListeners.size());
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

        Collection<AppLifecycleListener> discoveredListeners =
            AppLifecycleListenerRegistry.discoverListeners();
        AppLifecycleListenerRegistry.registerListeners(app, discoveredListeners);

        Collection<AppLifecycleListener> registeredListeners =
            AppLifecycleListenerRegistry.getListeners(app);

        assertEquals(1, registeredListeners.size());
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

        Collection<AppLifecycleListener> discoveredListeners =
            AppLifecycleListenerRegistry.discoverListeners();
        AppLifecycleListenerRegistry.registerListeners(app, discoveredListeners);

        Collection<AppLifecycleListener> registeredListeners =
            AppLifecycleListenerRegistry.getListeners(app);
        assertEquals(1, registeredListeners.size());

        AppLifecycleListenerRegistry.unregisterListeners(app);

        Collection<AppLifecycleListener> afterUnregister =
            AppLifecycleListenerRegistry.getListeners(app);
        assertTrue(afterUnregister.isEmpty());
      }
    }
  }

  @Nested
  class CreationHooks {

    @Test
    void shouldAllowConfigModificationInOnWillCreate() {
      AppLifecycleListener configModifyingListener = new AppLifecycleListener() {
        @Override
        public void onWillCreate(Environment env) {
          Config additionalConfig = ConfigFactory.parseString("webforj.test.value = \"modified\"");
          env.setConfig(additionalConfig);
        }
      };

      List<AppLifecycleListener> listeners = List.of(configModifyingListener);

      Environment mockEnv = mock(Environment.class);
      Config mockConfig = ConfigFactory.empty();
      when(mockEnv.getConfig()).thenReturn(mockConfig);

      AppLifecycleListenerRegistry.notifyListeners(listeners,
          listener -> listener.onWillCreate(mockEnv), "onWillCreate");

      verify(mockEnv).setConfig(any(Config.class));
    }
  }

  @Nested
  class LifecycleOrdering {

    @Test
    void shouldInvokeLifecycleHooksInCorrectOrder() {
      TestApp app = new TestApp();

      AppLifecycleListener mockListener = mock(AppLifecycleListener.class);
      List<AppLifecycleListener> listeners = List.of(mockListener);

      Environment mockEnv = mock(Environment.class);
      Config mockConfig = ConfigFactory.empty();
      when(mockEnv.getConfig()).thenReturn(mockConfig);
      AppLifecycleListenerRegistry.notifyListeners(listeners,
          listener -> listener.onWillCreate(mockEnv), "onWillCreate");

      AppLifecycleListenerRegistry.notifyListeners(listeners, listener -> listener.onDidCreate(app),
          "onDidCreate");

      AppLifecycleListenerRegistry.notifyListeners(listeners, listener -> listener.onWillRun(app),
          "onWillRun");

      AppLifecycleListenerRegistry.notifyListeners(listeners, listener -> listener.onDidRun(app),
          "onDidRun");

      InOrder inOrder = inOrder(mockListener);
      inOrder.verify(mockListener).onWillCreate(mockEnv);
      inOrder.verify(mockListener).onDidCreate(app);
      inOrder.verify(mockListener).onWillRun(app);
      inOrder.verify(mockListener).onDidRun(app);
    }
  }

  static class TestApp extends App {
    @Override
    public void run() throws WebforjException {}
  }

  static class TestListener implements AppLifecycleListener {
    private final int priority;
    private final List<String> events = new ArrayList<>();

    TestListener(int expectedPriority) {
      this.priority = expectedPriority;
    }

    @Override
    public void onWillCreate(Environment env) {
      events.add("onWillCreate");
    }

    @Override
    public void onDidCreate(App app) {
      events.add("onDidCreate");
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

  static class ListenerWithoutPriority implements AppLifecycleListener {
  }

  @AppListenerPriority(5)
  static class HighPriorityListener implements AppLifecycleListener {
  }

  @AppListenerPriority(10)
  static class DefaultPriorityListener implements AppLifecycleListener {
  }

  @AppListenerPriority(20)
  static class LowPriorityListener implements AppLifecycleListener {
  }
}
