package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.environment.ObjectTable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class ComponentSourceRegistryTest {

  @Test
  void shouldReturnNullForUnregisteredComponent() {
    Map<Integer, Throwable> storage = new HashMap<>();

    try (MockedStatic<ObjectTable> mocked = mockStatic(ObjectTable.class)) {
      mocked.when(() -> ObjectTable.contains(any())).thenReturn(true);
      mocked.when(() -> ObjectTable.get(any())).thenReturn(storage);

      Object component = new Object();
      SourcePoint result = ComponentSourceRegistry.getSourcePoint(component);

      assertNull(result);
    }
  }

  @Test
  void shouldRegisterAndFindSourcePoint() {
    Map<Integer, Throwable> storage = new HashMap<>();

    try (MockedStatic<ObjectTable> mocked = mockStatic(ObjectTable.class)) {
      mocked.when(() -> ObjectTable.contains(any())).thenReturn(true);
      mocked.when(() -> ObjectTable.get(any())).thenReturn(storage);

      Object component = new Object();
      ComponentSourceRegistry.register(component);

      SourcePoint result = ComponentSourceRegistry.getSourcePoint(component);

      assertNotNull(result);
      assertNotNull(result.className());
      assertNotNull(result.fileName());
      assertTrue(result.lineNumber() > 0);
    }
  }

  @Test
  void shouldCreateStorageIfNotExists() {
    Map<Integer, Throwable> storage = new HashMap<>();

    try (MockedStatic<ObjectTable> mocked = mockStatic(ObjectTable.class)) {
      mocked.when(() -> ObjectTable.contains(any())).thenReturn(false);
      mocked.when(() -> ObjectTable.put(any(), any())).thenAnswer(inv -> storage);
      mocked.when(() -> ObjectTable.get(any())).thenReturn(storage);

      Object component = new Object();
      ComponentSourceRegistry.register(component);

      mocked.verify(() -> ObjectTable.put(eq(ComponentSourceRegistry.class.getName()), any()));
    }
  }

  @Test
  void shouldReturnEmptyChainForUnregisteredComponent() {
    Map<Integer, Throwable> storage = new HashMap<>();

    try (MockedStatic<ObjectTable> mocked = mockStatic(ObjectTable.class)) {
      mocked.when(() -> ObjectTable.contains(any())).thenReturn(true);
      mocked.when(() -> ObjectTable.get(any())).thenReturn(storage);

      Object component = new Object();
      List<SourcePoint> chain = ComponentSourceRegistry.getSourceChain(component);

      assertNotNull(chain);
      assertTrue(chain.isEmpty());
    }
  }

  @Test
  void shouldReturnChainWithFirstElementMatchingSourcePoint() {
    Map<Integer, Throwable> storage = new HashMap<>();

    try (MockedStatic<ObjectTable> mocked = mockStatic(ObjectTable.class)) {
      mocked.when(() -> ObjectTable.contains(any())).thenReturn(true);
      mocked.when(() -> ObjectTable.get(any())).thenReturn(storage);

      Object component = new Object();
      Throwable stack = new Throwable();
      stack.setStackTrace(new StackTraceElement[] {
          new StackTraceElement("com.webforj.component.ComponentSourceRegistry", "register",
              "ComponentSourceRegistry.java", 37),
          new StackTraceElement("com.example.app.MyView", "<init>", "MyView.java", 10),
          new StackTraceElement("com.example.app.MyApp", "run", "MyApp.java", 20)});
      storage.put(System.identityHashCode(component), stack);

      List<SourcePoint> chain = ComponentSourceRegistry.getSourceChain(component);
      SourcePoint sourcePoint = ComponentSourceRegistry.getSourcePoint(component);

      assertEquals(2, chain.size());
      assertEquals(sourcePoint, chain.get(0));
      assertEquals("com.example.app.MyView", chain.get(0).className());
      assertEquals("com.example.app.MyApp", chain.get(1).className());
    }
  }

  @Test
  void shouldExcludeFilteredPackagesFromChain() {
    Map<Integer, Throwable> storage = new HashMap<>();

    try (MockedStatic<ObjectTable> mocked = mockStatic(ObjectTable.class)) {
      mocked.when(() -> ObjectTable.contains(any())).thenReturn(true);
      mocked.when(() -> ObjectTable.get(any())).thenReturn(storage);

      Object component = new Object();
      Throwable stack = new Throwable();
      stack.setStackTrace(new StackTraceElement[] {
          new StackTraceElement("com.webforj.component.ComponentSourceRegistry", "register",
              "ComponentSourceRegistry.java", 37),
          new StackTraceElement("com.basis.internal.Helper", "help", "Helper.java", 5),
          new StackTraceElement("java.lang.Thread", "run", "Thread.java", 1),
          new StackTraceElement("jdk.internal.reflect.Foo", "invoke", "Foo.java", 2),
          new StackTraceElement("sun.reflect.Bar", "invoke", "Bar.java", 3),
          new StackTraceElement("com.example.app.MyView", "<init>", "MyView.java", 10)});
      storage.put(System.identityHashCode(component), stack);

      List<SourcePoint> chain = ComponentSourceRegistry.getSourceChain(component);

      assertEquals(1, chain.size());
      assertEquals("com.example.app.MyView", chain.get(0).className());
    }
  }

  @Test
  void shouldCapChainAtTenEntries() {
    Map<Integer, Throwable> storage = new HashMap<>();

    try (MockedStatic<ObjectTable> mocked = mockStatic(ObjectTable.class)) {
      mocked.when(() -> ObjectTable.contains(any())).thenReturn(true);
      mocked.when(() -> ObjectTable.get(any())).thenReturn(storage);

      Object component = new Object();
      StackTraceElement[] frames = new StackTraceElement[15];
      for (int i = 0; i < frames.length; i++) {
        frames[i] =
            new StackTraceElement("com.example.app.Frame" + i, "run", "Frame" + i + ".java", i);
      }

      Throwable stack = new Throwable();
      stack.setStackTrace(frames);
      storage.put(System.identityHashCode(component), stack);

      List<SourcePoint> chain = ComponentSourceRegistry.getSourceChain(component);

      assertEquals(10, chain.size());
      assertEquals("com.example.app.Frame0", chain.get(0).className());
      assertEquals("com.example.app.Frame9", chain.get(9).className());
    }
  }
}
