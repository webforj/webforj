package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;

import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.environment.ObjectTable;
import java.util.HashMap;
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
}
