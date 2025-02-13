package com.webforj.environment.namespace;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjNamespace;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.Environment;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.environment.namespace.event.NamespaceAccessEvent;
import com.webforj.environment.namespace.event.NamespaceChangeEvent;
import com.webforj.environment.namespace.event.NamespaceKeyAccessEvent;
import com.webforj.environment.namespace.event.NamespaceKeyChangeEvent;
import com.webforj.environment.namespace.exception.NamespaceLockedException;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.function.Function;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class NamespaceTest {

  Namespace namespace;
  BBjNamespace bbjNamespace;

  @BeforeEach
  void setUp() throws BBjException {
    BBjAPI api = mock(BBjAPI.class);
    WebforjBBjBridge bridge = mock(WebforjBBjBridge.class);
    Environment.init(api, bridge, 0);

    bbjNamespace = mock(BBjNamespace.class);
    namespace = new GlobalNamespace();
    namespace.setBbjNamespace(bbjNamespace);
  }

  @AfterEach
  void tearDown() {
    Environment.cleanup();
  }

  @Nested
  class PutTests {
    @Test
    void shouldPutValueInNamespace() throws BBjException, NamespaceLockedException {
      doNothing().when(bbjNamespace).setValue(anyString(), any(), anyLong());
      namespace.put("key", "value", 1000L);
      verify(bbjNamespace, times(1)).setValue("key", "value", 1000L);
    }

    @Test
    void shouldThrowExceptionWhenPutFails() throws BBjException {
      doThrow(BBjException.class).when(bbjNamespace).setValue(anyString(), any(), anyLong());
      assertThrows(NamespaceLockedException.class, () -> namespace.put("key", "value", 1000L));
    }
  }

  @Nested
  class AtomicPutTests {
    @Test
    void shouldAtomicPutValueInNamespace() throws BBjException, NamespaceLockedException {
      doNothing().when(bbjNamespace).setLock(anyString(), anyLong());
      doNothing().when(bbjNamespace).setValue(anyString(), any(), anyLong());
      doNothing().when(bbjNamespace).removeLock(anyString());

      namespace.atomicPut("key", "value", 1000L);

      verify(bbjNamespace, times(1)).setLock("key", 1000L);
      verify(bbjNamespace, times(1)).setValue("key", "value", 1000L);
      verify(bbjNamespace, times(1)).removeLock("key");
    }

    @Test
    void shouldThrowExceptionWhenAtomicPutFails() throws BBjException {
      doThrow(BBjException.class).when(bbjNamespace).setLock(anyString(), anyLong());

      assertThrows(NamespaceLockedException.class,
          () -> namespace.atomicPut("key", "value", 1000L));

      verify(bbjNamespace, times(1)).setLock("key", 1000L);
      verify(bbjNamespace, times(0)).setValue(anyString(), any(), anyLong());
      verify(bbjNamespace, times(1)).removeLock("key");
    }
  }

  @Nested
  class GetTests {
    @Test
    void shouldGetValueFromNamespace() throws BBjException {
      when(bbjNamespace.getValue("key")).thenReturn("value");
      assertEquals("value", namespace.get("key"));
    }

    @Test
    void shouldThrowExceptionWhenGetFails() throws BBjException {
      when(bbjNamespace.getValue("key")).thenThrow(BBjException.class);
      assertThrows(NoSuchElementException.class, () -> namespace.get("key"));
    }
  }

  @Nested
  class GetOrDefaultTests {
    @Test
    void shouldReturnDefaultValueWhenKeyDoesNotExist() throws BBjException {
      when(bbjNamespace.getValue("key")).thenThrow(BBjException.class);
      assertEquals("default", namespace.getOrDefault("key", "default"));
    }

    @Test
    void shouldReturnValueWhenKeyExists() throws BBjException {
      when(bbjNamespace.getValue("key")).thenReturn("value");
      assertEquals("value", namespace.getOrDefault("key", "default"));
    }
  }

  @Nested
  class ContainsTests {
    @Test
    void shouldReturnTrueWhenKeyExists() throws BBjException {
      when(bbjNamespace.getValue("key")).thenReturn("value");
      assertTrue(namespace.contains("key"));
    }

    @Test
    void shouldReturnFalseWhenKeyDoesNotExist() throws BBjException {
      when(bbjNamespace.getValue("key")).thenThrow(BBjException.class);
      assertFalse(namespace.contains("key"));
    }
  }

  @Nested
  class ComputeIfAbsentTests {
    @Test
    void shouldComputeValueWhenKeyDoesNotExist() throws BBjException, NamespaceLockedException {
      when(bbjNamespace.getValue("key")).thenThrow(BBjException.class);
      doNothing().when(bbjNamespace).setValue(anyString(), any(), anyLong());

      Function<String, Object> mappingFunction = key -> "computedValue";
      assertEquals("computedValue", namespace.computeIfAbsent("key", mappingFunction));

      verify(bbjNamespace, times(1)).setValue("key", "computedValue", 20);
    }

    @Test
    void shouldReturnExistingValueWhenKeyExists() throws BBjException, NamespaceLockedException {
      when(bbjNamespace.getValue("key")).thenReturn("value");

      Function<String, Object> mappingFunction = key -> "computedValue";
      assertEquals("value", namespace.computeIfAbsent("key", mappingFunction));

      verify(bbjNamespace, times(0)).setValue(anyString(), any(), anyLong());
    }
  }

  @Nested
  class RemoveTests {
    @Test
    void shouldRemoveValueFromNamespace() throws BBjException, NamespaceLockedException {
      doNothing().when(bbjNamespace).removeValue(anyString(), anyLong());
      namespace.remove("key", 1000L);
      verify(bbjNamespace, times(1)).removeValue("key", 1000L);
    }

    @Test
    void shouldThrowExceptionWhenRemoveFails() throws BBjException {
      doThrow(BBjException.class).when(bbjNamespace).removeValue(anyString(), anyLong());
      assertThrows(NamespaceLockedException.class, () -> namespace.remove("key", 1000L));
    }
  }

  @Nested
  class KeySetTests {
    @SuppressWarnings("unchecked")
    @Test
    void shouldReturnKeySet() throws BBjException {
      BBjVector keys = new BBjVector();
      keys.add("key1");
      keys.add("key2");
      when(bbjNamespace.getKeys()).thenReturn(keys);

      Set<String> keySet = namespace.keySet();
      assertTrue(keySet.contains("key1"));
      assertTrue(keySet.contains("key2"));
    }

    @Test
    void shouldReturnEmptyKeySetWhenExceptionThrown() throws BBjException {
      when(bbjNamespace.getKeys()).thenThrow(BBjException.class);
      Set<String> keySet = namespace.keySet();
      assertTrue(keySet.isEmpty());
    }
  }

  @Nested
  class SizeTests {
    @SuppressWarnings("unchecked")
    @Test
    void shouldReturnNamespaceSize() throws BBjException {
      BBjVector keys = new BBjVector();
      keys.add("key1");
      keys.add("key2");
      when(bbjNamespace.getKeys()).thenReturn(keys);
      assertEquals(2, namespace.size());
    }

    @Test
    void shouldReturnZeroWhenSizeFails() throws BBjException {
      when(bbjNamespace.getKeys()).thenThrow(BBjException.class);
      assertEquals(0, namespace.size());
    }
  }

  @Nested
  class ClearTests {
    @Test
    void shouldClearNamespace() {
      doNothing().when(bbjNamespace).clear();
      namespace.clear();
      verify(bbjNamespace, times(1)).clear();
    }
  }

  @Nested
  class LockTests {
    @Test
    void shouldSetLockOnNamespaceVariable() throws BBjException, NamespaceLockedException {
      doNothing().when(bbjNamespace).setLock(anyString(), anyLong());
      namespace.setLock("key", 1000L);
      verify(bbjNamespace, times(1)).setLock("key", 1000L);
    }

    @Test
    void shouldThrowExceptionWhenSetLockFails() throws BBjException {
      doThrow(BBjException.class).when(bbjNamespace).setLock(anyString(), anyLong());
      assertThrows(NamespaceLockedException.class, () -> namespace.setLock("key", 1000L));
    }

    @Test
    void shouldRemoveLockFromNamespaceVariable() throws BBjException {
      doNothing().when(bbjNamespace).removeLock(anyString());
      namespace.removeLock("key");
      verify(bbjNamespace, times(1)).removeLock("key");
    }

    @Test
    void shouldHandleExceptionWhenRemoveLockFails() throws BBjException {
      doThrow(BBjException.class).when(bbjNamespace).removeLock(anyString());
      namespace.removeLock("key");
      verify(bbjNamespace, times(1)).removeLock("key");
    }
  }

  @Nested
  class ListenerTests {
    @Test
    void shouldAddNamespaceAccessListener() throws BBjException {
      EventListener<NamespaceAccessEvent> listener = mock(EventListener.class);
      ListenerRegistration<NamespaceAccessEvent> registration = namespace.onAccess(listener);
      assertNotNull(registration);
      verify(bbjNamespace, times(1)).setCallbackForNamespace(any(), anyString());
    }

    @Test
    void shouldAddNamespaceChangeListener() throws BBjException {
      EventListener<NamespaceChangeEvent> listener = mock(EventListener.class);
      ListenerRegistration<NamespaceChangeEvent> registration = namespace.onChange(listener);
      assertNotNull(registration);
      verify(bbjNamespace, times(1)).setCallbackForNamespaceChange(any(), anyString());
    }

    @Test
    void shouldAddNamespaceKeyAccessListener() throws BBjException {
      EventListener<NamespaceKeyAccessEvent> listener = mock(EventListener.class);
      ListenerRegistration<NamespaceKeyAccessEvent> registration =
          namespace.onKeyAccess("key", listener);
      assertNotNull(registration);
      verify(bbjNamespace, times(1)).setCallbackForVariable(eq("key"), any(), anyString());
    }

    @Test
    void shouldAddNamespaceKeyChangeListener() throws BBjException {
      EventListener<NamespaceKeyChangeEvent> listener = mock(EventListener.class);
      ListenerRegistration<NamespaceKeyChangeEvent> registration =
          namespace.onKeyChange("key", listener);
      assertNotNull(registration);
      verify(bbjNamespace, times(1)).setCallbackForVariableChange(eq("key"), any(), anyString());
    }
  }
}
