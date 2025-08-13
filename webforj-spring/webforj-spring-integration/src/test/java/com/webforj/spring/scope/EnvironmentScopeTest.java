package com.webforj.spring.scope;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjObjectTable;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.bridge.WebforjBBjBridge;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.beans.factory.ObjectFactory;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class EnvironmentScopeTest {

  @Mock
  private BBjAPI mockApi;

  @Mock
  private BBjObjectTable mockObjectTable;

  @Mock
  private WebforjBBjBridge mockBridge;

  private EnvironmentScope scope;
  private Map<String, Object> objectTableStorage;

  @BeforeEach
  void setUp() throws BBjException {
    scope = new EnvironmentScope();
    objectTableStorage = new HashMap<>();

    // Mock the ObjectTable behavior
    when(mockApi.getObjectTable()).thenReturn(mockObjectTable);
    // Mock put
    doAnswer(invocation -> {
      String key = invocation.getArgument(0);
      Object value = invocation.getArgument(1);
      objectTableStorage.put(key, value);
      return null;
    }).when(mockObjectTable).put(anyString(), any());
    when(mockObjectTable.get(anyString())).thenAnswer(invocation -> {
      String key = invocation.getArgument(0);
      Object value = objectTableStorage.get(key);
      if (value == null) {
        throw new BBjException("Key not found: " + key, 0);
      }
      return value;
    });
    // Mock remove
    doAnswer(invocation -> {
      String key = invocation.getArgument(0);
      objectTableStorage.remove(key);
      return null;
    }).when(mockObjectTable).remove(anyString());
    when(mockObjectTable.size()).thenAnswer(invocation -> objectTableStorage.size());

    Environment.init(mockApi, mockBridge, 0);
  }

  @AfterEach
  void tearDown() {
    Environment.cleanup();
    objectTableStorage.clear();
  }

  @Nested
  class BasicOperations {

    @Test
    void shouldCreateBeanWhenNotExists() {
      ObjectFactory<TestBean> factory = TestBean::new;

      Object bean = scope.get("testBean", factory);

      assertNotNull(bean);
      assertTrue(bean instanceof TestBean);
    }

    @Test
    void shouldReturnSameBeanInstance() {
      ObjectFactory<TestBean> factory = TestBean::new;

      Object bean1 = scope.get("testBean", factory);
      Object bean2 = scope.get("testBean", factory);

      assertSame(bean1, bean2);
      assertEquals(bean1, bean2);
    }

    @Test
    void shouldCreateDifferentInstancesForDifferentNames() {
      ObjectFactory<TestBean> factory = TestBean::new;

      Object bean1 = scope.get("bean1", factory);
      Object bean2 = scope.get("bean2", factory);

      assertNotSame(bean1, bean2);
    }

    @Test
    void shouldThrowExceptionWhenNoEnvironment() {
      Environment.cleanup();
      ObjectFactory<TestBean> factory = TestBean::new;

      assertThrows(IllegalStateException.class, () -> scope.get("testBean", factory),
          "Should throw IllegalStateException when no environment");
    }
  }

  @Nested
  class BeanRemoval {

    @Test
    void shouldRemoveExistingBean() {
      ObjectFactory<TestBean> factory = TestBean::new;
      Object bean = scope.get("testBean", factory);

      Object removed = scope.remove("testBean");

      assertNotNull(removed);
      assertEquals(bean, removed);

      // After removal, getting the bean should create a new instance
      Object newBean = scope.get("testBean", factory);
      assertNotSame(bean, newBean);
    }
  }

  @Nested
  class DestructionCallbacks {

    @Test
    void shouldExecuteDestructionCallback() {
      ObjectFactory<TestBean> factory = TestBean::new;
      scope.get("testBean", factory);

      AtomicBoolean callbackExecuted = new AtomicBoolean(false);
      Runnable callback = () -> callbackExecuted.set(true);

      scope.registerDestructionCallback("testBean", callback);
      scope.remove("testBean");

      assertTrue(callbackExecuted.get(), "Destruction callback should be executed");
    }

    @Test
    void shouldExecuteMultipleCallbacksInCleanup() {
      AtomicInteger callbackCount = new AtomicInteger(0);

      for (int i = 0; i < 5; i++) {
        String beanName = "bean" + i;
        scope.get(beanName, TestBean::new);
        scope.registerDestructionCallback(beanName, callbackCount::incrementAndGet);
      }

      EnvironmentScope.cleanup();

      assertEquals(5, callbackCount.get());
    }

    @Test
    void shouldHandleCleanupWithFailingCallbacks() {
      scope.get("bean1", TestBean::new);
      scope.registerDestructionCallback("bean1", () -> {
        throw new RuntimeException("Callback 1 failed");
      });

      scope.get("bean2", TestBean::new);
      AtomicBoolean bean2CallbackExecuted = new AtomicBoolean(false);
      scope.registerDestructionCallback("bean2", () -> bean2CallbackExecuted.set(true));

      // Cleanup should continue despite failing callbacks
      EnvironmentScope.cleanup();

      assertTrue(bean2CallbackExecuted.get(),
          "Other callbacks should still execute even if one fails");
    }
  }

  @Nested
  class ContextualObjects {

    @Test
    void shouldResolveEnvironment() {
      Object contextual = scope.resolveContextualObject("environment");

      assertNotNull(contextual);
      assertTrue(contextual instanceof Environment);
      assertEquals(Environment.getCurrent(), contextual);
    }

    @Test
    void shouldReturnNullForUnknownContextual() {
      Object contextual = scope.resolveContextualObject("unknown");

      assertNull(contextual);
    }
  }

  @Nested
  class ConversationId {

    @Test
    void shouldReturnUniqueConversationId() {
      String id1 = scope.getConversationId();

      assertNotNull(id1);
      assertTrue(id1.startsWith("webforj-environment-"));

      // Same environment should return same ID
      String id2 = scope.getConversationId();
      assertEquals(id1, id2);
    }

    @Test
    void shouldReturnNullWhenNoEnvironment() {
      Environment.cleanup();

      String id = scope.getConversationId();

      assertNull(id);
    }
  }

  // Test bean class
  static class TestBean {
    private static final AtomicInteger instanceCounter = new AtomicInteger(0);
    private final int instanceId;

    public TestBean() {
      this.instanceId = instanceCounter.incrementAndGet();
    }

    public int getInstanceId() {
      return instanceId;
    }
  }
}
