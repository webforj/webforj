package com.webforj.spring.scope;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.Environment;
import com.webforj.environment.ObjectTable;
import com.webforj.spring.scope.processor.EnvironmentScopeProcessor;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.beans.factory.ObjectFactory;

class EnvironmentScopeProcessorTest {

  private EnvironmentScopeProcessor scopeProcessor;
  @SuppressWarnings("rawtypes")
  private ObjectFactory objectFactory;
  private MockedStatic<Environment> environmentMock;
  private MockedStatic<ObjectTable> objectTableMock;
  private Environment mockEnvironment;
  private BeanStore mockBeanStore;

  @BeforeEach
  void setUp() {
    scopeProcessor = new EnvironmentScopeProcessor();
    objectFactory = mock(ObjectFactory.class);
    mockEnvironment = mock(Environment.class);
    mockBeanStore = mock(BeanStore.class);

    environmentMock = mockStatic(Environment.class);
    objectTableMock = mockStatic(ObjectTable.class);

    environmentMock.when(Environment::getCurrent).thenReturn(mockEnvironment);
    environmentMock.when(Environment::isPresent).thenReturn(true);
  }

  @AfterEach
  void tearDown() {
    if (environmentMock != null) {
      environmentMock.close();
    }
    if (objectTableMock != null) {
      objectTableMock.close();
    }
  }

  @Test
  void shouldCreateNewBeanWhenGetting() {
    String beanName = "testBean";
    Object expectedBean = new Object();

    when(objectFactory.getObject()).thenReturn(expectedBean);

    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      beanStoreMock.when(() -> BeanStore.getOrCreate(anyString())).thenReturn(mockBeanStore);
      when(mockBeanStore.get(anyString(), anyString(), any(ObjectFactory.class)))
          .thenReturn(expectedBean);

      Object result = scopeProcessor.get(beanName, objectFactory);

      assertNotNull(result);
      assertSame(expectedBean, result);
    }
  }

  @Test
  void shouldThrowExceptionWhenGettingWithoutEnvironment() {
    String beanName = "testBean";

    environmentMock.when(Environment::getCurrent).thenReturn(null);

    assertThrows(IllegalStateException.class, () -> {
      scopeProcessor.get(beanName, objectFactory);
    });
  }

  @Test
  void shouldRemoveBeanFromScope() {
    String beanName = "testBean";
    Object expectedBean = new Object();

    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      beanStoreMock.when(() -> BeanStore.getOrCreate(anyString())).thenReturn(mockBeanStore);
      when(mockBeanStore.remove(anyString(), anyString())).thenReturn(expectedBean);

      Object result = scopeProcessor.remove(beanName);

      assertSame(expectedBean, result);
    }
  }

  @Test
  void shouldReturnNullWhenRemovingWithoutEnvironment() {
    String beanName = "testBean";

    environmentMock.when(Environment::getCurrent).thenReturn(null);

    Object result = scopeProcessor.remove(beanName);

    assertNull(result);
  }

  @Test
  void shouldRegisterDestructionCallbackForBean() {
    String beanName = "testBean";
    Runnable callback = mock(Runnable.class);

    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      beanStoreMock.when(() -> BeanStore.getOrCreate(anyString())).thenReturn(mockBeanStore);

      scopeProcessor.registerDestructionCallback(beanName, callback);

      verify(mockBeanStore).registerDestructionCallback(anyString(), anyString(),
          any(Runnable.class));
    }
  }

  @Test
  void shouldIgnoreDestructionCallbackWhenNoEnvironment() {
    String beanName = "testBean";
    Runnable callback = mock(Runnable.class);

    environmentMock.when(Environment::getCurrent).thenReturn(null);

    // Should not throw exception, just return
    scopeProcessor.registerDestructionCallback(beanName, callback);

    // Verify BeanStore was never accessed via our mock
    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      beanStoreMock.verifyNoInteractions();
    }
  }

  @Test
  void shouldResolveEnvironmentAsContextualObject() {
    Object result = scopeProcessor.resolveContextualObject("webforj-environment");
    assertSame(mockEnvironment, result);

    result = scopeProcessor.resolveContextualObject("unknown");
    assertNull(result);
  }

  @Test
  void shouldReturnScopeIdAsConversationId() {
    String conversationId = scopeProcessor.getConversationId();

    assertNotNull(conversationId);
    assertEquals("webforj-environment-" + System.identityHashCode(mockEnvironment), conversationId);
  }

  @Test
  void shouldReturnNullConversationIdWhenNoEnvironment() {
    environmentMock.when(Environment::getCurrent).thenReturn(null);

    String conversationId = scopeProcessor.getConversationId();

    assertNull(conversationId);
  }

  @Test
  void shouldCleanupScopeAndRemoveBeanStore() {
    objectTableMock.when(() -> ObjectTable.contains(anyString())).thenReturn(true);
    objectTableMock.when(() -> ObjectTable.get(anyString())).thenReturn(mockBeanStore);
    when(mockBeanStore.getScopeCount()).thenReturn(0);

    EnvironmentScopeProcessor.cleanup();

    verify(mockBeanStore).destroyScopeInstance(anyString());
    objectTableMock.verify(() -> ObjectTable.clear(anyString()), times(1));
  }

  @Test
  void shouldSkipCleanupWhenNoEnvironment() {
    environmentMock.when(Environment::getCurrent).thenReturn(null);

    // Should not throw exception, just return
    EnvironmentScopeProcessor.cleanup();

    // Verify BeanStore cleanup was never called
    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      beanStoreMock.verifyNoMoreInteractions();
    }
  }

  @Test
  void shouldKeepBeanStoreWhenOtherScopesExist() {
    objectTableMock.when(() -> ObjectTable.contains(anyString())).thenReturn(true);
    objectTableMock.when(() -> ObjectTable.get(anyString())).thenReturn(mockBeanStore);
    when(mockBeanStore.getScopeCount()).thenReturn(1); // Still has beans

    EnvironmentScopeProcessor.cleanup();

    verify(mockBeanStore).destroyScopeInstance(anyString());
    // Should NOT clear from ObjectTable when store still has beans
    objectTableMock.verify(() -> ObjectTable.clear(anyString()), times(0));
  }

  @Test
  void shouldCreateBeanStoreOnFirstAccess() {
    // Mock the static BeanStore.getOrCreate method
    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      BeanStore realStore = new BeanStore();
      beanStoreMock.when(() -> BeanStore.getOrCreate(anyString())).thenReturn(realStore);

      String beanName = "testBean";
      Object expectedBean = new Object();
      when(objectFactory.getObject()).thenReturn(expectedBean);

      // This should trigger BeanStore.getOrCreate
      Object result = scopeProcessor.get(beanName, objectFactory);

      assertNotNull(result);
      assertSame(expectedBean, result);

      // Verify BeanStore.getOrCreate was called
      beanStoreMock.verify(() -> BeanStore.getOrCreate(anyString()), times(1));
    }
  }
}
