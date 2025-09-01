package com.webforj.spring.scope.processor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.environment.SessionObjectTable;
import com.webforj.spring.scope.BeanStore;
import com.webforj.spring.scope.BeanStore.StorageType;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.beans.factory.ObjectFactory;

class SessionScopeProcessorTest {

  private SessionScopeProcessor scopeProcessor;
  @SuppressWarnings("rawtypes")
  private ObjectFactory objectFactory;
  private MockedStatic<BeanStore> beanStoreMock;
  private MockedStatic<SessionObjectTable> sessionTableMock;
  private BeanStore mockBeanStore;

  @BeforeEach
  void setUp() {
    scopeProcessor = new SessionScopeProcessor();
    objectFactory = mock(ObjectFactory.class);
    mockBeanStore = mock(BeanStore.class);

    beanStoreMock = mockStatic(BeanStore.class);
    sessionTableMock = mockStatic(SessionObjectTable.class);

    // Mock SessionObjectTable to always return true for contains
    sessionTableMock.when(() -> SessionObjectTable.contains(anyString())).thenReturn(true);
  }

  @AfterEach
  void tearDown() {
    if (beanStoreMock != null) {
      beanStoreMock.close();
    }
    if (sessionTableMock != null) {
      sessionTableMock.close();
    }
  }

  @Test
  void shouldCreateNewBeanWhenGetting() {
    String beanName = "testBean";
    Object expectedBean = new Object();
    when(objectFactory.getObject()).thenReturn(expectedBean);

    beanStoreMock
        .when(
            () -> BeanStore.getOrCreate(SessionScopeProcessor.BEAN_STORE_KEY, StorageType.SESSION))
        .thenReturn(mockBeanStore);
    when(mockBeanStore.get(eq("session"), eq(beanName), any())).thenReturn(expectedBean);

    Object result = scopeProcessor.get(beanName, objectFactory);

    assertNotNull(result);
    assertSame(expectedBean, result);
    verify(mockBeanStore).get(eq("session"), eq(beanName), any());
  }

  @Test
  void shouldReturnCachedBean() {
    String beanName = "cachedBean";
    Object cachedBean = new Object();

    beanStoreMock
        .when(
            () -> BeanStore.getOrCreate(SessionScopeProcessor.BEAN_STORE_KEY, StorageType.SESSION))
        .thenReturn(mockBeanStore);
    when(mockBeanStore.get(eq("session"), eq(beanName), any())).thenReturn(cachedBean);

    Object result1 = scopeProcessor.get(beanName, objectFactory);
    Object result2 = scopeProcessor.get(beanName, objectFactory);

    assertSame(result1, result2);
    verify(mockBeanStore, times(2)).get(eq("session"), eq(beanName), any());
  }

  @Test
  void shouldRemoveBeanFromScope() {
    String beanName = "beanToRemove";
    Object removedBean = new Object();

    beanStoreMock
        .when(
            () -> BeanStore.getOrCreate(SessionScopeProcessor.BEAN_STORE_KEY, StorageType.SESSION))
        .thenReturn(mockBeanStore);
    when(mockBeanStore.remove("session", beanName)).thenReturn(removedBean);

    Object result = scopeProcessor.remove(beanName);

    assertSame(removedBean, result);
    verify(mockBeanStore).remove("session", beanName);
  }

  @Test
  void shouldReturnNullWhenRemovingWithoutSession() {
    String beanName = "beanToRemove";

    beanStoreMock
        .when(
            () -> BeanStore.getOrCreate(SessionScopeProcessor.BEAN_STORE_KEY, StorageType.SESSION))
        .thenThrow(new IllegalStateException("Session not available"));

    Object result = scopeProcessor.remove(beanName);

    assertNull(result);
  }

  @Test
  void shouldRegisterDestructionCallbackForBean() {
    String beanName = "beanWithCallback";
    Runnable callback = mock(Runnable.class);

    beanStoreMock
        .when(
            () -> BeanStore.getOrCreate(SessionScopeProcessor.BEAN_STORE_KEY, StorageType.SESSION))
        .thenReturn(mockBeanStore);

    scopeProcessor.registerDestructionCallback(beanName, callback);

    verify(mockBeanStore).registerDestructionCallback("session", beanName, callback);
  }

  @Test
  void shouldIgnoreDestructionCallbackWhenNoSession() {
    String beanName = "beanWithCallback";
    Runnable callback = mock(Runnable.class);

    beanStoreMock
        .when(
            () -> BeanStore.getOrCreate(SessionScopeProcessor.BEAN_STORE_KEY, StorageType.SESSION))
        .thenThrow(new IllegalStateException("Session not available"));

    scopeProcessor.registerDestructionCallback(beanName, callback);

    verify(mockBeanStore, times(0)).registerDestructionCallback(anyString(), anyString(), any());
  }

  @Test
  void shouldReturnNullForContextualObject() {
    Object result = scopeProcessor.resolveContextualObject("anyKey");

    assertNull(result);
  }

  @Test
  void shouldReturnSessionAsConversationId() {
    String conversationId = scopeProcessor.getConversationId();

    assertEquals(SessionScopeProcessor.SCOPE_ID, conversationId);
  }

  @Test
  void shouldCleanupAllSessionScopedBeans() {
    beanStoreMock.when(() -> BeanStore
        .cleanupAllScopeInstances(SessionScopeProcessor.BEAN_STORE_KEY, StorageType.SESSION))
        .thenAnswer(invocation -> null);

    SessionScopeProcessor.cleanup();

    beanStoreMock.verify(() -> BeanStore
        .cleanupAllScopeInstances(SessionScopeProcessor.BEAN_STORE_KEY, StorageType.SESSION));
  }
}
