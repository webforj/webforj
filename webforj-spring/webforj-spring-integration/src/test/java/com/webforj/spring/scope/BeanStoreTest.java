package com.webforj.spring.scope;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.ObjectFactory;

/**
 * Unit tests for BeanStore.
 */
class BeanStoreTest {

  private BeanStore beanStore;
  @SuppressWarnings("rawtypes")
  private ObjectFactory objectFactory;

  @BeforeEach
  void setUp() {
    beanStore = new BeanStore();
    objectFactory = mock(ObjectFactory.class);
  }

  @Test
  void shouldCreateNewBeanOnFirstGet() {
    String scopeId = "scope1";
    String beanName = "bean1";
    Object expectedBean = new Object();

    when(objectFactory.getObject()).thenReturn(expectedBean);

    Object actualBean = beanStore.get(scopeId, beanName, objectFactory);

    assertNotNull(actualBean);
    assertSame(expectedBean, actualBean);
    verify(objectFactory, times(1)).getObject();
  }

  @Test
  void shouldReturnCachedBeanOnSubsequentGets() {
    String scopeId = "scope1";
    String beanName = "bean1";
    Object expectedBean = new Object();

    when(objectFactory.getObject()).thenReturn(expectedBean);

    // First call creates the bean
    Object firstCall = beanStore.get(scopeId, beanName, objectFactory);
    // Second call should return cached bean
    Object secondCall = beanStore.get(scopeId, beanName, objectFactory);

    assertSame(firstCall, secondCall);
    verify(objectFactory, times(1)).getObject();
  }


  @Test
  void shouldRemoveBeanFromStore() {
    String scopeId = "scope1";
    String beanName = "bean1";
    Object expectedBean = new Object();

    when(objectFactory.getObject()).thenReturn(expectedBean);

    // Create bean
    beanStore.get(scopeId, beanName, objectFactory);

    // Remove bean
    Object removed = beanStore.remove(scopeId, beanName);
    assertSame(expectedBean, removed);

    // Bean should no longer exist
    assertFalse(beanStore.hasBeans(scopeId));
  }

  @Test
  void shouldExecuteDestructionCallbackWhenRemovingBean() {
    String scopeId = "scope1";
    String beanName = "bean1";
    Object expectedBean = new Object();
    Runnable callback = mock(Runnable.class);

    when(objectFactory.getObject()).thenReturn(expectedBean);

    // Create bean and register callback
    beanStore.get(scopeId, beanName, objectFactory);
    beanStore.registerDestructionCallback(scopeId, beanName, callback);

    // Remove bean - should execute callback
    Object removed = beanStore.remove(scopeId, beanName);

    assertSame(expectedBean, removed);
    verify(callback, times(1)).run();
  }

  @Test
  void shouldReturnNullWhenRemovingNonExistentBean() {
    String scopeId = "scope1";
    String beanName = "nonexistent";

    Object removed = beanStore.remove(scopeId, beanName);
    assertNull(removed);
  }

  @Test
  void shouldExecuteDestructionCallbackOnDestroy() {
    String scopeId = "scope1";
    String beanName = "bean1";
    Runnable callback = mock(Runnable.class);

    // Register callback
    beanStore.registerDestructionCallback(scopeId, beanName, callback);

    // Destroy beans for this scope
    beanStore.destroyScopeInstance(scopeId);

    // Callback should have been executed
    verify(callback, times(1)).run();
  }

  @Test
  void shouldContinueDestroyingBeansEvenWhenCallbackThrows() {
    String scopeId = "scope1";
    String beanName1 = "bean1";
    String beanName2 = "bean2";
    Runnable callback1 = mock(Runnable.class);
    Runnable callback2 = mock(Runnable.class);

    // Make first callback throw exception
    doThrow(new RuntimeException("Test exception")).when(callback1).run();

    beanStore.registerDestructionCallback(scopeId, beanName1, callback1);
    beanStore.registerDestructionCallback(scopeId, beanName2, callback2);

    // Destroy should not fail despite callback1 throwing exception
    beanStore.destroyScopeInstance(scopeId);

    // Both callbacks should have been attempted
    verify(callback1, times(1)).run();
    verify(callback2, times(1)).run();
  }

  @Test
  void shouldNotExecuteRemovedDestructionCallback() {
    String scopeId = "scope1";
    String beanName = "bean1";
    Runnable callback = mock(Runnable.class);

    // Register and then remove callback
    beanStore.registerDestructionCallback(scopeId, beanName, callback);
    beanStore.removeDestructionCallback(scopeId, beanName);

    // Destroy beans - callback should not be executed
    beanStore.destroyScopeInstance(scopeId);

    verify(callback, times(0)).run();
  }

  @Test
  void shouldDestroyAllBeansInScope() {
    String scopeId = "scope1";
    String beanName = "bean1";
    Object bean = new Object();
    Runnable callback = mock(Runnable.class);

    when(objectFactory.getObject()).thenReturn(bean);

    // Create bean and register callback
    beanStore.get(scopeId, beanName, objectFactory);
    beanStore.registerDestructionCallback(scopeId, beanName, callback);

    // Destroy beans for this scope
    beanStore.destroyScopeInstance(scopeId);

    // Callback should have been executed
    verify(callback, times(1)).run();

    // Bean should no longer exist
    assertFalse(beanStore.hasBeans(scopeId));
  }

  @Test
  void shouldDestroyAllScopesAndBeans() {
    String scopeId1 = "scope1";
    String scopeId2 = "scope2";
    String beanName = "bean1";

    Object bean1 = new Object();
    Object bean2 = new Object();
    Runnable callback1 = mock(Runnable.class);
    Runnable callback2 = mock(Runnable.class);

    // Create beans in different scopes
    when(objectFactory.getObject()).thenReturn(bean1, bean2);
    beanStore.get(scopeId1, beanName, objectFactory);
    beanStore.get(scopeId2, beanName, objectFactory);

    // Register callbacks
    beanStore.registerDestructionCallback(scopeId1, beanName, callback1);
    beanStore.registerDestructionCallback(scopeId2, beanName, callback2);

    // Destroy all
    beanStore.destroyAllScopeInstances();

    // Both callbacks should have been executed
    verify(callback1, times(1)).run();
    verify(callback2, times(1)).run();

    // No beans should exist
    assertEquals(0, beanStore.getScopeCount());
  }

  @Test
  void shouldReportWhetherScopeHasBeans() {
    String scopeId = "scope1";
    String beanName = "bean1";

    // Initially no beans
    assertFalse(beanStore.hasBeans(scopeId));

    // Create bean
    when(objectFactory.getObject()).thenReturn(new Object());
    beanStore.get(scopeId, beanName, objectFactory);

    // Now has beans
    assertTrue(beanStore.hasBeans(scopeId));

    // Remove bean
    beanStore.remove(scopeId, beanName);

    // No longer has beans
    assertFalse(beanStore.hasBeans(scopeId));
  }

  @Test
  void shouldTrackNumberOfScopes() {
    assertEquals(0, beanStore.getScopeCount());

    // Create beans in different scopes
    when(objectFactory.getObject()).thenReturn(new Object());
    beanStore.get("scope1", "bean1", objectFactory);
    assertEquals(1, beanStore.getScopeCount());

    beanStore.get("scope2", "bean1", objectFactory);
    assertEquals(2, beanStore.getScopeCount());

    // Adding another bean to existing scope doesn't increase count
    beanStore.get("scope1", "bean2", objectFactory);
    assertEquals(2, beanStore.getScopeCount());
  }

  @Test
  void shouldTrackNumberOfBeansInScope() {
    String scopeId = "scope1";

    // Initially 0 beans
    assertEquals(0, beanStore.getBeanCount(scopeId));

    // Add beans
    when(objectFactory.getObject()).thenReturn(new Object());
    beanStore.get(scopeId, "bean1", objectFactory);
    assertEquals(1, beanStore.getBeanCount(scopeId));

    beanStore.get(scopeId, "bean2", objectFactory);
    assertEquals(2, beanStore.getBeanCount(scopeId));

    // Remove a bean
    beanStore.remove(scopeId, "bean1");
    assertEquals(1, beanStore.getBeanCount(scopeId));
  }

  @Test
  void shouldIsolateBeansBetweenDifferentScopes() {
    String scopeId1 = "scope1";
    String scopeId2 = "scope2";
    String beanName = "bean1";

    Object bean1 = new Object();
    Object bean2 = new Object();

    // Create different beans with same name in different scopes
    when(objectFactory.getObject()).thenReturn(bean1, bean2);

    Object scope1Bean = beanStore.get(scopeId1, beanName, objectFactory);
    Object scope2Bean = beanStore.get(scopeId2, beanName, objectFactory);

    // Beans should be different instances
    assertNotNull(scope1Bean);
    assertNotNull(scope2Bean);
    assertNotEquals(scope1Bean, scope2Bean);

    // Removing from one scope doesn't affect the other
    beanStore.remove(scopeId1, beanName);
    assertFalse(beanStore.hasBeans(scopeId1));
    assertTrue(beanStore.hasBeans(scopeId2));
  }
}
