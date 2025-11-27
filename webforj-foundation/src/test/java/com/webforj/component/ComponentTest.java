package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

import com.webforj.PendingResult;
import com.webforj.component.ComponentLifecycleObserver.LifecycleEvent;
import com.webforj.component.window.Window;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ComponentTest {
  ComponentMock component;

  @BeforeEach
  void setUp() {
    component = new ComponentMock();
  }

  @Test
  @DisplayName("Test getComponentId")
  void testGetComponentId() {
    String componentId = component.getComponentId();
    assertNotNull(componentId);
    assertNotEquals("", componentId);
  }

  @Test
  @DisplayName("Test setUserData and getUserData")
  void testSetAndGetUserData() {
    String key = "testKey";
    String value = "testValue";

    component.setUserData(key, value);
    Object retrievedValue = component.getUserData(key);

    assertNotNull(retrievedValue);
    assertEquals(value, retrievedValue);
  }

  @Test
  @DisplayName("Test destroy")
  void testDestroy() {
    assertFalse(component.isDestroyed());

    component.destroy();

    assertTrue(component.isDestroyed());
    assertNull(component.getComponentId());
    assertFalse(component.isAttached());
    assertNull(component.getWindow());
  }

  @Test
  @DisplayName("Test isAttached")
  void testIsAttached() {
    assertFalse(component.isAttached());

    component.create(mock(Window.class));
    assertTrue(component.isAttached());
  }

  @Test
  @DisplayName("Test getWindow")
  void testGetWindow() {
    assertNull(component.getWindow());

    component.create(mock(Window.class));
    assertNotNull(component.getWindow());
  }

  @Test
  @DisplayName("If a component is destroyed, it cannot be created again")
  void testComponentCreateThrowsIllegalAccessExceptionIfDestroyed() {
    Window window = mock(Window.class);

    component.create(window);
    component.destroy();


    assertThrows(IllegalStateException.class, () -> component.create(window));
  }

  @Test
  @DisplayName("Test component lifecycle methods")
  void testComponentLifecycleMethods() {
    ComponentMock spy = spy(ComponentMock.class);
    Window window = mock(Window.class);

    spy.create(window);

    verify(spy).onCreate(window);
    verify(spy).onAttach();

    spy.destroy();
    verify(spy).onDestroy();
  }

  @Test
  @DisplayName("Test Lifecycle observers")
  void testNotifyLifecycleObservers() {
    ComponentLifecycleObserver observer = mock(ComponentLifecycleObserver.class);

    component.addLifecycleObserver(observer);

    component.create(null);
    verify(observer).onComponentLifecycleEvent(component, LifecycleEvent.CREATE);

    component.destroy();
    verify(observer).onComponentLifecycleEvent(component, LifecycleEvent.DESTROY);
  }

  @Test
  @DisplayName("whenAttached should immediately complete if component is already attached")
  void testWhenAttachedAlreadyAttached() {
    component.create(mock(Window.class));
    PendingResult<Component> result = component.whenAttached();

    assertTrue(result.isDone());
  }

  @Test
  @DisplayName("whenAttached should complete when component is attached")
  void testWhenAttachedCompletesUponAttachment() {
    PendingResult<Component> result = component.whenAttached();

    assertFalse(result.isDone());
    component.create(mock(Window.class));
    assertTrue(result.isDone());
  }

  @Test
  @DisplayName("getOwner should return null initially")
  void shouldGetOwnerReturnsNullInitially() {
    assertNull(component.getOwner());
  }

  @Test
  @DisplayName("destroy should clear the owner")
  void shouldDestroyResetsOwner() {
    Component owner = spy(Component.class);
    component.setOwner(owner);

    assertEquals(owner, component.getOwner());

    component.destroy();

    assertNull(component.getOwner());
  }
}
