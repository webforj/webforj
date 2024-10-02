package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.function.Consumer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

class ComponentRegistryTest {

  private ComponentRegistry registry;
  private Component parent;
  private Consumer<Component> consumer;
  private Component component;

  @BeforeEach
  void setUp() {
    parent = spy(Component.class);
    consumer = mock(Consumer.class);
    component = spy(Component.class);

    registry = new ComponentRegistry(parent, consumer);

    when(component.getComponentId()).thenReturn("comp1");
    when(component.isDestroyed()).thenReturn(false);
    when(parent.isAttached()).thenReturn(false);
  }

  @Test
  @DisplayName("Expect NullPointerException when adding a null array of components")
  void addShouldThrowExceptionForNullComponents() {
    Executable action = () -> registry.add((Component[]) null);
    assertThrows(NullPointerException.class, action);
  }

  @Test
  @DisplayName("Expect NullPointerException when adding a single null component")
  void addShouldThrowExceptionForNullComponent() {
    Executable action = () -> registry.add(new Component[] {null});
    assertThrows(NullPointerException.class, action);
  }

  @Test
  @DisplayName("Expect IllegalArgumentException when adding a duplicate component")
  void addShouldThrowExceptionForDuplicateComponent() {
    registry.add(component);
    Executable action = () -> registry.add(component);
    assertThrows(IllegalArgumentException.class, action);
  }

  @Test
  @DisplayName("Expect IllegalArgumentException when adding an already attached component")
  void addShouldThrowExceptionWhenAddingAttachedComponent() {
    Component attachedComponent = spy(Component.class);
    when(attachedComponent.getComponentId()).thenReturn("attachedComp");
    when(attachedComponent.isAttached()).thenReturn(true);

    Executable action = () -> registry.add(attachedComponent);

    IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, action);
    assertTrue(exception.getMessage().contains("is already attached to a different component"));
  }

  @Test
  @DisplayName("Expect IllegalStateException when adding a destroyed component")
  void addShouldThrowExceptionWhenAddingDestroyedComponent() {
    Component destroyedComponent = spy(Component.class);
    when(destroyedComponent.getComponentId()).thenReturn("destroyedComp");
    when(destroyedComponent.isDestroyed()).thenReturn(true);

    Executable action = () -> registry.add(destroyedComponent);

    assertThrows(IllegalStateException.class, action);
  }

  @Test
  @DisplayName("Should call consumer when adding a component to an attached parent")
  void addShouldCallConsumerForAttachedParent() {
    when(parent.isAttached()).thenReturn(true);
    registry.add(component);
    verify(consumer).accept(component);
  }

  @Test
  @DisplayName("Expect NullPointerException when removing a null array of components")
  void removeShouldThrowExceptionForNullComponents() {
    Executable action = () -> registry.remove((Component[]) null);
    assertThrows(NullPointerException.class, action);
  }

  @Test
  @DisplayName("Expect NullPointerException when removing a single null component")
  void removeShouldThrowExceptionForNullComponent() {
    Executable action = () -> registry.remove(new Component[] {null});
    assertThrows(NullPointerException.class, action);
  }

  @Test
  @DisplayName("Component should be destroyed and removed from registry upon removal")
  void removeShouldDestroyAndRemoveComponent() {
    registry.add(component);
    registry.remove(component);

    verify(component).destroy();
    assertFalse(registry.getComponents().contains(component));
  }

  @Test
  @DisplayName("All components should be destroyed and registry cleared on removeAll")
  void removeAllShouldDestroyAndClearAllComponents() {
    Component secondComponent = spy(Component.class);
    when(secondComponent.getComponentId()).thenReturn("comp2");
    registry.add(component, secondComponent);

    registry.removeAll();

    verify(component).destroy();
    verify(secondComponent).destroy();
    assertTrue(registry.getComponents().isEmpty());
  }

  @Test
  @DisplayName("Should monitor components destroyed event and remove from registry")
  void shouldMonitorComponentDestroyedEvent() {
    Component secondComponent = spy(Component.class);
    when(secondComponent.getComponentId()).thenReturn("comp2");
    registry.add(component, secondComponent);

    assertFalse(registry.getComponents().isEmpty());

    component.destroy();
    assertFalse(registry.getComponents().contains(component));
    assertTrue(registry.getComponents().contains(secondComponent));

    secondComponent.destroy();
    assertFalse(registry.getComponents().contains(secondComponent));
  }

  @Test
  @DisplayName("getComponents should return an unmodifiable list of components")
  void getComponentsShouldReturnUnmodifiableList() {
    registry.add(component);
    List<Component> components = registry.getComponents();
    assertThrows(UnsupportedOperationException.class, () -> components.add(component));
  }

  @Test
  @DisplayName("getComponent should return the correct component by ID")
  void getComponentShouldReturnCorrectComponent() {
    registry.add(component);
    Component result = registry.getComponent("comp1");
    assertEquals(component, result);
  }

  @Test
  @DisplayName("getComponent should return null for an unknown component ID")
  void getComponentShouldReturnNullForUnknownId() {
    Component result = registry.getComponent("unknown");
    assertNull(result);
  }
}
