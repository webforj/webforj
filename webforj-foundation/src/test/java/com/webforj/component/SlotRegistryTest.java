package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import com.webforj.component.window.Window;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class SlotRegistryTest {

  private SlotRegistry slotRegistry;
  private Component component1;
  private Component component2;

  @BeforeEach
  void setup() {
    slotRegistry = new SlotRegistry();
    component1 = mock(Component.class);
    component2 = mock(Component.class);
  }

  @ParameterizedTest
  @ValueSource(strings = {"header", "", SlotRegistry.DEFAULT_SLOT})
  void shouldAddComponentsToSlot(String slot) {
    slotRegistry.addComponentsToSlot(slot, component1, component2);
    List<Component> components = slotRegistry.getComponentsInSlot(slot);

    assertEquals(2, components.size());
    assertTrue(components.contains(component1));
    assertTrue(components.contains(component2));
  }

  @Test
  void shouldNotAllowAddingComponentToMultipleSlots() {
    slotRegistry.addComponentsToSlot("header", component1);

    assertThrows(IllegalArgumentException.class, () -> {
      slotRegistry.addComponentsToSlot("footer", component1);
    });
  }

  @ParameterizedTest
  @ValueSource(strings = {"header", "", SlotRegistry.DEFAULT_SLOT})
  void shouldReplaceComponentsToSlot(String slot) {
    slotRegistry.addComponentsToSlot(slot, component1, component2);
    slotRegistry.replaceComponentsInSlot(slot, new TestComponent());

    List<Component> components = slotRegistry.getComponentsInSlot(slot);
    assertEquals(1, components.size());
    assertTrue(components.get(0) instanceof TestComponent);

    verify(component1).destroy();
    verify(component2).destroy();
  }

  @ParameterizedTest
  @ValueSource(strings = {"header", "", SlotRegistry.DEFAULT_SLOT})
  void shouldRemoveComponentsFromSlot(String slot) {
    slotRegistry.addComponentsToSlot(slot, component1, component2);
    slotRegistry.removeComponentsFromSlot(component1);

    List<Component> components = slotRegistry.getComponentsInSlot(slot);
    assertEquals(1, components.size());
    assertFalse(components.contains(component1));
    assertTrue(components.contains(component2));

    verify(component1).destroy();
  }

  @ParameterizedTest
  @ValueSource(strings = {"header", "", SlotRegistry.DEFAULT_SLOT})
  void shouldRemoveDestroyedComponentsFromSlot(String slot) {
    component1 = new TestComponent();
    component2 = new TestComponent();
    slotRegistry.addComponentsToSlot(slot, component1, component2);
    component1.destroy();

    List<Component> components = slotRegistry.getComponentsInSlot(slot);
    assertEquals(1, components.size());
    assertFalse(components.contains(component1));
    assertTrue(components.contains(component2));
  }

  @ParameterizedTest
  @ValueSource(strings = {"header", "", SlotRegistry.DEFAULT_SLOT})
  void shouldRemoveSlot(String slot) {
    slotRegistry.addComponentsToSlot(slot, component1, component2);
    slotRegistry.removeSlot(slot);

    List<Component> components = slotRegistry.getComponentsInSlot(slot);
    assertTrue(components.isEmpty());

    verify(component1).destroy();
    verify(component2).destroy();
  }

  @ParameterizedTest
  @ValueSource(strings = {"header", ""})
  void shouldFindComponentSlot(String slot) {
    slotRegistry.addComponentsToSlot(slot, component1);

    assertEquals(slot, slotRegistry.findComponentSlot(component1));
  }

  @Test
  void shouldReturnNullWhenComponentNotInAnySlot() {
    assertNull(slotRegistry.findComponentSlot(component1));
  }

  @ParameterizedTest
  @ValueSource(strings = {"header", "", SlotRegistry.DEFAULT_SLOT})
  void shouldReturnFirstComponentInSlot(String slot) {
    slotRegistry.addComponentsToSlot(slot, component1, component2);
    Component firstComponent = slotRegistry.getFirstComponentInSlot(slot);

    assertEquals(component1, firstComponent);
  }

  @Test
  void shouldReturnNullWhenNoComponentsInSlot() {
    Component firstComponent = slotRegistry.getFirstComponentInSlot("nonexistent");
    assertNull(firstComponent);
  }

  @ParameterizedTest
  @ValueSource(strings = {"header", "", SlotRegistry.DEFAULT_SLOT})
  void shouldGetComponentsInSlotOfType(String slot) {
    slotRegistry.addComponentsToSlot(slot, component1, component2, new TestComponent());

    List<TestComponent> components = slotRegistry.getComponentsInSlot(slot, TestComponent.class);
    assertEquals(1, components.size());

    List<Component> allComponents = slotRegistry.getComponentsInSlot(slot, Component.class);
    assertEquals(3, allComponents.size());
  }

  @ParameterizedTest
  @ValueSource(strings = {"header", "", SlotRegistry.DEFAULT_SLOT})
  void shouldReturnNullWhenNoComponentOfTypeInSlot(String slot) {
    slotRegistry.addComponentsToSlot(slot, component1);
    Component firstTypedComponent = slotRegistry.getFirstComponentInSlot(slot, TestComponent.class);

    assertNull(firstTypedComponent);
  }

  @ParameterizedTest
  @ValueSource(strings = {"header", "", SlotRegistry.DEFAULT_SLOT})
  void shouldReturnEmptyListWhenSlotIsEmpty(String slot) {
    List<Component> components = slotRegistry.getComponentsInSlot(slot);
    assertTrue(components.isEmpty());
  }

  static class TestComponent extends Component {

    @Override
    protected void onCreate(Window window) {
      // no-op
    }

    @Override
    protected void onDestroy() {
      // no-op
    }
  }
}
