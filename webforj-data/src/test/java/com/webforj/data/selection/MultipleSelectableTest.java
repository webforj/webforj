package com.webforj.data.selection;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class MultipleSelectableTest {

  MultipleSelectableComponentMock component;

  @BeforeEach
  void setUp() {
    component = new MultipleSelectableComponentMock();
  }

  @Test
  void shouldSelectItems() {
    component.select("item1", "item2");
    assertEquals(List.of("item1", "item2"), component.getSelectedItems());
  }

  @Test
  void shouldSelectKeys() {
    component.selectKey("item2", "item3");
    assertEquals(List.of("item2", "item3"), component.getSelectedKeys());
  }

  @Test
  void shouldDeselectItem() {
    component.select("item2");
    component.deselect("item2");
    assertTrue(component.getSelectedItems().isEmpty());
  }

  @Test
  void shouldDeselectKey() {
    component.select("item2");
    component.deselectKey("item2");
    assertTrue(component.getSelectedKeys().isEmpty());
  }

  @Test
  void shouldReturnSelectedItems() {
    component.select("item2", "item3");
    assertEquals(List.of("item2", "item3"), component.getSelectedItems());
  }

  @Test
  void shouldReturnSelectedKeys() {
    component.select("item2", "item3");
    assertEquals(List.of("item2", "item3"), component.getSelectedKeys());
  }
}
