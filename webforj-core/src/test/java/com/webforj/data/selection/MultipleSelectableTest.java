package com.webforj.data.selection;

import static org.junit.jupiter.api.Assertions.assertEquals;

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
  void shouldDeselectItem() {
    component.select("item2");
    component.deselect("item2");

    assertEquals(-1, component.getSelectedIndex());
  }

  @Test
  void shouldDeselectKey() {
    component.select("item2");
    component.deselectKey("item2");

    assertEquals(-1, component.getSelectedIndex());
  }

  @Test
  void shouldSelectItems() {
    component.select("item1");
    component.select("item2");

    List<Integer> indices = component.getSelectedIndices();

    assertEquals(2, indices.size());
    assertEquals(0, indices.get(0));
    assertEquals(1, indices.get(1));
  }

  @Test
  void shouldSelectKeys() {
    component.selectKey("item2", "item3");

    List<Integer> indices = component.getSelectedIndices();

    assertEquals(2, indices.size());
    assertEquals(1, indices.get(0));
    assertEquals(2, indices.get(1));
  }

  @Test
  void shouldReturnSelectedItems() {
    component.select("item2", "item3");

    List<String> items = component.getSelectedItems();

    assertEquals(2, items.size());
    assertEquals("item2", items.get(0));
    assertEquals("item3", items.get(1));
  }

  @Test
  void shouldReturnSelectedKeys() {
    component.select("item2");
    component.select("item3");

    List<Object> keys = component.getSelectedKeys();

    assertEquals(2, keys.size());
    assertEquals("item2", keys.get(0));
    assertEquals("item3", keys.get(1));
  }
}
