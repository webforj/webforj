package com.webforj.data.selection;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class SelectableTest {

  private SelectableComponentMock component;

  @BeforeEach
  void setUp() {
    component = new SelectableComponentMock();
  }

  @Test
  void shouldSelectItem() {
    component.select("item2");
    assertEquals("item2", component.getSelected());
  }

  @Test
  void shouldSelectKey() {
    component.selectKey("item2");
    assertEquals("item2", component.getSelectedKey());
  }

  @Test
  void shouldDeselectItem() {
    component.select("item2");
    component.deselect();
    assertNull(component.getSelected());
  }

  @Test
  void shouldReturnSelectedItem() {
    component.select("item2");
    assertEquals("item2", component.getSelected());
  }

  @Test
  void shouldReturnSelectedKey() {
    component.select("item2");
    assertEquals("item2", component.getSelectedKey());
  }
}
