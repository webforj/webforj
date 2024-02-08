package org.dwcj.data.selection;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class SelectableTest {

  private SelectableComponentMock component;

  @BeforeEach
  void setUp() {
    component = new SelectableComponentMock();
  }

  @Test
  void shouldDeselectItem() {
    component.selectIndex(1);
    component.deselect();
    assertEquals(-1, component.getSelectedIndex());
  }

  @Test
  void shouldSelectItem() {
    component.select("item2");
    assertEquals(1, component.getSelectedIndex());
  }

  @Test
  void shouldSelectKey() {
    component.selectKey("item2");
    assertEquals(1, component.getSelectedIndex());
  }

  @Test
  void shouldSelectItemByIndex() {
    component.selectIndex(1);
    assertEquals(1, component.getSelectedIndex());
  }

  @Test
  void shouldReturnSelectedItem() {
    component.selectIndex(1);
    assertEquals("item2", component.getSelected());
  }

  @Test
  void shouldReturnSelectedKey() {
    component.selectIndex(1);
    assertEquals("item2", component.getSelectedKey());
  }
}
