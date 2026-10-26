package com.webforj.testbed.runtime.sysgui;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.basis.bbj.proxies.sysgui.BBjListBox;
import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import java.util.List;
import org.junit.jupiter.api.Test;

class BBjListBehaviorMockTest {
  private static final String THIRD_ITEM = "three";

  @Test
  void shouldKeepItemState() throws BBjException {
    BBjListButton list = new BBjListButtonMock(1).getProxy(BBjListButton.class);

    list.addItem("one");
    list.insertItemAt(1, THIRD_ITEM);
    list.insertItems(1, new BBjVector(List.of("two")));

    assertEquals(3, list.getItemCount());
    assertEquals("two", list.getItemAt(1));
    assertEquals(2, list.indexOf(THIRD_ITEM));

    list.removeItemAt(0);

    assertEquals(2, list.getItemCount());

    list.removeAllItems();

    assertEquals(0, list.getItemCount());
  }

  @Test
  void shouldKeepSingleSelectionState() throws BBjException {
    BBjListButtonMock mock = new BBjListButtonMock(1);
    BBjListButton list = mock.getProxy(BBjListButton.class);

    list.addItem("one");
    list.addItem("two");

    assertEquals(-1, list.getSelectedIndex());
    assertEquals("", list.getSelectedItem());

    list.selectIndex(1);

    assertEquals(1, list.getSelectedIndex());
    assertEquals("two", list.getSelectedItem());
    assertEquals(List.of("one", "two"), mock.getItems());
  }

  @Test
  void shouldOpenCloseAndDeselectDropdowns() throws BBjException {
    BBjListButtonMock mock = new BBjListButtonMock(1);
    BBjListButton list = mock.getProxy(BBjListButton.class);

    list.openList();

    assertTrue(mock.isListOpened());

    list.closeList();

    assertFalse(mock.isListOpened());

    list.addItem("one");
    list.selectIndex(0);
    list.deselect();

    assertEquals(-1, list.getSelectedIndex());
  }

  @Test
  void shouldKeepMultipleSelectionState() throws BBjException {
    BBjListBox list = new BBjListBoxMock(1).getProxy(BBjListBox.class);

    list.addItem("one");
    list.addItem("two");
    list.addItem(THIRD_ITEM);
    list.setMultipleSelection(true);

    list.setSelectedIndices(new BBjVector(List.of(0, 2)));

    assertEquals(0, list.getSelectedIndex());
    assertEquals(List.of(0, 2), list.getSelectedIndices());

    list.deselectIndex(0);

    assertEquals(List.of(2), list.getSelectedIndices());

    list.deselectAll();

    assertEquals(-1, list.getSelectedIndex());
    assertTrue(list.getSelectedIndices().isEmpty());
  }

  @Test
  void shouldGuardIndexesTheWayTheRuntimeDoes() throws BBjException {
    BBjListBox list = new BBjListBoxMock(1).getProxy(BBjListBox.class);
    list.addItem("one");
    list.addItem("two");

    assertThrows(BBjException.class, () -> list.insertItemAt(-1, "zero"));
    assertThrows(BBjException.class, () -> list.getItemAt(2));
    assertThrows(BBjException.class, () -> list.removeItemAt(2));

    list.insertItemAt(9, THIRD_ITEM);

    assertEquals(THIRD_ITEM, list.getItemAt(2));

    list.selectIndex(1);
    list.selectIndex(9);

    assertEquals(1, list.getSelectedIndex());

    list.selectIndex(0);

    assertEquals(List.of(0), list.getSelectedIndices());

    list.setMultipleSelection(true);
    list.selectIndex(2);

    assertEquals(List.of(0, 2), list.getSelectedIndices());

    list.removeItemAt(1);

    assertEquals(List.of(0, 1), list.getSelectedIndices());

    list.selectIndex(-1);

    assertEquals(-1, list.getSelectedIndex());
  }
}
