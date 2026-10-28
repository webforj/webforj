package com.webforj.testbed.runtime.sysgui;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Set;

/**
 * The mock backing the drop down list, whose text is the selected item.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjListButtonMock extends BBjComboBoxMock {

  BBjListButtonMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    if (method.getName().equals("getText")) {
      Set<Integer> selected = getSelectedIndicesState();
      List<String> items = getItems();
      int index = selected.isEmpty() ? -1 : selected.iterator().next();

      return index >= 0 && index < items.size() ? items.get(index) : "";
    }

    return super.handle(proxy, method, args);
  }
}
