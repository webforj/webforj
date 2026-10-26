package com.webforj.testbed.runtime.sysgui;

import java.lang.reflect.Method;
import java.util.List;

/**
 * The mock backing the editable list, whose text selects the item it matches.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjListEditMock extends BBjComboBoxMock {

  BBjListEditMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    if (method.getName().equals("setText")) {
      // The runtime drops the selection, then selects the first item equal to the text ignoring
      // case, then shows the text.
      String text = args[0] == null ? "" : (String) args[0];
      getSelectedIndicesState().clear();
      List<String> items = getItems();
      for (int index = 0; index < items.size(); index++) {
        if (items.get(index).equalsIgnoreCase(text)) {
          selectIndex(index);
          break;
        }
      }
      putState("text", text);

      return null;
    }

    return super.handle(proxy, method, args);
  }
}
