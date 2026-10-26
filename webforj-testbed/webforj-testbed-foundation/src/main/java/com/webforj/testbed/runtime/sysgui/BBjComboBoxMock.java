package com.webforj.testbed.runtime.sysgui;

import java.lang.reflect.Method;

/**
 * The mock backing the dropdown list control surface.
 *
 * <p>
 * Driven by {@code com.webforj.component.list.DwcSelectDropdown}, which opens and closes the
 * dropdown, clears the selection and updates item texts through the control.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjComboBoxMock extends BBjListBehaviorMock {
  private boolean listOpened = false;

  /**
   * Creates a new dropdown list mock.
   *
   * @param id the control identifier
   */
  BBjComboBoxMock(int id) {
    super(id);
  }

  /**
   * Returns whether the dropdown list is open.
   *
   * @return true when the list is open
   */
  boolean isListOpened() {
    return listOpened;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "openList" -> {
        listOpened = true;
        yield null;
      }
      case "closeList" -> {
        listOpened = false;
        yield null;
      }
      case "deselect" -> {
        getSelectedIndicesState().clear();
        yield null;
      }
      case "setTextAt" -> {
        setItemText((Integer) args[0], (String) args[1]);
        yield null;
      }
      default -> super.handle(proxy, method, args);
    };
  }
}
