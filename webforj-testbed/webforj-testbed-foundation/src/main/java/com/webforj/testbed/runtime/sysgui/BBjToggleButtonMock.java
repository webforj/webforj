package com.webforj.testbed.runtime.sysgui;

import java.lang.reflect.Method;

/**
 * The mock backing the toggle button control surface.
 *
 * <p>
 * Driven by {@code com.webforj.component.optioninput.DwcOptionInput}, which writes and reads the
 * selected state and positions the label text through the control.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjToggleButtonMock extends BBjControlMock {
  private boolean selected = false;

  /**
   * Creates a new toggle button mock.
   *
   * @param id the control identifier
   */
  BBjToggleButtonMock(int id) {
    super(id);
  }

  /**
   * Sets the selected state of the toggle button.
   *
   * @param selected the selected state
   */
  public void setSelected(boolean selected) {
    this.selected = selected;
  }

  /**
   * Returns the selected state of the toggle button.
   *
   * @return the selected state
   */
  boolean isSelected() {
    return selected;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setSelected" -> {
        selected = (Boolean) args[0];
        yield null;
      }
      case "isSelected" -> selected;
      case "setHorizontalTextPosition" -> null;
      default -> super.handle(proxy, method, args);
    };
  }
}
