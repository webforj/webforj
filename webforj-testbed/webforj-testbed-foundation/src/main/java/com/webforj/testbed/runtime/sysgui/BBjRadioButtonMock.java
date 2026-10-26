package com.webforj.testbed.runtime.sysgui;

import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import java.lang.reflect.Method;
import java.util.Optional;



/**
 * The mock backing the radio button control.
 *
 * <p>
 * Driven by {@code com.webforj.component.optioninput.RadioButton}, which only uses the toggle
 * button surface. Group membership is managed by the radio group mock.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjRadioButtonMock extends BBjToggleButtonMock {

  /**
   * Creates a new radio button mock.
   *
   * @param id the control identifier
   */
  BBjRadioButtonMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    Object result = super.handle(proxy, method, args);
    // Selecting a radio button moves the selection of the group it belongs to, and unchecking the
    // selected one leaves the group without a selection.
    if (method.getName().equals("setSelected")) {
      Optional<BBjRadioGroupMock> group = BBjWindowMock.groupContaining(this);
      if (group.isPresent() && Boolean.TRUE.equals(args[0])) {
        group.get().moveSelectionTo((BBjRadioButton) proxy);
      } else if (group.isPresent()) {
        group.get().deselect((BBjRadioButton) proxy);
      }
    }

    return result;
  }
}
