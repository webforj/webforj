package com.webforj.testbed.runtime.sysgui;

import com.basis.startup.type.BBjVector;
import java.lang.reflect.Method;
import java.util.List;

/**
 * The mock backing the list box control.
 *
 * <p>
 * Driven by {@code com.webforj.component.list.ListBox}, which switches between single and multiple
 * selection, deselects indices and writes and reads the selected indices as a vector of integers,
 * expecting an empty vector when nothing is selected.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjListBoxMock extends BBjListBehaviorMock {

  /**
   * Creates a new list box mock.
   *
   * @param id the control identifier
   */
  BBjListBoxMock(int id) {
    super(id);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setMultipleSelection" -> {
        setMultipleSelection((Boolean) args[0]);
        yield null;
      }
      case "deselectAll" -> {
        getSelectedIndicesState().clear();
        yield null;
      }
      case "deselectIndex" -> {
        getSelectedIndicesState().remove(args[0]);
        yield null;
      }
      case "setSelectedIndices" -> {
        getSelectedIndicesState().clear();
        for (Object index : (List<?>) args[0]) {
          getSelectedIndicesState().add((Integer) index);
        }
        yield null;
      }
      case "getSelectedIndices" -> new BBjVector(getSelectedIndicesState());
      default -> super.handle(proxy, method, args);
    };
  }
}
