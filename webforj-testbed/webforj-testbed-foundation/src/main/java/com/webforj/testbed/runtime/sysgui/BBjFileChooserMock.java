package com.webforj.testbed.runtime.sysgui;

import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The mock backing the file chooser control.
 *
 * <p>
 * Driven by {@code com.webforj.component.upload.Upload}, which configures selection mode and file
 * filters through the control and approves or cancels the running selection.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjFileChooserMock extends BBjControlMock {
  private final Map<String, Object> fileFilters = new LinkedHashMap<>();

  /**
   * Creates a new file chooser mock.
   *
   * @param id the control identifier
   */
  BBjFileChooserMock(int id) {
    super(id);
  }

  /**
   * Returns the file filters added through the control, keyed by description.
   *
   * @return the file filters
   */
  Map<String, Object> getFileFilters() {
    return fileFilters;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setMultiSelectionEnabled", "setFileSelectionMode", "setActiveFileFilter",
          "approveSelection", "cancelSelection" -> null;
      case "addFileFilter" -> {
        fileFilters.put((String) args[0], args[1]);
        yield null;
      }
      case "removeFileFilter" -> {
        fileFilters.remove(args[0]);
        yield null;
      }
      default -> super.handle(proxy, method, args);
    };
  }
}
