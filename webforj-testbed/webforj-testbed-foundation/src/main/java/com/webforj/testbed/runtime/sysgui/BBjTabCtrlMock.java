package com.webforj.testbed.runtime.sysgui;

import com.basis.startup.type.BBjException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

/**
 * The mock backing the tab control.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjTabCtrlMock extends BBjControlMock {
  private final List<Tab> tabs = new ArrayList<>();
  private int selectedIndex = 0;

  BBjTabCtrlMock(int id) {
    super(id);
  }

  List<String> getTitles() {
    List<String> titles = new ArrayList<>();
    for (Tab tab : tabs) {
      titles.add(tab.title);
    }

    return titles;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "addTab" -> {
        tabs.add(new Tab((String) args[0]));
        yield null;
      }
      case "insertTab" -> {
        int index = (Integer) args[0];
        tabs.add(Math.clamp(index, 0, tabs.size()), new Tab((String) args[1]));
        yield null;
      }
      case "removeTab" -> {
        tabs.remove(checkIndex((Integer) args[0]));
        clampSelection();
        yield null;
      }
      case "getNumTabs" -> tabs.size();
      case "setTitleAt" -> {
        tabs.get(checkIndex((Integer) args[0])).title = (String) args[1];
        yield null;
      }
      case "getTitleAt" -> tabs.get(checkIndex((Integer) args[0])).title;
      case "setEnabledAt" -> {
        tabs.get(checkIndex((Integer) args[0])).enabled = (Boolean) args[1];
        yield null;
      }
      case "isEnabledAt" -> tabs.get(checkIndex((Integer) args[0])).enabled;
      case "setCloseableAt" -> {
        tabs.get(checkIndex((Integer) args[0])).closeable = (Boolean) args[1];
        yield null;
      }
      case "isCloseableAt" -> tabs.get(checkIndex((Integer) args[0])).closeable;
      case "setSlotAt" -> {
        checkIndex((Integer) args[0]);
        yield null;
      }
      case "setControlAt", "setToolTipTextAt" -> null;
      case "setSelectedIndex" -> {
        selectedIndex = checkIndex((Integer) args[0]);
        yield null;
      }
      case "getSelectedIndex" -> tabs.isEmpty() ? -1 : selectedIndex;
      default -> super.handle(proxy, method, args);
    };
  }

  private int checkIndex(int index) throws BBjException {
    if (index < 0 || index >= tabs.size()) {
      throw new BBjException("Invalid tab index " + index, BBjException.ARG_ERR);
    }

    return index;
  }

  private void clampSelection() {
    if (tabs.isEmpty()) {
      selectedIndex = 0;
    } else {
      selectedIndex = Math.clamp(selectedIndex, 0, tabs.size() - 1);
    }
  }

  private static final class Tab {
    private String title;
    private boolean enabled = true;
    private boolean closeable = false;

    private Tab(String title) {
      this.title = title;
    }
  }
}
