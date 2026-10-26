package com.webforj.testbed.runtime;

import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjVector;
import com.webforj.testbed.runtime.sysgui.BBjTopLevelWindowMock;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * The mock backing the window system under test.
 *
 * <p>
 * Driven by {@code com.webforj.Environment}, {@code com.webforj.component.window.Frame} and
 * {@code com.webforj.App}, which request window contexts, create top level windows and enumerate
 * the open windows of the application. Every created window is tracked so that enumeration works.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjSysGuiMock extends RuntimeMock {
  private final AtomicInteger contexts = new AtomicInteger(0);
  private final AtomicInteger windowIds = new AtomicInteger(0);
  private final List<BBjTopLevelWindowMock> windows = new ArrayList<>();
  private final ScriptAnswerRegistry scriptAnswers;

  /**
   * Creates a new system gui mock.
   *
   * @param scriptAnswers the answers the client gives to scripts
   */
  BBjSysGuiMock(ScriptAnswerRegistry scriptAnswers) {
    this.scriptAnswers = scriptAnswers;
  }

  /**
   * Returns the window mocks created through this mock, in creation order.
   *
   * @return the window mocks
   */
  List<BBjTopLevelWindowMock> getWindowMocks() {
    return List.copyOf(windows);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    String name = method.getName();

    if (name.equals("getAvailableContext")) {
      return contexts.incrementAndGet();
    }

    if (name.equals("addWindow") || name.equals("createTopLevelWindow")) {
      BBjTopLevelWindowMock window =
          new BBjTopLevelWindowMock(windowIds.incrementAndGet(), scriptAnswers);
      windows.add(window);

      return window.getProxy(BBjTopLevelWindow.class);
    }

    if (name.equals("getWindows")) {
      BBjVector vector = new BBjVector();
      for (BBjTopLevelWindowMock window : windows) {
        vector.addItem(window.getProxy(BBjTopLevelWindow.class));
      }

      return vector;
    }

    if (name.equals("getWindow") && args.length == 1 && args[0] instanceof Integer windowId) {
      for (BBjTopLevelWindowMock window : windows) {
        if (window.getId() == windowId) {
          return window.getProxy(BBjWindow.class);
        }
      }

      return null;
    }

    return UNHANDLED;
  }
}
