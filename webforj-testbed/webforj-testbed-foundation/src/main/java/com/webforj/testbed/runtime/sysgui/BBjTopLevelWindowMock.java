package com.webforj.testbed.runtime.sysgui;

import com.webforj.testbed.runtime.ScriptAnswerRegistry;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * The mock backing a top level window under test.
 *
 * <p>
 * Driven by {@code com.webforj.component.window.Frame}, which styles three window areas. The center
 * area styles and classes go through the base control surface, while the outer and content areas
 * have their own style and class state here, so styles written to one area are read back from that
 * area only.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjTopLevelWindowMock extends BBjWindowMock {
  private final Map<String, String> outerStyles = new ConcurrentHashMap<>();
  private final Map<String, String> panelStyles = new ConcurrentHashMap<>();

  /**
   * Creates a new top level window mock.
   *
   * @param id the window identifier
   */
  public BBjTopLevelWindowMock(int id) {
    super(id);
  }

  /**
   * Creates a new top level window mock.
   *
   * @param id the window identifier
   * @param scriptAnswers the answers the client gives to scripts
   */
  public BBjTopLevelWindowMock(int id, ScriptAnswerRegistry scriptAnswers) {
    super(id, new AtomicInteger(id), scriptAnswers);
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setOuterStyle" -> {
        putStyle(outerStyles, (String) args[0], (String) args[1]);
        yield null;
      }
      case "getOuterStyle" -> outerStyles.get(args[0]);
      case "setPanelStyle" -> {
        putStyle(panelStyles, (String) args[0], (String) args[1]);
        yield null;
      }
      case "getPanelStyle", "getComputedPanelStyle" -> panelStyles.get(args[0]);
      case "addOuterClass", "removeOuterClass", "addPanelClass", "removePanelClass" -> null;
      default -> super.handle(proxy, method, args);
    };
  }

  private static void putStyle(Map<String, String> styles, String property, String value) {
    if (value == null) {
      styles.remove(property);
    } else {
      styles.put(property, value);
    }
  }
}
