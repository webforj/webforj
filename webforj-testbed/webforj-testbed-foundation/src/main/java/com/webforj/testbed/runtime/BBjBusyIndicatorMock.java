package com.webforj.testbed.runtime;

import java.lang.reflect.Method;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The mock backing the busy indicator of the context under test.
 *
 * <p>
 * Driven by {@code com.webforj.BusyIndicator}, which stores its whole configuration in attributes
 * and reads it back, and carries the busy message as text or html. All of it round trips through
 * memory here.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjBusyIndicatorMock extends RuntimeMock {
  private final Map<String, String> attributes = new ConcurrentHashMap<>();

  /**
   * Returns the attributes written through the busy indicator.
   *
   * @return the attribute map
   */
  public Map<String, String> getAttributes() {
    return attributes;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "setAttribute" -> {
        attributes.put((String) args[0], (String) args[1]);
        yield null;
      }
      case "getAttribute" -> attributes.get(args[0]);
      default -> UNHANDLED;
    };
  }
}
