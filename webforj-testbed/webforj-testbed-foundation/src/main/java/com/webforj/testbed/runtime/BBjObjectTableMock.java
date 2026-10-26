package com.webforj.testbed.runtime;

import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The runtime mock backing the object table of the context under test.
 *
 * <p>
 * The mock keeps the entries in memory. Reading an absent key throws {@link BBjException}, which
 * matches the behavior the framework expects from the runtime.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjObjectTableMock extends RuntimeMock {
  private final Map<Object, Object> entries = new ConcurrentHashMap<>();

  /**
   * Returns the entries of the object table.
   *
   * @return the entries
   */
  Map<Object, Object> getEntries() {
    return entries;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "put" -> {
        if (args[1] == null) {
          entries.remove(args[0]);
        } else {
          entries.put(args[0], args[1]);
        }
        yield null;
      }
      case "get" -> {
        Object value = entries.get(args[0]);
        if (value == null) {
          throw new BBjException("No object table entry for key: " + args[0], 0);
        }
        yield value;
      }
      case "remove" -> {
        entries.remove(args[0]);
        yield null;
      }
      case "clear" -> {
        entries.clear();
        yield null;
      }
      case "size" -> entries.size();
      case "getKeys" -> new BBjVector(entries.keySet());
      default -> UNHANDLED;
    };
  }
}
