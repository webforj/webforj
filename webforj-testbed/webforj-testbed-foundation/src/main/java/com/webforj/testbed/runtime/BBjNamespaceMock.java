package com.webforj.testbed.runtime;

import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The runtime mock backing a namespace of the context under test.
 *
 * <p>
 * The mock keeps the namespace variables in memory. Reading an absent variable throws
 * {@link BBjException}, which matches the behavior the framework expects from the runtime.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjNamespaceMock extends RuntimeMock {
  private final String name;
  private final Map<String, Object> variables = new ConcurrentHashMap<>();

  /**
   * Creates a new namespace mock.
   *
   * @param name the namespace name
   */
  BBjNamespaceMock(String name) {
    this.name = name;
  }

  /**
   * Returns the variables of the namespace.
   *
   * @return the variables
   */
  Map<String, Object> getVariables() {
    return variables;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "getName" -> name;
      case "setValue" -> {
        if (args[1] == null) {
          variables.remove(args[0]);
        } else {
          variables.put((String) args[0], args[1]);
        }
        yield null;
      }
      case "getValue" -> {
        Object value = variables.get(args[0]);
        if (value == null) {
          throw new BBjException("No namespace variable named: " + args[0], 0);
        }
        yield value;
      }
      case "removeValue" -> {
        variables.remove(args[0]);
        yield null;
      }
      case "getKeys" -> new BBjVector(variables.keySet());
      case "cloneMap" -> new HashMap<>(variables);
      case "clear" -> {
        variables.clear();
        yield null;
      }
      default -> UNHANDLED;
    };
  }
}
