package com.webforj.testbed.runtime.event;

import com.basis.bbj.proxies.event.BBjWebEventOptions;
import com.webforj.testbed.runtime.RuntimeMock;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The runtime mock backing event options created through {@code newEventOptions()}.
 *
 * <p>
 * The mock stores the code, filter, data items, debounce and throttle configuration written by the
 * framework and returns them unchanged. All configuration methods return the options proxy itself.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjWebEventOptionsMock extends RuntimeMock {
  private final Map<String, String> items = new LinkedHashMap<>();

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "addItem" -> {
        items.put((String) args[0], (String) args[1]);
        yield proxy;
      }
      case "getItemMap" -> items;
      case "setCode", "setFilter" -> {
        setValue(method.getName().equals("setCode") ? "code" : "filter", args[0]);
        yield proxy;
      }
      case "setDebounce" -> {
        setValue("debounce", true);
        setValue("delay", args[0]);
        if (args.length == 3) {
          setValue("leading", args[1]);
          setValue("trailing", args[2]);
        }
        yield proxy;
      }
      case "setThrottle" -> {
        setValue("throttle", true);
        setValue("delay", args[0]);
        yield proxy;
      }
      case "setImmediate" -> {
        setValue("immediate", true);
        yield proxy;
      }
      default -> UNHANDLED;
    };
  }

  /**
   * Returns the options proxy backed by this mock.
   *
   * @return the options proxy
   */
  public BBjWebEventOptions getOptions() {
    return getProxy(BBjWebEventOptions.class);
  }
}
