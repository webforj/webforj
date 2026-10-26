package com.webforj.testbed.runtime.event;

import com.basis.bbj.proxies.event.BBjEvent;
import com.webforj.testbed.runtime.RuntimeMock;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The store for event callbacks registered under an event type constant.
 *
 * <p>
 * Controls and the radio group register one callback per event type constant. This store keeps
 * those registrations and fires events through them exactly as the production runtime does.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjControlEventCallbackRegistry {
  private final Map<Integer, RuntimeMock.Callback> callbacks = new ConcurrentHashMap<>();

  /**
   * Registers a callback under the given event type constant.
   *
   * @param eventType the event type constant
   * @param handler the object carrying the callback method
   * @param methodName the name of the callback method
   */
  public void register(int eventType, Object handler, String methodName) {
    callbacks.put(eventType, new RuntimeMock.Callback(handler, methodName));
  }

  /**
   * Removes the callback registered under the given event type constant.
   *
   * @param eventType the event type constant
   */
  public void clear(int eventType) {
    callbacks.remove(eventType);
  }

  /**
   * Returns whether a callback is registered under the given event type constant.
   *
   * @param eventType the event type constant
   *
   * @return true when a callback is registered
   */
  public boolean has(int eventType) {
    return callbacks.containsKey(eventType);
  }

  /**
   * Fires an event through the callback registered under the given event type constant.
   *
   * @param eventType the event type constant
   * @param event the event to deliver
   *
   * @return true when a callback was registered and invoked, false otherwise
   */
  public boolean fire(int eventType, BBjEvent event) {
    RuntimeMock.Callback callback = callbacks.get(eventType);
    if (callback == null) {
      return false;
    }

    callback.invoke(event);

    return true;
  }
}
