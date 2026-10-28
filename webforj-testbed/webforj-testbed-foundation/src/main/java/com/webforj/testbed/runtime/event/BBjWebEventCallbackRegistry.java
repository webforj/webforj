package com.webforj.testbed.runtime.event;

import com.basis.bbj.proxies.event.BBjWebEvent;
import com.webforj.testbed.runtime.RuntimeMock;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * The store for event callbacks registered under an event type name.
 *
 * <p>
 * Both web components and the web manager register callbacks by event type name and receive an
 * identifier per registration. This store keeps those registrations and fires events through them
 * exactly as the production runtime does, one {@link BBjWebEvent} per registered callback carrying
 * the callback identifier and the payload.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjWebEventCallbackRegistry {
  private final Map<String, Map<Integer, RuntimeMock.Callback>> callbacks =
      new ConcurrentHashMap<>();
  private final AtomicInteger ids;

  /**
   * Creates a new callback store.
   *
   * @param ids the identifier sequence shared with the owning mock
   */
  public BBjWebEventCallbackRegistry(AtomicInteger ids) {
    this.ids = ids;
  }

  /**
   * Registers a callback under the given event type name.
   *
   * @param eventType the event type name
   * @param handler the object carrying the callback method
   * @param methodName the name of the callback method
   *
   * @return the callback identifier
   */
  public int register(String eventType, Object handler, String methodName) {
    int callbackId = ids.incrementAndGet();
    callbacks.computeIfAbsent(eventType, key -> new ConcurrentHashMap<>()).put(callbackId,
        new RuntimeMock.Callback(handler, methodName));

    return callbackId;
  }

  /**
   * Removes every callback registered under the given event type name.
   *
   * @param eventType the event type name
   */
  public void clear(String eventType) {
    callbacks.remove(eventType);
  }

  /**
   * Removes the callback with the given identifier from the given event type name.
   *
   * @param eventType the event type name
   * @param callbackId the callback identifier
   */
  public void clear(String eventType, int callbackId) {
    Map<Integer, RuntimeMock.Callback> registered = callbacks.get(eventType);
    if (registered != null) {
      registered.remove(callbackId);
    }
  }

  /**
   * Returns whether at least one callback is registered under the given event type name.
   *
   * @param eventType the event type name
   *
   * @return true when at least one callback is registered
   */
  public boolean has(String eventType) {
    Map<Integer, RuntimeMock.Callback> registered = callbacks.get(eventType);

    return registered != null && !registered.isEmpty();
  }

  /**
   * Fires an event through every callback registered under the given event type name.
   *
   * @param eventType the event type name
   * @param payload the event payload
   *
   * @return the number of callbacks invoked
   */
  public int fire(String eventType, Map<String, Object> payload) {
    Map<Integer, RuntimeMock.Callback> registered = callbacks.get(eventType);
    if (registered == null || registered.isEmpty()) {
      return 0;
    }

    Map<Integer, RuntimeMock.Callback> snapshot = new LinkedHashMap<>(registered);
    for (Map.Entry<Integer, RuntimeMock.Callback> entry : snapshot.entrySet()) {
      Map<String, Object> returnValues = new LinkedHashMap<>();
      returnValues.put("getCallbackID", entry.getKey());
      returnValues.put("getEventMap", payload);
      returnValues.put("getEventType", eventType);
      entry.getValue().invoke(RuntimeMock.createEvent(BBjWebEvent.class, returnValues));
    }

    return snapshot.size();
  }
}
