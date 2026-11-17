package com.webforj.matchmedia;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.webforj.PendingResult;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.matchmedia.event.MediaQueryChangeEvent;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

/**
 * A browser media query created by {@link MediaQuery#matchMedia(String)}.
 *
 * <p>
 * Instances own independent listeners, including when their query strings are identical. Dispose a
 * query when it is no longer needed. Access this object from its owning webforJ environment, as
 * with other browser services.
 * </p>
 *
 * @since 26.03
 */
public final class MediaQueryList {

  private final MediaQuery owner;
  private final String id;
  private final EventDispatcher dispatcher = new EventDispatcher();
  private final Map<String, MediaQueryListenerRegistration> registrations = new LinkedHashMap<>();
  private String media;
  private boolean destroyed;

  MediaQueryList(MediaQuery owner, String id) {
    this.owner = owner;
    this.id = id;
  }

  /**
   * Returns the browser's serialized query, which can differ from the original input.
   *
   * @return the serialized query
   */
  public String getMedia() {
    return media;
  }

  /**
   * Reads the match state when this request executes in the browser.
   *
   * @return an asynchronous snapshot of the match state
   * @throws IllegalStateException if this query has been destroyed
   */
  public PendingResult<Boolean> getMatches() {
    requireActive();
    return owner.call(request("matches")).thenApply(value -> {
      if (!value.isJsonPrimitive() || !value.getAsJsonPrimitive().isBoolean()) {
        throw new WebforjRuntimeException(
            "The browser returned an invalid media query match state");
      }
      return value.getAsBoolean();
    });
  }

  /**
   * Adds a listener for future changes, without an initial notification.
   *
   * @param listener the listener to add
   * @return a removable registration with a browser readiness result
   */
  public MediaQueryListenerRegistration addChangeListener(
      EventListener<MediaQueryChangeEvent> listener) {
    return addChangeListener(listener, false);
  }

  /**
   * Adds a listener, optionally reporting the current state before subsequent changes.
   *
   * <p>
   * The initial notification is sent only to this registration and is marked by
   * {@link MediaQueryChangeEvent#isInitial()}. Observation and the initial snapshot are installed
   * in the same browser operation. All listeners on this query share one native change handler; the
   * handler is detached when the last listener is removed.
   * </p>
   *
   * @param listener the listener to add
   * @param notifyImmediately whether to request an asynchronous initial notification
   * @return a removable registration with a browser readiness result
   * @throws IllegalStateException if this query has been destroyed
   */
  public MediaQueryListenerRegistration addChangeListener(
      EventListener<MediaQueryChangeEvent> listener, boolean notifyImmediately) {
    requireActive();
    Objects.requireNonNull(listener, "listener must not be null");
    String listenerId = UUID.randomUUID().toString();
    MediaQueryListenerRegistration registration =
        new MediaQueryListenerRegistration(this, listenerId, listener);
    registrations.put(listenerId, registration);

    JsonObject request = request("subscribe");
    request.addProperty("listener", listenerId);
    request.addProperty("initial", notifyImmediately);
    owner.call(request).thenAccept(value -> registration.complete()).exceptionally(error -> {
      registration.fail(error);
      return null;
    });
    return registration;
  }

  /**
   * Alias for {@link #addChangeListener(EventListener)}.
   *
   * @param listener the listener to add
   * @return the listener registration
   */
  public MediaQueryListenerRegistration onChange(EventListener<MediaQueryChangeEvent> listener) {
    return addChangeListener(listener);
  }

  /**
   * Alias for {@link #addChangeListener(EventListener, boolean)}.
   *
   * @param listener the listener to add
   * @param notifyImmediately whether to request an asynchronous initial notification
   * @return the listener registration
   */
  public MediaQueryListenerRegistration onChange(EventListener<MediaQueryChangeEvent> listener,
      boolean notifyImmediately) {
    return addChangeListener(listener, notifyImmediately);
  }

  /**
   * Removes all listeners and releases the browser query. Repeated calls have no effect.
   * Outstanding requests fail, and subsequent operations cannot restart this query.
   */
  public void destroy() {
    if (destroyed) {
      return;
    }
    dispose();
    owner.release(id);
  }

  void initialize(JsonElement value) {
    requireActive();
    if (!value.isJsonObject()) {
      throw new WebforjRuntimeException("The browser returned an invalid media query");
    }
    media = MediaQueryBridge.stringValue(value.getAsJsonObject(), "media");
  }

  void dispatch(JsonObject payload) {
    if (destroyed) {
      return;
    }
    JsonElement listeners = payload.get("listeners");
    if (listeners == null || !listeners.isJsonArray()) {
      throw new WebforjRuntimeException("The browser returned invalid media query listeners");
    }
    Set<EventListener<MediaQueryChangeEvent>> recipients = new HashSet<>();
    for (JsonElement listener : listeners.getAsJsonArray()) {
      if (listener.isJsonPrimitive() && listener.getAsJsonPrimitive().isString()) {
        MediaQueryListenerRegistration registration = registrations.get(listener.getAsString());
        if (registration != null) {
          recipients.add(registration.getCallback());
        }
      }
    }
    MediaQueryChangeEvent event =
        new MediaQueryChangeEvent(this, MediaQueryBridge.stringValue(payload, "media"),
            MediaQueryBridge.booleanValue(payload, "matches"),
            MediaQueryBridge.booleanValue(payload, "initial"));
    dispatcher.dispatchEvent(event, (listener, notification) -> recipients.contains(listener));
  }

  void removeListener(String listenerId) {
    if (registrations.remove(listenerId) != null && !destroyed) {
      JsonObject request = request("unsubscribe");
      request.addProperty("listener", listenerId);
      owner.send(request);
    }
  }

  void dispose() {
    destroyed = true;
    // Clear ownership before completing results, since user callbacks may reenter this object.
    var listeners = registrations.values().toArray(MediaQueryListenerRegistration[]::new);
    registrations.clear();
    dispatcher.removeAllListeners();
    for (MediaQueryListenerRegistration listener : listeners) {
      listener.invalidate();
    }
  }

  EventDispatcher getEventDispatcher() {
    return dispatcher;
  }

  private JsonObject request(String command) {
    return MediaQueryBridge.request(command, id);
  }

  private void requireActive() {
    if (destroyed) {
      throw new IllegalStateException("Media query has been destroyed");
    }
  }
}
