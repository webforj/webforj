package com.webforj.matchmedia;

import com.webforj.PendingResult;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.matchmedia.event.MediaQueryChangeEvent;

/**
 * A removable media query listener with an asynchronous browser registration result.
 *
 * @since 26.03
 */
public final class MediaQueryListenerRegistration
    extends ListenerRegistration<MediaQueryChangeEvent> {

  private final MediaQueryList query;
  private final String id;
  private final EventListener<MediaQueryChangeEvent> callback;
  private final PendingResult<Void> ready = new PendingResult<>();
  private boolean removed;

  MediaQueryListenerRegistration(MediaQueryList query, String id,
      EventListener<MediaQueryChangeEvent> listener) {
    super(query.getEventDispatcher(), MediaQueryChangeEvent.class, listener);
    this.query = query;
    this.id = id;
    // Each registration needs its own identity, even when the listener is registered twice.
    callback = event -> {
      if (!removed) {
        listener.onEvent(event);
      }
    };
    query.getEventDispatcher().addListener(MediaQueryChangeEvent.class, callback);
  }

  /**
   * Returns a result that completes when the browser has installed this registration.
   *
   * <p>
   * An initial notification, when requested, can arrive before this result completes. Removing the
   * registration before installation completes fails this result.
   * </p>
   *
   * @return the registration result, including any browser or transport failure
   */
  public PendingResult<Void> whenReady() {
    return ready;
  }

  /**
   * Removes this listener. Repeated calls have no effect.
   */
  @Override
  public void remove() {
    if (removed) {
      return;
    }
    invalidate();
    query.removeListener(id);
  }

  EventListener<MediaQueryChangeEvent> getCallback() {
    return callback;
  }

  void complete() {
    ready.complete(null);
  }

  void fail(Throwable error) {
    ready.completeExceptionally(error);
    remove();
  }

  void invalidate() {
    removed = true;
    query.getEventDispatcher().removeListener(MediaQueryChangeEvent.class, callback);
    if (!ready.isDone()) {
      ready.completeExceptionally(new IllegalStateException("Media query listener was removed"));
    }
  }
}
