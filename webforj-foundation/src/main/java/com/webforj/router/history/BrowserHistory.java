package com.webforj.router.history;

import com.google.gson.Gson;
import com.webforj.Page;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.event.page.PageEventOptions;
import com.webforj.router.history.event.HistoryStateChangeEvent;
import java.util.Base64;
import java.util.Optional;

/**
 * Represents <code>window.history</code> in the browser. See e.g.
 * <a href="https://developer.mozilla.org/en-US/docs/Web/API/History_API"> documentation on MDN</a>
 * for detailed information on how the API works in the browser.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class BrowserHistory implements History {
  private Gson gson = new Gson();
  private EventDispatcher dispatcher = new EventDispatcher();
  private boolean isPopStateListenerRegistered = false;

  /**
   * {@inheritDoc}
   */
  @Override
  public History back() {
    Page.getCurrent().executeJsVoidAsync("window.history.back()");
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public History forward() {
    Page.getCurrent().executeJsVoidAsync("window.history.forward()");
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public History go(int index) {
    Page.getCurrent().executeJsVoidAsync("window.history.go(" + index + ")");
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int size() {
    Object result = Page.getCurrent().executeJs("window.history.length");
    if (result instanceof Integer) {
      return (int) result;
    }

    return 0;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> getLocation() {
    Object result =
        Page.getCurrent().executeJs("window.location.href.replace(window.location.origin, '')");
    if (result instanceof String) {
      return Optional.of(new Location((String) result));
    }

    return Optional.empty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public History pushState(Object state, Location location) {
    String path = location.getFullURI();
    String stringifiedState = gson.toJson(state);
    String encodedState = Base64.getEncoder().encodeToString(stringifiedState.getBytes());

    Page.getCurrent().executeJsVoidAsync(
        "window.history.pushState(JSON.parse(atob('" + encodedState + "')), '', '" + path + "')");

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public History replaceState(Object state, Location location) {
    String path = location.getFullURI();
    String stringifiedState = gson.toJson(state);
    String encodedState = Base64.getEncoder().encodeToString(stringifiedState.getBytes());

    Page.getCurrent().executeJsVoidAsync("window.history.replaceState(JSON.parse(atob('"
        + encodedState + "')), '', '" + path + "')");

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ListenerRegistration<HistoryStateChangeEvent> addHistoryStateChangeListener(
      EventListener<HistoryStateChangeEvent> listener) {
    if (!isPopStateListenerRegistered) {
      registerPopStateListener();
      isPopStateListenerRegistered = true;
    }

    return dispatcher.addListener(HistoryStateChangeEvent.class, listener);
  }

  private void registerPopStateListener() {
    PageEventOptions options = new PageEventOptions();
    options.addData("state", "btoa(JSON.stringify(event.state))");
    options.addData("path", "window.location.href.replace(window.location.origin, '')");

    Page.getCurrent().addEventListener("popstate", ev -> {
      String state = (String) ev.getData().get("state");
      String path = (String) ev.getData().get("path");

      Object decodedState = null;
      if (state != null) {
        decodedState = gson.fromJson(new String(Base64.getDecoder().decode(state)), Object.class);
      }

      HistoryStateChangeEvent event =
          new HistoryStateChangeEvent(this, new Location(path), decodedState);

      dispatcher.dispatchEvent(event);
    }, options);
  }
}
