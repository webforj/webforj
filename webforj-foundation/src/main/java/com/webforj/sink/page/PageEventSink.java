package com.webforj.sink.page;


import com.basis.bbj.proxies.BBjWebManager;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjWebEvent;
import com.basis.bbj.proxies.event.BBjWebEventOptions;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.CustomObject;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.component.element.event.DebouncePhase;
import com.webforj.component.event.sink.DwcEventSink;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.event.page.PageEvent;
import com.webforj.event.page.PageEventOptions;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

/**
 * The {@code PageEventSink} implements the required logic for setting and removing the callback on
 * the {@code BBjWebManager}. It will delegates the BBj event to the corresponding event listener to
 * the Java component.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class PageEventSink implements DwcEventSink<Page> {
  private final Page page;
  private EventDispatcher dispatcher;
  private final String type;
  private Map<Integer, PageEventOptions> callbackOptionsMap = new HashMap<>();

  /**
   * Constructs a new instance of {@link PageEventSink}.
   *
   * @param page The associated Page component.
   * @param type The event type.
   * @param dispatcher The event dispatcher.
   */
  public PageEventSink(Page page, String type, EventDispatcher dispatcher) {
    this.page = page;
    this.dispatcher = dispatcher;
    this.type = type;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String setCallback(Object options) {
    if (isConnected()) {
      try {
        WebforjBBjBridge bridge = getEnvironment().getBridge();
        CustomObject handler = bridge.getEventProxy(this, "handleEvent");
        return doSetCallback(getBbjWebManager(), options, handler, "onEvent");
      } catch (BBjException e) {
        throw new WebforjRuntimeException("Failed to set BBjWebManager callback.", e);
      }
    }

    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeCallback(String id) {
    if (isConnected()) {
      try {
        doRemoveCallback(getBbjWebManager(), id);
      } catch (BBjException e) {
        throw new WebforjRuntimeException("Failed to remove BBjWebManager callback.", e);
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final EventDispatcher getEventDispatcher() {
    return this.dispatcher;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final boolean isConnected() {
    return this.page != null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isMultipleCallbacks() {
    return true;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Page getComponent() {
    return this.page;
  }

  /**
   * Handles the BBj event and dispatches a new {@link PageEvent}.
   *
   * @param bbjEvent The BBj event to handle.
   */
  public void handleEvent(BBjEvent bbjEvent) {
    BBjWebEvent bbjWebEvent = (BBjWebEvent) bbjEvent;
    PageEvent event;
    try {
      int id = bbjWebEvent.getCallbackID();
      PageEventOptions options = callbackOptionsMap.get(id);

      event = new PageEvent(getComponent(), bbjWebEvent.getEventMap(), type, id, options);
      getEventDispatcher().dispatchEvent(event);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to dispatch PageEvent '" + type + "'", e);
    }
  }

  /**
   * Do set a callback on the underlying BBj web manager.
   *
   * @param manager The BBjWebManager instance
   * @param options The options object
   * @param handler The BBj CustomObject instance
   * @param callback The callback method name as defined in the handler
   *
   * @return the callback id.
   * @throws BBjException if the callback cannot be set.
   */
  protected String doSetCallback(BBjWebManager manager, Object options, CustomObject handler,
      String callback) throws BBjException {

    // create a control options and map it from the passed PageEventOptions
    BBjWebEventOptions managerOptions = manager.newEventOptions();
    PageEventOptions pageOptions = (PageEventOptions) options;

    // Set the code and filter
    managerOptions.setCode(pageOptions.getCode());
    managerOptions.setFilter(pageOptions.getFilter());

    // Add the items
    for (Entry<String, String> entry : pageOptions.getDataMap().entrySet()) {
      managerOptions.addItem(entry.getKey(), entry.getValue());
    }

    // Set the debounce/throttle options
    if (pageOptions.isDebounce()) {
      DebouncePhase phase = pageOptions.getDebouncePhase();
      boolean leading = phase.equals(DebouncePhase.LEADING) || phase.equals(DebouncePhase.BOTH);
      boolean trailing = phase.equals(DebouncePhase.TRAILING) || phase.equals(DebouncePhase.BOTH);

      managerOptions.setDebounce(pageOptions.getDebounceTimeout(), leading, trailing);
    } else if (pageOptions.isThrottle()) {
      managerOptions.setThrottle(pageOptions.getThrottleTimeout());
    } else {
      managerOptions.setImmediate();
    }

    int id = manager.setCallback(type, handler, callback, managerOptions);
    callbackOptionsMap.put(id, pageOptions);

    return String.valueOf(id);
  }

  /**
   * Do remove a callback from underlying BBj web manager.
   *
   * @param manager The BBjWebManager instance
   * @param callbackId The callback id.
   *
   * @throws BBjException if the callback cannot be removed.
   */
  protected void doRemoveCallback(BBjWebManager manager, String callbackId) throws BBjException {
    int theId = Integer.parseInt(callbackId);
    callbackOptionsMap.remove(theId);

    // Remove the callback only if there are no more callbacks registered
    manager.clearCallback(type, theId);
  }

  /**
   * Gets the BBjWebManager instance.
   *
   * @return the BBjWebManager instance.
   */
  protected BBjWebManager getBbjWebManager() {
    try {
      return getEnvironment().getBBjAPI().getWebManager();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get BBjWebManager instance.", e);
    }
  }

  Environment getEnvironment() {
    return Environment.getCurrent();
  }
}

