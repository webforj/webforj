package com.webforj.component.element.event;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Represents event options for an HTML component.
 *
 * <p>
 * This class provides a way to configure and customize event options for an HTML elements. Event
 * options can include items, code to be evaluated on the client-side before an event is fired, and
 * a filter expression to determine whether an event should be fired or not.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class ElementEventOptions {

  private final Map<String, String> data;
  private String code;
  private String filter;
  private int debounceTimeout = 0;
  private DebouncePhase debouncePhase = DebouncePhase.TRAILING;
  private int throttleTimeout = 0;
  private boolean isDebounce = false;
  private boolean isThrottle = false;

  /**
   * Creates a new instance of the event options.
   *
   * @param data The event data.
   * @param code The event code.
   * @param filter The event filter.
   */
  public ElementEventOptions(Map<String, String> data, String code, String filter) {
    this.data = data == null ? new HashMap<>() : data;
    setCode(code);
    setFilter(filter);
  }

  /**
   * Creates a new instance of the event options.
   *
   * @param items The event items.
   * @param code The event code.
   */
  public ElementEventOptions(Map<String, String> items, String code) {
    this(items, code, null);
  }

  /**
   * Creates a new instance of the event options.
   *
   * @param items The event items.
   */
  public ElementEventOptions(Map<String, String> items) {
    this(items, null, null);
  }

  /**
   * Creates a new instance of the event options with default values.
   */
  public ElementEventOptions() {
    this(null, null, null);
  }

  /**
   * Adds an data item to the event options.
   *
   * <p>
   * The event options are used to pass data from the client to the server when an event is fired.
   * The item name is used as the key in the map, and the expression is evaluated in the client to
   * determine the value of the item.
   * </p>
   *
   * @param name The name of the item to be created in the event map.
   * @param expression The JavaScript expression to evaluate in the client to return the value of
   *        the item.
   *
   * @return The options instance.
   */
  public ElementEventOptions addData(String name, String expression) {
    data.put(name, expression);
    return this;
  }

  /**
   * Retrieves a data item from the event options.
   *
   * @param name The name of the item to retrieve.
   * @return The item value.
   */
  public String getData(String name) {
    return data.get(name);
  }

  /**
   * Retrieves the map of event items.
   *
   * @return Unmodifiable map of event items.
   */
  public Map<String, String> getDataMap() {
    return Collections.unmodifiableMap(data);
  }

  /**
   * Sets a JavaScript expression to be evaluated in the client before the event is fired.
   *
   * <p>
   * The expression is evaluated in the client, allowing the client to prepare the event data before
   * the event is fired or to fire additional events if necessary.
   * </p>
   *
   * @param expression The JavaScript expression to be evaluated in the client before the event is
   *        fired.
   *
   * @return The options instance.
   */
  public ElementEventOptions setCode(String expression) {
    this.code = expression != null ? expression.trim() : "";
    return this;
  }

  /**
   * Retrieves the event code.
   *
   * @return The event code.
   */
  public String getCode() {
    return code;
  }


  /**
   * Set a filter expression to be evaluated in the client before the event is fired.
   *
   * <p>
   * The filter expression is evaluated in the client and allows the client to determine if the
   * event should be fired or not.
   * </p>
   *
   * @param expression The event filter expression to set.
   * @return The options instance.
   */
  public ElementEventOptions setFilter(String expression) {
    this.filter = expression != null ? expression.trim() : "";
    return this;
  }

  /**
   * Retrieves the event filter expression.
   *
   * @return The event filter expression.
   */
  public String getFilter() {
    return filter;
  }

  /**
   * Sets the debounce configuration for the event.
   *
   * <p>
   * Debounce is a mechanism to control the frequency of event listeners. It ensures that the event
   * listener is triggered only after a certain timeout has passed since the last event occurrence.
   * This can be particularly helpful for handling user input, such as search input fields, where
   * you want to delay execution until the user has finished typing.
   * </p>
   * </p>
   *
   * @param timeout The timeout in milliseconds, which determines the delay after the last event
   *        occurrence before the listener is invoked.
   * @param phase The phase of the event to debounce, which can be either
   *        {@link DebouncePhase#LEADING} or {@link DebouncePhase#TRAILING}.
   *
   * @return The options instance.
   */
  public ElementEventOptions setDebounce(int timeout, DebouncePhase phase) {
    if (timeout < 0) {
      throw new IllegalArgumentException("Debounce timeout must be a positive integer");
    }

    this.debounceTimeout = timeout;
    this.debouncePhase = phase;

    this.isDebounce = true;
    this.isThrottle = false;
    this.throttleTimeout = 0;

    return this;
  }

  /**
   * Sets the debounce configuration for the event with a default phase of
   * {@link DebouncePhase#TRAILING}.
   *
   * <p>
   * Debounce is a mechanism to control the frequency of event listeners. It ensures that the event
   * listener is triggered only after a certain timeout has passed since the last event occurrence.
   * This can be particularly helpful for handling user input, such as search input fields, where
   * you want to delay execution until the user has finished typing.
   * </p>
   *
   * @see #setDebounce(int, DebouncePhase)
   *
   * @param timeout The timeout in milliseconds, which determines the delay after the last event
   *        occurrence before the listener is invoked.
   *
   * @return The options instance.
   */
  public ElementEventOptions setDebounce(int timeout) {
    return setDebounce(timeout, DebouncePhase.TRAILING);
  }

  /**
   * Retrieves the debounce timeout.
   *
   * @return The debounce timeout in milliseconds.
   */
  public int getDebounceTimeout() {
    return debounceTimeout;
  }

  /**
   * Retrieves the debounce phase.
   *
   * @return The debounce phase, which can be either {@link DebouncePhase#LEADING},
   *         {@link DebouncePhase#TRAILING} or {@link DebouncePhase#BOTH}.
   */
  public DebouncePhase getDebouncePhase() {
    return debouncePhase;
  }

  /**
   * Retrieves whether the event is debounced.
   *
   * @return True if the event is debounced, false otherwise.
   */
  public boolean isDebounce() {
    return isDebounce;
  }

  /**
   * Sets the throttle configuration for the event.
   *
   * <p>
   * Throttle is a mechanism to limit the frequency of event listeners. It ensures that the event
   * listener is triggered at most once every specified timeout period. Throttling is often applied
   * to events that can occur rapidly, such as scrolling or resizing, to reduce the number of
   * computations performed and improve performance.
   * </p>
   *
   * @param timeout The timeout in milliseconds, which determines the minimum time interval between
   *        successive event listeners.
   *
   * @return The options instance.
   */
  public ElementEventOptions setThrottle(int timeout) {
    if (timeout < 0) {
      throw new IllegalArgumentException("Throttle timeout must be a positive integer");
    }

    this.throttleTimeout = timeout;
    this.isDebounce = false;
    this.isThrottle = true;
    this.debounceTimeout = 0;

    return this;
  }

  /**
   * Retrieves the throttle timeout.
   *
   * @return The throttle timeout in milliseconds.
   */
  public int getThrottleTimeout() {
    return throttleTimeout;
  }

  /**
   * Retrieves whether the event is throttled.
   *
   * @return True if the event is throttled, false otherwise.
   */
  public boolean isThrottle() {
    return isThrottle;
  }

  /**
   * Sets the event to be immediate, i.e. not debounced or throttled.
   */
  public void setImmediate() {
    this.isDebounce = false;
    this.isThrottle = false;
    this.debounceTimeout = 0;
    this.throttleTimeout = 0;
  }

  /**
   * Retrieves whether the event is immediate, i.e. not debounced or throttled.
   *
   * @return True if the event is immediate, false otherwise.
   */
  public boolean isImmediate() {
    return !isDebounce && !isThrottle;
  }

  /**
   * Merges the current ElementEventOptions instance with other passed instances. This method
   * updates the current instance's properties with those from the passed instances. For each
   * setting (items, code, and filter), the last non-empty value encountered will take precedence.
   *
   * <p>
   * If any of the objects in the argument list is null, it is safely ignored during the merging
   * process.
   * </p>
   *
   * @param options Varargs parameter that accepts multiple ElementEventOptions objects. It can be
   *        passed as separate arguments or as an array of ElementEventOptions.
   * @return The current ElementEventOptions instance after merging with the passed instances.
   */
  public ElementEventOptions mergeWith(ElementEventOptions... options) {
    if (options == null) {
      return this;
    }

    for (ElementEventOptions option : options) {
      if (option != null) {
        // Merge items
        this.data.putAll(option.getDataMap());

        // Override code and filter if present in the currentOption
        if (!option.getCode().isEmpty()) {
          this.code = option.getCode();
        }

        if (!option.getFilter().isEmpty()) {
          this.filter = option.getFilter();
        }

        // If debounce is set in the option
        if (option.isDebounce()) {
          this.setDebounce(option.getDebounceTimeout(), option.getDebouncePhase());
        }

        // If throttle is set in the option
        if (option.isThrottle()) {
          this.setThrottle(option.getThrottleTimeout());
        }
      }
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return "ElementEventOptions{" + "items=" + data + ", code='" + code + '\'' + ", filter='"
        + filter + '\'' + ", debounceTimeout=" + debounceTimeout + ", debouncePhase="
        + debouncePhase + ", throttleTimeout=" + throttleTimeout + ", isDebounce=" + isDebounce
        + ", isThrottle=" + isThrottle + '}';
  }
}
