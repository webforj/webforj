package org.dwcj.component.element.event;

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
  private final Map<String, String> items;
  private String code;
  private String filter;

  /**
   * Creates a new instance of the event options.
   *
   * @param items The event items.
   * @param code The event code.
   * @param filter The event filter.
   */
  public ElementEventOptions(Map<String, String> items, String code, String filter) {
    this.items = items == null ? new HashMap<>() : items;
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
   * Adds an item to the event options.
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
  public ElementEventOptions addItem(String name, String expression) {
    items.put(name, expression);
    return this;
  }

  /**
   * Retrieves an item from the event options.
   *
   * @param name The name of the item to retrieve.
   * @return The item value.
   */
  public String getItem(String name) {
    return items.get(name);
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
   * Retrieves the map of event items.
   *
   * @return Unmodifiable map of event items.
   */
  public Map<String, String> getItems() {
    return Collections.unmodifiableMap(items);
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
   * Retrieves the event filter expression.
   *
   * @return The event filter expression.
   */
  public String getFilter() {
    return filter;
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
        this.items.putAll(option.getItems());

        // Override code and filter if present in the currentOption
        if (!option.getCode().isEmpty()) {
          this.code = option.getCode();
        }

        if (!option.getFilter().isEmpty()) {
          this.filter = option.getFilter();
        }
      }
    }

    return this;
  }
}
