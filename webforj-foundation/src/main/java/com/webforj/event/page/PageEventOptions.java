package com.webforj.event.page;

import com.webforj.component.element.event.DebouncePhase;
import com.webforj.component.element.event.ElementEventOptions;
import java.util.Map;

/**
 * Represents event options for a page event.
 *
 * <p>
 * This class provides a way to configure and customize event options for the page. Event options
 * can include items, code to be evaluated on the client-side before an event is fired, and a filter
 * expression to determine whether an event should be fired or not.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public final class PageEventOptions extends ElementEventOptions {

  /**
   * Creates a new instance of the event options.
   *
   * @param data The event data.
   * @param code The event code.
   * @param filter The event filter.
   */
  public PageEventOptions(Map<String, String> data, String code, String filter) {
    super(data, code, filter);
  }

  /**
   * Creates a new instance of the event options.
   *
   * @param items The event items.
   * @param code The event code.
   */
  public PageEventOptions(Map<String, String> items, String code) {
    this(items, code, null);
  }

  /**
   * Creates a new instance of the event options.
   *
   * @param items The event items.
   */
  public PageEventOptions(Map<String, String> items) {
    this(items, null, null);
  }

  /**
   * Creates a new instance of the event options with default values.
   */
  public PageEventOptions() {
    this(null, null, null);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PageEventOptions addData(String name, String expression) {
    super.addData(name, expression);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PageEventOptions setCode(String expression) {
    super.setCode(expression);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PageEventOptions setFilter(String expression) {
    super.setFilter(expression);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PageEventOptions setDebounce(int timeout, DebouncePhase phase) {
    super.setDebounce(timeout, phase);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PageEventOptions setDebounce(int timeout) {
    super.setDebounce(timeout);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PageEventOptions setThrottle(int timeout) {
    super.setThrottle(timeout);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PageEventOptions mergeWith(ElementEventOptions... options) {
    super.mergeWith(options);
    return this;
  }
}
