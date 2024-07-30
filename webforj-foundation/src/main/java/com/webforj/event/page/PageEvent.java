package com.webforj.event.page;

import com.webforj.Page;
import java.util.Collections;
import java.util.EventObject;
import java.util.Map;

/**
 * Represents an event associated with the {@link Page}.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public final class PageEvent extends EventObject {
  private final transient Map<String, Object> payload;
  private final transient PageEventOptions options;
  private final String type;
  private final int id;

  /**
   * Creates a new PageEvent instance.
   *
   * @param page The page associated with the event.
   * @param payload A map containing additional data associated with the event.
   * @param type The type of the event.
   * @param options The event options.
   */
  public PageEvent(Page page, Map<String, Object> payload, String type, int id,
      PageEventOptions options) {
    super(page);
    this.payload = payload;
    this.type = type;
    this.id = id;
    this.options = options;
  }

  /**
   * Get the event id.
   *
   * @return The event id.
   */
  public int getId() {
    return id;
  }

  /**
   * Get the type of the event.
   *
   * @return The event type as a string.
   */
  public String getType() {
    return type;
  }

  /**
   * Get the event options.
   *
   * @return The event options.
   */
  public PageEventOptions getOptions() {
    return options;
  }

  /**
   * Gets the event map sent by the page.
   *
   * <p>
   * The event map is a serializable map from the original client event sent by the page.
   * </p>
   *
   * @return the event map
   */
  public Map<String, Object> getData() {
    return Collections.unmodifiableMap(payload);
  }

  /**
   * Alias for {@link #getData()} method.
   *
   * @return the event map
   */
  public Map<String, Object> getEventMap() {
    return getData();
  }

  /**
   * Gets the page instance associated with the event.
   *
   * @return The page instance.
   */
  public Page getPage() {
    return (Page) super.getSource();
  }
}
