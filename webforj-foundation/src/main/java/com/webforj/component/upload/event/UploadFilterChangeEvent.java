package com.webforj.component.upload.event;

import com.webforj.component.event.ComponentEvent;
import com.webforj.component.upload.Upload;
import java.util.Map;

/**
 * Fired when the active file filter changes on a {@link Upload} component.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class UploadFilterChangeEvent extends ComponentEvent<Upload> {

  /**
   * Creates a new event for the given component and payload.
   *
   * @param component the component
   * @param payload the event payload that contains the {@code filter} entry
   */
  public UploadFilterChangeEvent(Upload component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Gets the description of the active filter.
   *
   * @return the active filter description, or an empty string when no filter is active
   */
  public String getActiveFilter() {
    Object filter = getEventMap().get("filter");

    return filter == null ? "" : String.valueOf(filter);
  }
}
