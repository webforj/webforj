package com.webforj.component.fileupload.event;

import com.webforj.component.event.ComponentEvent;
import com.webforj.component.fileupload.FileUpload;
import java.util.Map;

/**
 * Fired when the active file filter changes on a {@link FileUpload} component.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class FileUploadFilterChangeEvent extends ComponentEvent<FileUpload> {

  /**
   * Creates a new event for the given component and payload.
   *
   * @param component the component
   * @param payload the event payload that contains the {@code filter} entry
   */
  public FileUploadFilterChangeEvent(FileUpload component, Map<String, Object> payload) {
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
