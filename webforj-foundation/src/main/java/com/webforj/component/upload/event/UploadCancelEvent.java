package com.webforj.component.upload.event;

import com.webforj.component.event.ComponentEvent;
import com.webforj.component.upload.Upload;
import java.util.Map;

/**
 * Fired when the user cancels the selection on a {@link Upload} component.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class UploadCancelEvent extends ComponentEvent<Upload> {

  /**
   * Creates a new event for the given component.
   *
   * @param component the component
   * @param payload the event payload
   */
  public UploadCancelEvent(Upload component, Map<String, Object> payload) {
    super(component, payload);
  }
}
