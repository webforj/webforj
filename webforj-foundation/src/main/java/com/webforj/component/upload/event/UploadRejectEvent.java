package com.webforj.component.upload.event;

import com.webforj.component.upload.Upload;
import java.util.Map;

/**
 * Fired when a file picked or dropped on the {@link Upload} component is rejected before uploading
 * because it does not meet the configured constraints (for example, the file is too large or has a
 * disallowed extension).
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class UploadRejectEvent extends UploadFilesEvent {

  /**
   * Creates a new event for the given component and payload.
   *
   * @param component the component
   * @param payload the event payload (expects {@code files}, {@code message})
   */
  public UploadRejectEvent(Upload component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Gets the human readable rejection message reported by the client.
   *
   * @return the message, or an empty string if none was reported
   */
  public String getMessage() {
    Object v = getEventMap().get("message");

    return v == null ? "" : v.toString();
  }
}
