package com.webforj.component.upload.event;

import com.webforj.component.upload.Upload;
import java.util.Map;

/**
 * Fired when the transfer of a single file from the {@link Upload} component to the server has
 * failed.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class UploadErrorEvent extends UploadFilesEvent {

  /**
   * Creates a new event for the given component and payload.
   *
   * @param component the component
   * @param payload the event payload (expects {@code files}, {@code message})
   */
  public UploadErrorEvent(Upload component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Gets the failure message reported by the client.
   *
   * @return the message, or an empty string if none was reported
   */
  public String getMessage() {
    Object v = getEventMap().get("message");

    return v == null ? "" : v.toString();
  }
}
