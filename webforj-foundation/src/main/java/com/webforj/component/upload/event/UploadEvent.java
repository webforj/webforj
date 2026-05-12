package com.webforj.component.upload.event;

import com.webforj.UploadedFile;
import com.webforj.component.upload.Upload;
import java.util.Map;

/**
 * Fired when the user uploads the current selection on a {@link Upload} component.
 *
 * <p>
 * The event carries the list of uploaded {@link UploadedFile} entries through {@link #getFiles()}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class UploadEvent extends UploadFilesEvent {

  /**
   * Creates a new event for the given component and payload.
   *
   * @param component the component
   * @param payload the event payload that contains the {@code files} entry
   */
  public UploadEvent(Upload component, Map<String, Object> payload) {
    super(component, payload);
  }
}
