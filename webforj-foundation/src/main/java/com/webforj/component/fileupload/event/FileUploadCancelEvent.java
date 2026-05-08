package com.webforj.component.fileupload.event;

import com.webforj.component.event.ComponentEvent;
import com.webforj.component.fileupload.FileUpload;
import java.util.Map;

/**
 * Fired when the user cancels the selection on a {@link FileUpload} component.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class FileUploadCancelEvent extends ComponentEvent<FileUpload> {

  /**
   * Creates a new event for the given component.
   *
   * @param component the component
   * @param payload the event payload
   */
  public FileUploadCancelEvent(FileUpload component, Map<String, Object> payload) {
    super(component, payload);
  }
}
