package com.webforj.component.fileupload.event;

import com.webforj.UploadedFile;
import com.webforj.component.fileupload.FileUpload;
import java.util.Map;

/**
 * Fired when the list of selected files on a {@link FileUpload} component changes.
 *
 * <p>
 * The event carries the current list of {@link UploadedFile} entries through {@link #getFiles()}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class FileUploadChangeEvent extends FileUploadFilesEvent {

  /**
   * Creates a new event for the given component and payload.
   *
   * @param component the component
   * @param payload the event payload that contains the {@code files} entry
   */
  public FileUploadChangeEvent(FileUpload component, Map<String, Object> payload) {
    super(component, payload);
  }
}
