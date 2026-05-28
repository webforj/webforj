package com.webforj.component.upload.event;

import com.webforj.UploadedFile;
import com.webforj.component.upload.Upload;
import java.util.Map;

/**
 * Fired each time the user picks files on a {@link Upload} component.
 *
 * <p>
 * {@link #getFiles()} returns only the {@link UploadedFile} entries added by the most recent
 * selection, not the cumulative list of files currently held by the component. When the user picks
 * files repeatedly, the event fires once per pick and each fire reports just that pick.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class UploadChangeEvent extends UploadFilesEvent {

  /**
   * Creates a new event for the given component and payload.
   *
   * @param component the component
   * @param payload the event payload that contains the {@code files} entry
   */
  public UploadChangeEvent(Upload component, Map<String, Object> payload) {
    super(component, payload);
  }
}
