package com.webforj.component.fileupload.event;

import com.basis.startup.type.BBjVector;
import com.webforj.UploadedFile;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.fileupload.FileUpload;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Base type for {@link FileUpload} events whose payload carries a {@code files} entry holding the
 * BBj selected files vector.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
abstract class FileUploadFilesEvent extends ComponentEvent<FileUpload> {

  /**
   * Creates a new event for the given component and payload.
   *
   * @param component the component
   * @param payload the event payload that contains the {@code files} entry
   */
  protected FileUploadFilesEvent(FileUpload component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Gets the snapshot of {@link UploadedFile} entries decoded from the payload.
   *
   * @return the list of files, never {@code null}
   */
  public List<UploadedFile> getFiles() {
    Object files = getEventMap().get("files");
    if (!(files instanceof BBjVector vector)) {
      return Collections.emptyList();
    }

    List<UploadedFile> out = new ArrayList<>();
    for (int i = 0; i < vector.size(); i++) {
      Object name = vector.get(i);
      if (name != null) {
        out.add(new UploadedFile(String.valueOf(name)));
      }
    }

    return Collections.unmodifiableList(out);
  }
}
