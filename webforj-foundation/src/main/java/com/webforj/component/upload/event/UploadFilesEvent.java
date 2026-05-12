package com.webforj.component.upload.event;

import com.basis.startup.type.BBjVector;
import com.webforj.UploadedFile;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.upload.Upload;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Base type for {@link Upload} events whose payload carries a {@code files} entry.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public abstract class UploadFilesEvent extends ComponentEvent<Upload> {

  /**
   * Creates a new event for the given component and payload.
   *
   * @param component the component
   * @param payload the event payload that contains the {@code files} entry
   */
  protected UploadFilesEvent(Upload component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Gets the snapshot of {@link UploadedFile} entries decoded from the payload.
   *
   * @return the list of files, never {@code null}
   */
  public List<UploadedFile> getFiles() {
    Object files = getEventMap().get("files");

    if (files instanceof BBjVector vector) {
      List<UploadedFile> out = new ArrayList<>();
      for (int i = 0; i < vector.size(); i++) {
        Object name = vector.get(i);
        if (name != null) {
          out.add(new UploadedFile(String.valueOf(name)));
        }
      }

      return Collections.unmodifiableList(out);
    }

    if (files instanceof List<?> list) {
      List<UploadedFile> out = new ArrayList<>();
      for (Object o : list) {
        if (o instanceof UploadedFile uf) {
          out.add(uf);
        }
      }

      return Collections.unmodifiableList(out);
    }

    return Collections.emptyList();
  }

  /**
   * Convenience accessor for events that report on a single file.
   *
   * <p>
   * Returns the first entry from {@link #getFiles()}, or {@code null} when no file is reported.
   * </p>
   *
   * @return the single file, or {@code null}
   */
  public UploadedFile getFile() {
    List<UploadedFile> files = getFiles();

    return files.isEmpty() ? null : files.get(0);
  }
}
