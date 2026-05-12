package com.webforj.component.upload.event;

import com.webforj.UploadedFile;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.upload.Upload;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Fired when every file queued on the {@link Upload} component has finished its transfer, whether
 * it succeeded or failed.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class UploadCompleteEvent extends ComponentEvent<Upload> {

  /**
   * Creates a new event for the given component and payload.
   *
   * @param component the component
   * @param payload the event payload (expects {@code uploaded} and {@code failed} entries holding
   *        {@link List}s of {@link UploadedFile})
   */
  public UploadCompleteEvent(Upload component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Gets the files that finished their transfer successfully.
   *
   * @return the uploaded files, or an empty list when none completed successfully
   */
  public List<UploadedFile> getUploadedFiles() {
    return readList("uploaded");
  }

  /**
   * Gets the files that failed during transfer.
   *
   * @return the failed files, or an empty list when none failed
   */
  public List<UploadedFile> getFailedFiles() {
    return readList("failed");
  }

  private List<UploadedFile> readList(String key) {
    Object v = getEventMap().get(key);
    if (v instanceof List<?> list) {
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
}
