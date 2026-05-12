package com.webforj.component.upload.event;

import com.webforj.component.upload.Upload;
import java.util.Map;

/**
 * Fires while a single file is being transferred to the server.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 *
 * @see UploadListProgressEvent
 */
public class UploadProgressEvent extends UploadFilesEvent {

  /**
   * Creates a new event for the given component and payload.
   *
   * @param component the component
   * @param payload the event payload (expects {@code files}, {@code progress},
   *        {@code transferredBytes}, {@code totalBytes})
   */
  public UploadProgressEvent(Upload component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Derives the client reported transfer progress for this file as a percentage between {@code 0.0}
   * and {@code 100.0}, computed from {@link #getTransferredBytes()} and {@link #getTotalBytes()}.
   *
   * @return the percentage, or {@code -1.0} if byte totals are unknown
   */
  public double getProgress() {
    long total = getTotalBytes();
    if (total <= 0) {
      return -1.0;
    }

    return getTransferredBytes() * 100.0 / total;
  }

  /**
   * Gets the client reported number of bytes transferred so far for this file. The value advances
   * as the client streams the file to the server.
   *
   * @return client reported bytes transferred, or {@code -1} if unknown
   */
  public long getTransferredBytes() {
    Object v = getEventMap().get("transferredBytes");
    if (v instanceof Number n) {
      return n.longValue();
    }

    return -1L;
  }

  /**
   * Gets the client reported total size of this file, as exposed by the browser at pick time.
   *
   * @return client reported total bytes, or {@code -1} if unknown
   */
  public long getTotalBytes() {
    Object v = getEventMap().get("totalBytes");
    if (v instanceof Number n) {
      return n.longValue();
    }

    return -1L;
  }
}
