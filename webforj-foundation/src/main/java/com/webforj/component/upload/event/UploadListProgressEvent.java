package com.webforj.component.upload.event;

import com.webforj.component.event.ComponentEvent;
import com.webforj.component.upload.Upload;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Fires on every progress tick alongside {@link UploadProgressEvent}, carrying the whole list state
 * so consumers can compute any aggregate without keeping state between ticks.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 *
 * @see UploadProgressEvent
 */
public class UploadListProgressEvent extends ComponentEvent<Upload> {

  /**
   * Creates a new event for the given component and payload.
   *
   * @param component the component
   * @param payload the event payload
   */
  public UploadListProgressEvent(Upload component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Gets the total number of files in the current upload list.
   *
   * @return list size, or {@code -1} if not reported
   */
  public int getListTotal() {
    return asInt(getEventMap().get("listTotal"), -1);
  }

  /**
   * Gets the number of files in the list that have not yet started transferring.
   *
   * @return remaining files, or {@code -1} if not reported
   */
  public int getListRemaining() {
    return asInt(getEventMap().get("listRemaining"), -1);
  }

  /**
   * Gets the total bytes transferred so far across every file in the list.
   *
   * @return aggregated transferred bytes, or {@code -1} if not reported
   */
  public long getListTransferredBytes() {
    return asLong(getEventMap().get("listTransferredBytes"), -1L);
  }

  /**
   * Gets the total bytes expected across every file in the list.
   *
   * @return aggregated total bytes, or {@code -1} if not reported
   */
  public long getListTotalBytes() {
    return asLong(getEventMap().get("listTotalBytes"), -1L);
  }

  /**
   * Derives the byte weighted overall transfer progress across the list.
   *
   * @return percentage 0.0 to 100.0, or {@code -1.0} if byte totals are unknown
   */
  public double getListProgress() {
    long total = getListTotalBytes();
    if (total <= 0) {
      return -1.0;
    }

    return getListTransferredBytes() * 100.0 / total;
  }

  /**
   * Gets a snapshot of every file currently in the upload list.
   *
   * @return the entries, never {@code null}
   */
  public List<Entry> getEntries() {
    Object raw = getEventMap().get("entries");
    if (!(raw instanceof List<?> list)) {
      return Collections.emptyList();
    }

    List<Entry> out = new ArrayList<>();
    for (Object o : list) {
      if (o instanceof Map<?, ?> map) {
        out.add(toEntry(map));
      }
    }

    return Collections.unmodifiableList(out);
  }

  private static Entry toEntry(Map<?, ?> map) {
    Object nameRaw = map.get("name");
    String name = nameRaw == null ? "" : nameRaw.toString();
    long size = asLong(map.get("size"), 0L);
    long transferred = asLong(map.get("loaded"), 0L);
    Object stateRaw = map.get("state");
    Entry.State state = parseState(stateRaw == null ? null : stateRaw.toString());

    return new Entry(name, size, transferred, state);
  }

  private static int asInt(Object v, int def) {
    return v instanceof Number n ? n.intValue() : def;
  }

  private static long asLong(Object v, long def) {
    return v instanceof Number n ? n.longValue() : def;
  }

  private static Entry.State parseState(String s) {
    if (s == null) {
      return Entry.State.QUEUED;
    }

    return switch (s) {
      case "inProgress" -> Entry.State.IN_PROGRESS;
      case "complete" -> Entry.State.COMPLETE;
      case "error" -> Entry.State.ERROR;
      default -> Entry.State.QUEUED;
    };
  }

  /**
   * Snapshot of one file's state in the upload list at the moment this event fired.
   */
  public static final class Entry { // NOSONAR - simple data holder, no need for equals/hashCode

    /**
     * Lifecycle position of a file in the list.
     */
    public enum State {
      /** In the list but not started yet. */
      QUEUED,

      /** Actively transferring. */
      IN_PROGRESS,

      /** Finished successfully. */
      COMPLETE,

      /** Ended with an error. */
      ERROR
    }

    private final String name;
    private final long size;
    private final long transferredBytes;
    private final State state;

    /**
     * Creates a new entry.
     *
     * @param name client file name
     * @param size client reported file size in bytes
     * @param transferredBytes client reported bytes transferred so far
     * @param state lifecycle state
     */
    public Entry(String name, long size, long transferredBytes, State state) {
      this.name = name;
      this.size = size;
      this.transferredBytes = transferredBytes;
      this.state = state;
    }

    /**
     * Gets the client file name.
     *
     * @return the file name
     */
    public String getName() {
      return name;
    }

    /**
     * Gets the client reported file size in bytes, as exposed by the browser's {@code File.size}
     * API at pick time. Never round trips to the server, so consumers must not rely on it for
     * billing, quota, or integrity checks.
     *
     * @return the client reported file size in bytes
     */
    public long getSize() {
      return size;
    }

    /**
     * Gets the client reported number of bytes transferred so far for this file. The value is
     * clamped to {@link #getSize()} so the reported count never claims more bytes than the file is
     * known to contain.
     *
     * @return client reported bytes transferred so far
     */
    public long getTransferredBytes() {
      return transferredBytes;
    }

    /**
     * Derives the client reported transfer progress for this file as a percentage between
     * {@code 0.0} and {@code 100.0}.
     *
     * @return the percentage, or {@code -1.0} if size is unknown
     */
    public double getProgress() {
      if (size <= 0) {
        return -1.0;
      }

      return transferredBytes * 100.0 / size;
    }

    /**
     * Gets the lifecycle state of this file.
     *
     * @return the lifecycle state
     */
    public State getState() {
      return state;
    }
  }
}
