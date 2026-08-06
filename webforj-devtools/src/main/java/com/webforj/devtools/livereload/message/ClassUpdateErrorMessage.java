package com.webforj.devtools.livereload.message;

import java.util.List;

/**
 * Message sent when the running virtual machine rejected a class redefinition.
 *
 * <p>
 * The change never reached the application, so the page keeps its state as it is and shows the
 * rejection instead of refreshing into the unchanged code.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ClassUpdateErrorMessage extends LiveReloadMessage {

  /** The message type value sent to the client. */
  public static final String TYPE = "class-update-error";

  private final List<String> classes;
  private final String reason;
  private final long timestamp;

  /**
   * Creates a new class update error message.
   *
   * @param classes the binary names of the classes whose redefinition was in flight
   * @param reason the rejection reason the virtual machine reported
   */
  public ClassUpdateErrorMessage(List<String> classes, String reason) {
    super(TYPE);
    this.classes = List.copyOf(classes);
    this.reason = reason;
    this.timestamp = System.currentTimeMillis();
  }

  /**
   * Gets the binary names of the classes whose redefinition was in flight.
   *
   * @return the class names
   */
  public List<String> getClasses() {
    return classes;
  }

  /**
   * Gets the rejection reason the virtual machine reported.
   *
   * @return the reason
   */
  public String getReason() {
    return reason;
  }

  /**
   * Gets the rejection timestamp.
   *
   * @return the timestamp when the rejection was detected
   */
  public long getTimestamp() {
    return timestamp;
  }
}
