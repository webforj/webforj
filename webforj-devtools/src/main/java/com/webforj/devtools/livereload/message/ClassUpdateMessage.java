package com.webforj.devtools.livereload.message;

import java.util.List;

/**
 * Message sent when Java classes were redefined in the running application.
 *
 * <p>
 * The message names the redefined classes, so each page can ask its application instance to rebuild
 * exactly the part of the interface those classes drive instead of reloading the whole page.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ClassUpdateMessage extends LiveReloadMessage {

  /** The message type value sent to the client. */
  public static final String TYPE = "class-update";

  private final List<String> classes;
  private final long timestamp;

  /**
   * Creates a new class update message.
   *
   * @param classes the binary names of the redefined classes
   */
  public ClassUpdateMessage(List<String> classes) {
    super(TYPE);
    this.classes = List.copyOf(classes);
    this.timestamp = System.currentTimeMillis();
  }

  /**
   * Gets the binary names of the redefined classes.
   *
   * @return the class names
   */
  public List<String> getClasses() {
    return classes;
  }

  /**
   * Gets the update timestamp.
   *
   * @return the timestamp when the update was detected
   */
  public long getTimestamp() {
    return timestamp;
  }
}
