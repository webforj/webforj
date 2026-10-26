package com.webforj.testbed;

/**
 * One option dialog the application showed during a test.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ShownOptionDialog { // NOSONAR

  /**
   * The kinds of option dialogs an application shows.
   */
  public enum Kind {
    /**
     * A confirm dialog, which also carries every message dialog.
     */
    CONFIRM,
    /**
     * An input dialog.
     */
    INPUT,
    /**
     * A file chooser dialog.
     */
    FILE_CHOOSER,
    /**
     * A file save dialog.
     */
    FILE_SAVE,
    /**
     * A file upload dialog.
     */
    FILE_UPLOAD
  }

  private final Kind kind;
  private final String title;
  private final String message;

  /**
   * Creates a request.
   *
   * @param kind the dialog kind
   * @param title the dialog title
   * @param message the dialog message, null for file dialogs
   */
  public ShownOptionDialog(Kind kind, String title, String message) {
    this.kind = kind;
    this.title = title;
    this.message = message;
  }

  /**
   * Returns the dialog kind.
   *
   * @return the kind
   */
  public Kind getKind() {
    return kind;
  }

  /**
   * Returns the dialog title.
   *
   * @return the title
   */
  public String getTitle() {
    return title;
  }

  /**
   * Returns the dialog message, null for file dialogs.
   *
   * @return the message
   */
  public String getMessage() {
    return message;
  }

  @Override
  public String toString() {
    return kind + " dialog titled '" + title + "'" + (message == null ? "" : ": " + message);
  }
}
