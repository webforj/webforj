package com.webforj.devtools.livereload.message;

/**
 * Message a client sends right after connecting, carrying the server clock time its page was served
 * with.
 *
 * <p>
 * The stamp lets the server spot a page that predates the last reload command, so a reload that
 * found nobody connected still reaches that page on its next connection.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class HelloMessage extends LiveReloadMessage {

  /** The message type value the client sends. */
  public static final String TYPE = "hello";

  private final long pageServedAt;

  /**
   * Creates a new hello message.
   *
   * @param pageServedAt the server clock time the page was served, in milliseconds
   */
  public HelloMessage(long pageServedAt) {
    super(TYPE);
    this.pageServedAt = pageServedAt;
  }

  /**
   * Gets the server clock time the page was served.
   *
   * @return the served time in milliseconds, or zero when the page carried no stamp
   */
  public long getPageServedAt() {
    return pageServedAt;
  }
}
