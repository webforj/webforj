package com.webforj;

/**
 * The visibility state of the {@link Page} reported by the browser.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public enum PageVisibilityState {

  /**
   * The page content is at least partially visible. In practice, this means the page is the
   * foreground tab of a non minimized window.
   */
  VISIBLE("visible"),

  /**
   * The page content is not visible to the user. In practice, the document is either a background
   * tab, in a minimized window, the screen is locked, or the operating system is showing a
   * screensaver.
   */
  HIDDEN("hidden");

  private final String value;

  PageVisibilityState(String value) {
    this.value = value;
  }

  /**
   * Returns the string value reported by the browser for this state.
   *
   * @return the state value
   */
  public String getValue() {
    return value;
  }

  /**
   * Resolves a state from the string value reported by the browser. Any value that is not
   * {@code "visible"} maps to {@link #HIDDEN}, which matches the conservative reading of the
   * specification for legacy values such as {@code "prerender"} or {@code "unloaded"}.
   *
   * @param value the value reported by the browser
   * @return the resolved state
   */
  public static PageVisibilityState fromValue(String value) {
    if (VISIBLE.value.equalsIgnoreCase(value)) {
      return VISIBLE;
    }

    return HIDDEN;
  }
}
