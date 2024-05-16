package com.webforj.optiondialog;

import com.webforj.component.Theme;

/**
 * The base class for themed blocking dialogs.
 *
 * @param <T> the type of the dialog
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
class ThemedDialog<T> extends Dialog<T> {

  private Theme theme = Theme.DEFAULT;

  /**
   * Sets the theme of the message box.
   *
   * @param theme the theme of the message box
   * @return the message box
   */
  public T setTheme(Theme theme) {
    this.theme = theme;
    toggleAttribute("theme", theme.toString().toLowerCase(), theme != null);
    return getSelf();
  }

  /**
   * Gets the theme of the message box.
   *
   * @return the theme of the message box
   */
  public Theme getTheme() {
    return theme;
  }
}
