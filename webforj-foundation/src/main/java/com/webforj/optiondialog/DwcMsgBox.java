package com.webforj.optiondialog;

/**
 * The base class for DWC {@code msgbox} dialog.
 *
 * @param <T> the type of the dialog
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
class DwcMsgBox<T extends DwcPromptMsgBox<T>> extends DwcPromptMsgBox<T> {

  private boolean rawText = false;

  /**
   * Disables/Enables HTML processing for the title and the message.
   *
   * @param rawText when true, HTML processing is disabled, otherwise it is enabled
   * @return the dialog
   */
  public T setRawText(boolean rawText) {
    this.rawText = rawText;
    return getSelf();
  }

  /**
   * Checks if HTML processing is disabled for the title and the message.
   *
   * @return true if HTML processing is disabled, otherwise false
   */
  public boolean isRawText() {
    return rawText;
  }
}
