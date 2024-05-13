package com.webforj.optiondialog;

/**
 * A utility class for creating and showing ConfirmDialogs.
 */
public final class OptionDialog {

  private OptionDialog() {
    // Prevent instantiation
  }

  /**
   * Shows a new ConfirmDialog with the specified title, message, and types.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   * @param optionType The type of buttons to display in the dialog.
   * @param messageType The type of dialog style.
   *
   * @return The result of the dialog.
   */
  public static ConfirmDialog.Result showConfirmDialog(Object message, Object title,
      ConfirmDialog.OptionType optionType, ConfirmDialog.MessageType messageType) {
    return new ConfirmDialog(message, title, optionType, messageType).show();
  }

  /**
   * Shows a new ConfirmDialog with the specified title, message, and type.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   * @param optionType The type of buttons to display in the dialog.
   *
   * @return The result of the dialog.
   */
  public static ConfirmDialog.Result showConfirmDialog(Object message, Object title,
      ConfirmDialog.OptionType optionType) {
    return new ConfirmDialog(message, title, optionType).show();
  }

  /**
   * Shows a new ConfirmDialog with the specified title, message, and type.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   *
   * @return The result of the dialog.
   */
  public static ConfirmDialog.Result showConfirmDialog(Object message, Object title) {
    return new ConfirmDialog(message, title).show();
  }

  /**
   * Shows a new ConfirmDialog with the specified title, message, and type.
   *
   * @param message The message to display in the dialog.
   *
   * @return The result of the dialog.
   */
  public static ConfirmDialog.Result showConfirmDialog(Object message) {
    return new ConfirmDialog(message).show();
  }
}
