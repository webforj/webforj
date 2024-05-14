package com.webforj.optiondialog;

import com.webforj.component.Theme;
import com.webforj.optiondialog.DwcMsgBox.MessageType;

/**
 * A utility class for creating and showing dialogs.
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
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
  public static ConfirmDialog.Result showConfirmDialog(Object message, String title,
      ConfirmDialog.OptionType optionType, DwcMsgBox.MessageType messageType) {
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
  public static ConfirmDialog.Result showConfirmDialog(Object message, String title,
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
  public static ConfirmDialog.Result showConfirmDialog(Object message, String title) {
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

  /**
   * Shows a new message dialog with the specified title, message, button text, type and theme.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   * @param buttonText The text of the OK button.
   * @param messageType The type of dialog style.
   * @param theme The theme of the dialog.
   */
  public static void showMessageDialog(Object message, String title, String buttonText,
      DwcMsgBox.MessageType messageType, Theme theme) {
    new MessageDialog(message, title, buttonText, messageType, theme).show();
  }

  /**
   * Shows a new message dialog with the specified title, message, type and theme.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   * @param messageType The type of dialog style.
   * @param theme The theme of the dialog.
   */
  public static void showMessageDialog(Object message, String title, MessageType messageType,
      Theme theme) {
    new MessageDialog(message, title, messageType, theme).show();
  }

  /**
   * Shows a new message dialog with the specified title, message, button text and type.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   * @param buttonText The text of the OK button.
   * @param messageType The type of dialog style.
   */
  public static void showMessageDialog(Object message, String title, String buttonText,
      MessageType messageType) {
    new MessageDialog(message, title, buttonText, messageType).show();
  }

  /**
   * Shows a new message dialog with the specified title, message and button text.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   * @param buttonText The text of the OK button.
   */
  public static void showMessageDialog(Object message, String title, String buttonText) {
    new MessageDialog(message, title, buttonText).show();
  }

  /**
   * Shows a new message dialog with the specified title, message and type.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   * @param messageType The type of dialog style.
   */
  public static void showMessageDialog(Object message, String title, MessageType messageType) {
    new MessageDialog(message, title, messageType).show();
  }

  /**
   * Shows a new message dialog with the specified title and message.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   */
  public static void showMessageDialog(Object message, String title) {
    new MessageDialog(message, title).show();
  }

  /**
   * Shows a new message dialog with the specified message.
   *
   * @param message The message to display in the dialog.
   */
  public static void showMessageDialog(Object message) {
    new MessageDialog(message).show();
  }
}
