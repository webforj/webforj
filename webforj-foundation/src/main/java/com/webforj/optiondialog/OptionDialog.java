package com.webforj.optiondialog;

import com.webforj.component.Theme;
import com.webforj.optiondialog.DwcPromptMsgBox.MessageType;
import com.webforj.optiondialog.InputDialog.InputType;

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
      ConfirmDialog.OptionType optionType, DwcPromptMsgBox.MessageType messageType) {
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
      DwcPromptMsgBox.MessageType messageType, Theme theme) {
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
  public static void showMessageDialog(Object message, String title,
      DwcPromptMsgBox.MessageType messageType, Theme theme) {
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
      DwcPromptMsgBox.MessageType messageType) {
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
  public static void showMessageDialog(Object message, String title,
      DwcPromptMsgBox.MessageType messageType) {
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



  /**
   * Shows a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param defaultValue the default value of the input field
   * @param messageType The type of dialog style.
   * @param inputType the type of input field
   */
  public static String showInputDialog(String message, String title, String defaultValue,
      MessageType messageType, InputType inputType) {
    return new InputDialog(message, title, defaultValue, messageType, inputType).show();
  }

  /**
   * Shows a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param defaultValue the default value of the input field
   * @param messageType The type of dialog style.
   */
  public static String showInputDialog(Object message, String title, String defaultValue,
      MessageType messageType) {
    return new InputDialog(message, title, defaultValue, messageType).show();
  }

  /**
   * Shows a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param defaultValue the default value of the input field
   * @param inputType the type of input field
   */
  public static String showInputDialog(Object message, String title, String defaultValue,
      InputType inputType) {
    return new InputDialog(message, title, defaultValue, inputType).show();
  }

  /**
   * Shows a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param messageType The type of dialog style.
   * @param inputType the type of input field
   */
  public static String showInputDialog(Object message, String title, MessageType messageType,
      InputType inputType) {
    return new InputDialog(message, title, messageType, inputType).show();
  }

  /**
   * Shows a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param messageType The type of dialog style.
   */
  public static String showInputDialog(Object message, String title, MessageType messageType) {
    return new InputDialog(message, title, messageType).show();
  }

  /**
   * Shows a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param inputType the type of input field
   */
  public static String showInputDialog(Object message, String title, InputType inputType) {
    return new InputDialog(message, title, inputType).show();
  }

  /**
   * Shows a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param messageType The type of dialog style.
   * @param inputType the type of input field
   */
  public static String showInputDialog(Object message, MessageType messageType,
      InputType inputType) {
    return new InputDialog(message, messageType, inputType).show();
  }

  /**
   * Shows a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param messageType The type of dialog style.
   */
  public static String showInputDialog(Object message, MessageType messageType) {
    return new InputDialog(message, messageType).show();
  }

  /**
   * Shows a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param inputType the type of input field
   */
  public static String showInputDialog(Object message, InputType inputType) {
    return new InputDialog(message, inputType).show();
  }

  /**
   * Shows a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param defaultValue the default value of the input field
   */
  public static String showInputDialog(Object message, String title, String defaultValue) {
    return new InputDialog(message, title, defaultValue).show();
  }

  /**
   * Shows a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   */
  public static String showInputDialog(Object message, String title) {
    return new InputDialog(message, title).show();
  }

  /**
   * Shows a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   */
  public static String showInputDialog(Object message) {
    return new InputDialog(message).show();
  }
}
