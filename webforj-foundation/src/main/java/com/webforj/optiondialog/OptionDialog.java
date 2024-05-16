package com.webforj.optiondialog;

import com.webforj.UploadedFile;
import com.webforj.component.Theme;
import com.webforj.optiondialog.DwcPromptMsgBox.MessageType;
import com.webforj.optiondialog.InputDialog.InputType;
import java.util.List;

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

  /**
   * Shows a new instance of the input dialog.
   */
  public static String showInputDialog() {
    return new InputDialog().show();
  }

  /**
   * Shows a new instance of the file chooser dialog.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param filters the filters of the file chooser dialog
   * @param activeFilter the active filter of the file chooser dialog
   * @param restricted whether the file chooser dialog is restricted to the current directory
   * @param selectionMode the selection mode of the file chooser dialog
   */
  public static String showFileChooserDialog(String title, String initialPath,
      List<FileChooserFilter> filters, String activeFilter, boolean restricted,
      FileChooserDialog.SelectionMode selectionMode) {
    return new FileChooserDialog(title, initialPath, filters, activeFilter, restricted,
        selectionMode).show();
  }

  /**
   * Shows a new instance of the file chooser dialog with title, initialPath, and filters.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param filters the filters of the file chooser dialog
   */
  public static String showFileChooserDialog(String title, String initialPath,
      List<FileChooserFilter> filters) {
    return new FileChooserDialog(title, initialPath, filters).show();
  }

  /**
   * Shows a new instance of the file chooser dialog with title, initialPath, and selectionMode.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param selectionMode the selection mode of the file chooser dialog
   */
  public static String showFileChooserDialog(String title, String initialPath,
      FileChooserDialog.SelectionMode selectionMode) {
    return new FileChooserDialog(title, initialPath, selectionMode).show();
  }

  /**
   * Shows a new instance of the file chooser dialog with title, initialPath, filters, and
   * activeFilter.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param filters the filters of the file chooser dialog
   * @param activeFilter the active filter of the file chooser dialog
   */
  public static String showFileChooserDialog(String title, String initialPath,
      List<FileChooserFilter> filters, String activeFilter) {
    return new FileChooserDialog(title, initialPath, filters, activeFilter).show();
  }

  /**
   * Shows a new instance of the file chooser dialog with title, initialPath, filters, and
   * restricted.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param filters the filters of the file chooser dialog
   * @param restricted whether the file chooser dialog is restricted to the current directory
   */
  public static String showFileChooserDialog(String title, String initialPath,
      List<FileChooserFilter> filters, boolean restricted) {
    return new FileChooserDialog(title, initialPath, filters, restricted).show();
  }

  /**
   * Shows a new instance of the file chooser dialog with title, initialPath, and restricted.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param restricted whether the file chooser dialog is restricted to the current directory
   */
  public static String showFileChooserDialog(String title, String initialPath, boolean restricted) {
    return new FileChooserDialog(title, initialPath, restricted).show();
  }

  /**
   * Shows a new instance of the file chooser dialog with title, initialPath, filters, restricted,
   * and selectionMode.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   * @param filters the filters of the file chooser dialog
   * @param restricted whether the file chooser dialog is restricted to the current directory
   * @param selectionMode the selection mode of the file chooser dialog
   */
  public static String showFileChooserDialog(String title, String initialPath,
      List<FileChooserFilter> filters, boolean restricted,
      FileChooserDialog.SelectionMode selectionMode) {
    return new FileChooserDialog(title, initialPath, filters, restricted, selectionMode).show();
  }

  /**
   * Shows a new instance of the file chooser dialog with title and initialPath.
   *
   * @param title the title of the file chooser dialog
   * @param initialPath the initial path of the file chooser dialog
   */
  public static String showFileChooserDialog(String title, String initialPath) {
    return new FileChooserDialog(title, initialPath).show();
  }

  /**
   * Shows a new instance of the file chooser dialog with title only.
   *
   * @param title the title of the file chooser dialog
   */
  public static String showFileChooserDialog(String title) {
    return new FileChooserDialog(title).show();
  }

  /**
   * Shows a new instance of the file chooser dialog with default title.
   */
  public static String showFileChooserDialog() {
    return new FileChooserDialog().show();
  }

  /**
   * Shows a new file upload dialog.
   *
   * @param title the dialog title
   */
  public static UploadedFile showFileUploadDialog(String title, List<FileChooserFilter> filters,
      String activeFilter) {
    return new FileUploadDialog(title, filters, activeFilter).show();
  }

  /**
   * Shows a new file upload dialog.
   *
   * @param title the dialog title
   * @param filters the filters
   */
  public static UploadedFile showFileUploadDialog(String title, List<FileChooserFilter> filters) {
    return new FileUploadDialog(title, filters).show();
  }

  /**
   * Shows a new file upload dialog.
   *
   * @param title the dialog title
   */
  public static UploadedFile showFileUploadDialog(String title) {
    return new FileUploadDialog(title).show();
  }

  /**
   * Shows a new file upload dialog.
   */
  public static UploadedFile showFileUploadDialog() {
    return new FileUploadDialog().show();
  }
}
