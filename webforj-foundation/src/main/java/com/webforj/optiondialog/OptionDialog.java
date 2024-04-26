package com.webforj.optiondialog;

/**
 * A utility class for creating and showing MessageBoxDialogs.
 */
public final class OptionDialog {

  private OptionDialog() {
    // Prevent instantiation
  }

  /**
   * Create a new MessageBoxDialog with the specified title, message, and type.
   *
   * @param title The title of the dialog.
   * @param message The message to display in the dialog.
   * @param type The type of button to display in the dialog.
   * @param iconType The type of icon to display in the dialog.
   *
   * @return A new MessageBoxDialog.
   */
  public static MessageDialog createMessageDialog(Object title, Object message,
      MessageDialog.MessageType type, MessageDialog.IconType iconType) {
    MessageDialog dialog = new MessageDialog();
    dialog.setTitle(title);
    dialog.setMessage(message);
    dialog.setMessageType(type);
    dialog.setIconType(iconType);

    return dialog;
  }

  /**
   * Create a new MessageBoxDialog with the specified title, message, and type.
   *
   * @param title The title of the dialog.
   * @param message The message to display in the dialog.
   * @param type The type of button to display in the dialog.
   *
   * @return A new MessageBoxDialog.
   */
  public static MessageDialog createMessageDialog(Object title, Object message,
      MessageDialog.MessageType type) {
    return createMessageDialog(title, message, type, MessageDialog.IconType.NONE);
  }

  /**
   * Create a new MessageBoxDialog with the specified title, message, and type.
   *
   * @param title The title of the dialog.
   * @param message The message to display in the dialog.
   *
   * @return A new MessageBoxDialog.
   */
  public static MessageDialog createMessageDialog(Object title, Object message) {
    return createMessageDialog(title, message, MessageDialog.MessageType.OK,
        MessageDialog.IconType.NONE);
  }

  /**
   * Create a new MessageBoxDialog with the specified title, message, and type.
   *
   * @param message The message to display in the dialog.
   *
   * @return A new MessageBoxDialog.
   */
  public static MessageDialog createMessageDialog(Object message) {
    return createMessageDialog("MessageDialog", message, MessageDialog.MessageType.OK,
        MessageDialog.IconType.NONE);
  }

  /**
   * Shows a new MessageBoxDialog with the specified title, message, and type.
   *
   * @param title The title of the dialog.
   * @param message The message to display in the dialog.
   * @param type The type of button to display in the dialog.
   * @param iconType The type of icon to display in the dialog.
   *
   * @return A new MessageBoxDialog.
   */
  public static MessageDialog showMessageDialog(Object title, Object message,
      MessageDialog.MessageType type, MessageDialog.IconType iconType) {
    MessageDialog dialog = createMessageDialog(title, message, type, iconType);
    dialog.show();
    return dialog;
  }

  /**
   * Shows a new MessageBoxDialog with the specified title, message, and type.
   *
   * @param title The title of the dialog.
   * @param message The message to display in the dialog.
   * @param type The type of button to display in the dialog.
   *
   * @return A new MessageBoxDialog.
   */
  public static MessageDialog showMessageDialog(Object title, Object message,
      MessageDialog.MessageType type) {
    return showMessageDialog(title, message, type, MessageDialog.IconType.NONE);
  }

  /**
   * Shows a new MessageBoxDialog with the specified title, message, and type.
   *
   * @param title The title of the dialog.
   * @param message The message to display in the dialog.
   *
   * @return A new MessageBoxDialog.
   */
  public static MessageDialog showMessageDialog(Object title, Object message) {
    return showMessageDialog(title, message, MessageDialog.MessageType.OK,
        MessageDialog.IconType.NONE);
  }

  /**
   * Shows a new MessageBoxDialog with the specified title, message, and type.
   *
   * @param message The message to display in the dialog.
   *
   * @return A new MessageBoxDialog.
   */
  public static MessageDialog showMessageDialog(Object message) {
    return showMessageDialog("MessageDialog", message, MessageDialog.MessageType.OK,
        MessageDialog.IconType.NONE);
  }
}
