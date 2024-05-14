package com.webforj.optiondialog;

import com.webforj.component.Theme;
import com.webforj.component.button.ButtonTheme;

/**
 * Represents a message dialog.
 *
 * <p>
 * The message dialog is a simple dialog that displays a message to the user and allows the user to
 * click an OK button to dismiss the dialog. The dialog is modal and will block application
 * execution until the user dismisses the dialog or the dialog is closed by timeout.
 * </p>
 *
 * @since 24.02
 */
public final class MessageDialog extends DwcMsgBox<MessageDialog> {
  static final String DEFAULT_TITLE = "Message";
  static final String DEFAULT_BUTTON_TEXT = "OK";

  private String buttonText = DEFAULT_BUTTON_TEXT;
  private ButtonTheme buttonTheme = ButtonTheme.DEFAULT;

  /**
   * Creates a new message dialog with the specified title, message, button text, type and theme.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   * @param buttonText The text of the OK button.
   * @param messageType The type of dialog style.
   * @param theme The theme of the dialog.
   */
  public MessageDialog(Object message, String title, String buttonText, MessageType messageType,
      Theme theme) {
    setMessage(message);
    setTitle(title);
    setButtonText(buttonText);
    setMessageType(messageType);
    setTheme(theme);
    setButtonTheme(detectButtonThemeFromDialogTheme(theme));
  }

  /**
   * Creates a new message dialog with the specified title, message, type and theme.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   * @param messageType The type of dialog style.
   * @param theme The theme of the dialog.
   */
  public MessageDialog(Object message, String title, MessageType messageType, Theme theme) {
    this(message, title, DEFAULT_BUTTON_TEXT, messageType, theme);
  }

  /**
   * Creates a new message dialog with the specified title, message, button text and type.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   * @param buttonText The text of the OK button.
   * @param messageType The type of dialog style.
   */
  public MessageDialog(Object message, String title, String buttonText, MessageType messageType) {
    this(message, title, buttonText, messageType, detectDialogThemeFromMessageType(messageType));
  }

  /**
   * Creates a new message dialog with the specified title, message and button text.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   * @param buttonText The text of the OK button.
   */
  public MessageDialog(Object message, String title, String buttonText) {
    this(message, title, buttonText, MessageType.PLAIN);
  }

  /**
   * Creates a new message dialog with the specified title, message and type.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   * @param messageType The type of dialog style.
   */
  public MessageDialog(Object message, String title, MessageType messageType) {
    this(message, title, DEFAULT_BUTTON_TEXT, messageType);
  }

  /**
   * Creates a new message dialog with the specified title and message.
   *
   * @param message The message to display in the dialog.
   * @param title The title of the dialog.
   */
  public MessageDialog(Object message, String title) {
    this(message, title, DEFAULT_BUTTON_TEXT);
  }

  /**
   * Creates a new message dialog with the specified message.
   *
   * @param message The message to display in the dialog.
   */
  public MessageDialog(Object message) {
    this(message, DEFAULT_TITLE);
  }

  /**
   * Creates a new message dialog with an empty message.
   */
  public MessageDialog() {
    this("");
  }

  /**
   * Sets the text of the OK button.
   *
   * @param text the text of the OK button
   * @return the dialog
   */
  public MessageDialog setButtonText(String text) {
    this.buttonText = text;
    return this;
  }

  /**
   * Gets the text of the OK button.
   *
   * @return the text of the OK button
   */
  public String getButtonText() {
    return buttonText;
  }

  /**
   * Sets the theme of the OK button.
   *
   * @param theme the theme of the OK button
   * @return the dialog
   */
  public MessageDialog setButtonTheme(ButtonTheme theme) {
    this.buttonTheme = theme;
    return this;
  }

  /**
   * Gets the theme of the OK button.
   *
   * @return the theme of the OK button
   */
  public ButtonTheme getButtonTheme() {
    return buttonTheme;
  }

  /**
   * Shows the message dialog.
   */
  public void show() {
    ConfirmDialog dialog = new ConfirmDialog(getMessage(), getTitle(),
        ConfirmDialog.OptionType.CUSTOM, getMessageType());
    dialog.setTimeout(getTimeout());
    dialog.setRawText(isRawText());
    dialog.setAttributes(getAttributes());
    dialog.setFirstButtonText(getButtonText());
    dialog.setFirstButtonTheme(getButtonTheme());
    dialog.show();
  }

  /**
   * Alias to {@link #show()}.
   */
  public void open() {
    show();
  }

  static ButtonTheme detectButtonThemeFromDialogTheme(Theme theme) {
    ButtonTheme buttonTheme = ButtonTheme.DEFAULT;
    switch (theme) {
      case DANGER:
        buttonTheme = ButtonTheme.DANGER;
        break;
      case INFO:
        buttonTheme = ButtonTheme.INFO;
        break;
      case PRIMARY:
        buttonTheme = ButtonTheme.PRIMARY;
        break;
      case SUCCESS:
        buttonTheme = ButtonTheme.SUCCESS;
        break;
      case WARNING:
        buttonTheme = ButtonTheme.WARNING;
        break;
      case GRAY:
        buttonTheme = ButtonTheme.GRAY;
        break;
      default:
    }
    return buttonTheme;
  }

  static Theme detectDialogThemeFromMessageType(MessageType messageType) {
    Theme theme = Theme.DEFAULT;
    switch (messageType) {
      case ERROR:
        theme = Theme.DANGER;
        break;
      case INFO:
        theme = Theme.INFO;
        break;
      case WARNING:
        theme = Theme.WARNING;
        break;
      case QUESTION:
        theme = Theme.PRIMARY;
        break;
      default:
    }

    return theme;
  }
}
