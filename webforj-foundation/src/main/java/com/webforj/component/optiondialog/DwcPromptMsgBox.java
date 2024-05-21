package com.webforj.component.optiondialog;

import com.basis.bbj.proxies.SysGuiProxyConstants;
import com.webforj.component.Theme;
import com.webforj.component.button.ButtonTheme;

/**
 * The base class for DWC {@code prompt} and {@code msgbox} dialogs.
 *
 * @param <T> the type of the dialog
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
class DwcPromptMsgBox<T extends ThemedDialog<T>> extends ThemedDialog<T> {

  /**
   * Defines the style of the dialog message.
   */
  public enum MessageType {
    /**
     * Plain.
     */
    PLAIN(SysGuiProxyConstants.MSGBOX_ICON_NONE),
    /**
     * Error.
     */
    ERROR(SysGuiProxyConstants.MSGBOX_ICON_STOP),
    /**
     * Question.
     */
    QUESTION(SysGuiProxyConstants.MSGBOX_ICON_QUESTION),
    /**
     * Warning.
     */
    WARNING(SysGuiProxyConstants.MSGBOX_ICON_EXCLAMATION),
    /**
     * Information.
     */
    INFO(SysGuiProxyConstants.MSGBOX_ICON_INFORMATION);

    private final int type;

    MessageType(int value) {
      this.type = value;
    }

    /**
     * Gets the style of the dialog message.
     *
     * @return the style of the dialog message
     */
    public int getValue() {
      return type;
    }
  }

  private String title = "";
  private Object message = "";
  private MessageType messageType = MessageType.PLAIN;
  private int timeout = 0;

  /**
   * Sets the title of the dialog.
   *
   * @param title the title to set
   * @return the dialog
   */
  public T setTitle(String title) {
    this.title = title;
    return getSelf();
  }

  /**
   * Gets the title of the dialog.
   *
   * @return the title of the dialog
   */
  public String getTitle() {
    return title;
  }

  /**
   * Sets the message of the dialog.
   *
   * @param message the message to set
   * @return the dialog
   */
  public T setMessage(Object message) {
    this.message = message;
    return getSelf();
  }

  /**
   * Gets the message of the dialog.
   *
   * @return the message of the dialog
   */
  public Object getMessage() {
    return message;
  }

  /**
   * Sets the message type of the dialog.
   *
   * @param type the message type to set
   * @return the dialog
   */
  public T setMessageType(MessageType type) {
    this.messageType = type;
    return getSelf();
  }

  /**
   * Gets the type of the message.
   *
   * @return the type of the dialog
   */
  public MessageType getMessageType() {
    return messageType;
  }

  /**
   * Sets the timeout of the dialog.
   *
   * @param timeout the timeout of the dialog
   * @return the dialog
   */
  public T setTimeout(int timeout) {
    if (timeout < 0) {
      throw new IllegalArgumentException("Timeout must be greater than or equal to 0");
    }

    this.timeout = timeout;
    return getSelf();
  }

  /**
   * Gets the timeout of the dialog.
   *
   * @return the timeout of the dialog
   */
  public int getTimeout() {
    return timeout;
  }

  /**
   * Detects the button theme based on the dialog theme.
   *
   * @param theme The theme of the dialog.
   * @return The corresponding button theme.
   */
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

  /**
   * Detects the dialog theme based on the message type.
   *
   * @param messageType The type of message.
   * @return The corresponding dialog theme.
   */
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
