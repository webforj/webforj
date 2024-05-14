package com.webforj.optiondialog;

import com.basis.bbj.proxies.SysGuiProxyConstants;

/**
 * The base class for all message boxes.
 *
 * @param <T> the type of the dialog
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
class DwcMsgBox<T extends Dialog<T>> extends Dialog<T> {

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
  private boolean rawText = false;
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
}
