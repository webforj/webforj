package com.webforj.optiondialog;

import com.webforj.Environment;

/**
 * A message box dialog.
 */
public final class MessageDialog extends OptionDialogBase<MessageDialog> {
  /**
   * The type of the buttons in the message box.
   */
  public enum MessageType {
    OK(0), OK_CANCEL(1), ABORT_RETRY_IGNORE(2), YES_NO_CANCEL(3), YES_NO(4), RETRY_CANCEL(
        5), CUSTOM(7);

    private final int type;

    MessageType(int value) {
      this.type = value;
    }

    /**
     * Gets the type of the button.
     *
     * @return the type of the button
     */
    public int getValue() {
      return type;
    }
  }

  /**
   * The type of the icon in the message box.
   */
  public enum IconType {
    NONE(0), STOP(16), QUESTION(32), EXCLAMATION(48), INFORMATION(64);

    private final int type;

    IconType(int value) {
      this.type = value;
    }

    /**
     * Gets the type of the icon.
     *
     * @return the type of the icon
     */
    public int getValue() {
      return type;
    }
  }

  /**
   * The default button in the message box.
   */
  public enum DefaultButton {
    FIRST(0), SECOND(256), THIRD(512), NONE(65536);

    private final int value;

    DefaultButton(int value) {
      this.value = value;
    }

    /**
     * Gets the type of the default button.
     *
     * @return the type of the default button
     */
    public int getValue() {
      return value;
    }
  }

  /**
   * The result of the message box.
   */
  public enum Result {
    OK(1), CANCEL(2), ABORT(3), RETRY(4), IGNORE(5), YES(6), NO(7), FIRST_CUSTOM_BUTTON(
        1), SECOND_CUSTOM_BUTTON(2), THIRD_CUSTOM_BUTTON(3), TIMEOUT(-1);

    private final int value;

    Result(int value) {
      this.value = value;
    }

    /**
     * Gets the value of the result.
     *
     * @return the value of the result
     */
    public int getValue() {
      return value;
    }
  }

  private Object title = "MessageDialog";
  private Object message = "";
  private MessageType messageType = MessageType.OK;
  private IconType iconType = IconType.NONE;
  private DefaultButton defaultButton = DefaultButton.FIRST;
  private boolean rawText = false;
  private String firstButtonText = "";
  private String secondButtonText = "";
  private String thirdButtonText = "";
  private int timeout = 0;

  MessageDialog() {
    // Prevent instantiation from outside
  }

  /**
   * Sets the title of the message box.
   *
   * @param title the title to set
   * @return the message box
   */
  public MessageDialog setTitle(Object title) {
    this.title = title;
    return this;
  }

  /**
   * Gets the title of the message box.
   */
  public Object getTitle() {
    return title;
  }

  /**
   * Sets the message of the message box.
   *
   * @param message the message to set
   * @return the message box
   */
  public MessageDialog setMessage(Object message) {
    this.message = message;
    return this;
  }

  /**
   * Gets the message of the message box.
   */
  public Object getMessage() {
    return message;
  }

  /**
   * Sets the type of message.
   *
   * @param type the type to set
   * @return the message box
   */
  public MessageDialog setMessageType(MessageType type) {
    this.messageType = type;
    return this;
  }

  /**
   * Gets the type of the message.
   *
   * @return the type of the message box
   */
  public MessageType getMessageType() {
    return messageType;
  }

  /**
   * Sets the type of the icon in the message box.
   *
   * @param type the iconType to set
   * @return the message box
   */
  public MessageDialog setIconType(IconType type) {
    this.iconType = type;
    return this;
  }

  /**
   * Gets the type of the icon in the message box.
   *
   * @return the iconType of the message box
   */
  public IconType getIconType() {
    return iconType;
  }

  /**
   * Sets the default button in the message box.
   *
   * @param defaultButton the defaultButton to set
   * @return the message box
   */
  public MessageDialog setDefaultButton(DefaultButton defaultButton) {
    this.defaultButton = defaultButton;
    return this;
  }

  /**
   * Gets the default button in the message box.
   *
   * @return the defaultButton of the message box
   */
  public DefaultButton getDefaultButton() {
    return defaultButton;
  }

  /**
   * Disables/Enables HTML processing for the title and the message.
   *
   * @param rawText when true, HTML processing is disabled, otherwise it is enabled
   * @return the message box
   */
  public MessageDialog setRawText(boolean rawText) {
    this.rawText = rawText;
    return this;
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
   * Sets the text of the first button.
   *
   * @param firstButtonText the text of the first button
   * @return the message box
   */
  public MessageDialog setFirstButtonText(String firstButtonText) {
    this.firstButtonText = firstButtonText == null ? "" : firstButtonText;
    return this;
  }

  /**
   * Gets the text of the first button.
   *
   * @return the text of the first button
   */
  public String getFirstButtonText() {
    return firstButtonText;
  }

  /**
   * Sets the text of the second button.
   *
   * @param secondButtonText the text of the second button
   * @return the message box
   */
  public MessageDialog setSecondButtonText(String secondButtonText) {
    this.secondButtonText = secondButtonText == null ? "" : secondButtonText;
    return this;
  }

  /**
   * Gets the text of the second button.
   *
   * @return the text of the second button
   */
  public String getSecondButtonText() {
    return secondButtonText;
  }

  /**
   * Sets the text of the third button.
   *
   * @param thirdButtonText the text of the third button
   * @return the message box
   */
  public MessageDialog setThirdButtonText(String thirdButtonText) {
    this.thirdButtonText = thirdButtonText == null ? "" : thirdButtonText;
    return this;
  }

  /**
   * Gets the text of the third button.
   *
   * @return the text of the third button
   */
  public String getThirdButtonText() {
    return thirdButtonText;
  }

  /**
   * Sets buttons texts.
   *
   * @param firstButtonText the text of the first button
   * @param secondButtonText the text of the second button
   * @param thirdButtonText the text of the third button
   *
   * @return the message box
   */
  public MessageDialog setButtonText(String firstButtonText, String secondButtonText,
      String thirdButtonText) {
    setFirstButtonText(firstButtonText);
    setSecondButtonText(secondButtonText);
    setThirdButtonText(thirdButtonText);
    return this;
  }

  /**
   * Sets buttons texts.
   *
   * @param firstButtonText the text of the first button
   * @param secondButtonText the text of the second button
   *
   * @return the message box
   */
  public MessageDialog setButtonText(String firstButtonText, String secondButtonText) {
    setFirstButtonText(firstButtonText);
    setSecondButtonText(secondButtonText);
    return this;
  }

  /**
   * Sets buttons texts.
   *
   * @param firstButtonText the text of the first button
   *
   * @return the message box
   */
  public MessageDialog setButtonText(String firstButtonText) {
    setFirstButtonText(firstButtonText);
    return this;
  }

  /**
   * Sets the timeout of the message box.
   *
   * @param timeout the timeout of the message box
   * @return the message box
   */
  public MessageDialog setTimeout(int timeout) {
    if (timeout < 0) {
      throw new IllegalArgumentException("Timeout must be greater than or equal to 0");
    }

    this.timeout = timeout;
    return this;
  }

  /**
   * Gets the timeout of the message box.
   *
   * @return the timeout of the message box
   */
  public int getTimeout() {
    return timeout;
  }

  /**
   * Shows the message box.
   *
   * @return the result of the message box
   */
  public int show() {
    return Environment.getCurrent().getWeforjHelper().msgbox(this);
  }

  /**
   * Alias to {@link #show()}.
   *
   * @return the result of the message box
   */
  public int open() {
    return show();
  }
}
