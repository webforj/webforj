package com.webforj.optiondialog;

import com.webforj.Environment;

/**
 * The base class for confirmation dialogs.
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
public final class ConfirmDialog extends OptionDialogBase<ConfirmDialog> {
  static final String DEFAULT_TITLE = "Select an Option";

  /**
   * Enum for the of option buttons that appear at the bottom of the dialog box.
   */
  public enum OptionType {
    /**
     * OK button.
     */
    OK(0),
    /**
     * OK and Cancel buttons.
     */
    OK_CANCEL(1),
    /**
     * Abort, Retry, and Ignore buttons.
     */
    ABORT_RETRY_IGNORE(2),
    /**
     * Yes, No, and Cancel buttons.
     */
    YES_NO_CANCEL(3),
    /**
     * Yes and No buttons.
     */
    YES_NO(4),
    /**
     * Retry and Cancel buttons.
     */
    RETRY_CANCEL(5),
    /**
     * Custom buttons.
     *
     * <p>
     * The number of buttons is determined by the number of button texts set.
     * </p>
     */
    CUSTOM(7);

    private final int type;

    OptionType(int value) {
      this.type = value;
    }

    /**
     * Gets the type of the option.
     *
     * @return the type of the option
     */
    public int getValue() {
      return type;
    }
  }

  /**
   * Defines the style of the message.
   */
  public enum MessageType {
    /**
     * No icon.
     */
    PLAIN(0),
    /**
     * Error icon.
     */
    ERROR(16),
    /**
     * Question icon.
     */
    QUESTION(32),
    /**
     * Warning icon.
     */
    WARNING(48),
    /**
     * Information icon.
     */
    INFORMATION(64);

    private final int type;

    MessageType(int value) {
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
   * The default button in the confirm dialog.
   */
  public enum DefaultButton {
    /**
     * The first button is the default button.
     */
    FIRST(0),
    /**
     * The second button is the default button.
     */
    SECOND(256),
    /**
     * The third button is the default button.
     */
    THIRD(512),
    /**
     * The fourth button is the default button.
     */
    NONE(65536);

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
   * The result of the confirm dialog.
   */
  public enum Result {
    /**
     * The OK button was clicked.
     */
    OK(1),
    /**
     * The Cancel button was clicked.
     */
    CANCEL(2),
    /**
     * The Abort button was clicked.
     */
    ABORT(3),
    /**
     * The Retry button was clicked.
     */
    RETRY(4),
    /**
     * The Ignore button was clicked.
     */
    IGNORE(5),
    /**
     * The Yes button was clicked.
     */
    YES(6),
    /**
     * The No button was clicked.
     */
    NO(7),
    /**
     * The first custom button was clicked.
     */
    FIRST_CUSTOM_BUTTON(1),
    /**
     * The second custom button was clicked.
     */
    SECOND_CUSTOM_BUTTON(2),
    /**
     * The third custom button was clicked.
     */
    THIRD_CUSTOM_BUTTON(3),
    /**
     * A timeout occurred.
     */
    TIMEOUT(-1),

    /**
     * Unknown result.
     */
    UNKNOWN(-2);

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

  private Object title = DEFAULT_TITLE;
  private Object message = "";
  private OptionType optionType = OptionType.OK;
  private MessageType messageType = MessageType.PLAIN;
  private DefaultButton defaultButton = DefaultButton.FIRST;
  private boolean rawText = false;
  private String firstButtonText = "";
  private String secondButtonText = "";
  private String thirdButtonText = "";
  private int timeout = 0;

  /**
   * Creates a new instance of the confirm dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param optionType the type of buttons to display in the dialog
   * @param messageType The type of dialog style.
   */
  public ConfirmDialog(Object message, Object title, OptionType optionType,
      MessageType messageType) {
    setMessage(message);
    setTitle(title);
    setOptionType(optionType);
    setMessageType(messageType);
  }

  /**
   * Creates a new instance of the confirm dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param optionType the type of buttons to display in the dialog
   */
  public ConfirmDialog(Object message, Object title, OptionType optionType) {
    this(message, title, optionType, MessageType.PLAIN);
  }

  /**
   * Creates a new instance of the confirm dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   */
  public ConfirmDialog(Object message, Object title) {
    this(message, title, OptionType.OK, MessageType.PLAIN);
  }

  /**
   * Creates a new instance of the confirm dialog.
   *
   * @param message the message to display in the dialog
   */
  public ConfirmDialog(Object message) {
    this(message, DEFAULT_TITLE, OptionType.OK, MessageType.PLAIN);
  }

  /**
   * Creates a new instance of the confirm dialog.
   */
  public ConfirmDialog() {
    this("", DEFAULT_TITLE, OptionType.OK, MessageType.PLAIN);
  }

  /**
   * Sets the title of the confirm dialog.
   *
   * @param title the title to set
   * @return the message dialog
   */
  public ConfirmDialog setTitle(Object title) {
    this.title = title;
    return this;
  }

  /**
   * Gets the title of the confirm dialog.
   *
   * @return the title of the message dialog
   */
  public Object getTitle() {
    return title;
  }

  /**
   * Sets the message of the confirm dialog.
   *
   * @param message the message to set
   * @return the message dialog
   */
  public ConfirmDialog setMessage(Object message) {
    this.message = message;
    return this;
  }

  /**
   * Gets the message of the confirm dialog.
   *
   * @return the message of the message dialog
   */
  public Object getMessage() {
    return message;
  }

  /**
   * Sets the option type of the confirm dialog.
   *
   * @param type the option type to set.
   * @return the message dialog
   */
  public ConfirmDialog setOptionType(OptionType type) {
    this.optionType = type;
    return this;
  }

  /**
   * Gets the option type of the confirm dialog.
   *
   * @return the option type of the message dialog
   */
  public OptionType getOptionType() {
    return optionType;
  }

  /**
   * Sets the message type of the confirm dialog.
   *
   * @param type the message type to set
   * @return the message dialog
   */
  public ConfirmDialog setMessageType(MessageType type) {
    this.messageType = type;
    return this;
  }

  /**
   * Gets the type of the message.
   *
   * @return the type of the message dialog
   */
  public MessageType getMessageType() {
    return messageType;
  }

  /**
   * Sets the default button in the confirm dialog.
   *
   * @param defaultButton the defaultButton to set
   * @return the message dialog
   */
  public ConfirmDialog setDefaultButton(DefaultButton defaultButton) {
    this.defaultButton = defaultButton;
    return this;
  }

  /**
   * Gets the default button in the confirm dialog.
   *
   * @return the defaultButton of the message dialog
   */
  public DefaultButton getDefaultButton() {
    return defaultButton;
  }

  /**
   * Disables/Enables HTML processing for the title and the message.
   *
   * @param rawText when true, HTML processing is disabled, otherwise it is enabled
   * @return the message dialog
   */
  public ConfirmDialog setRawText(boolean rawText) {
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
   * @return the message dialog
   */
  public ConfirmDialog setFirstButtonText(String firstButtonText) {
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
   * @return the message dialog
   */
  public ConfirmDialog setSecondButtonText(String secondButtonText) {
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
   * @return the message dialog
   */
  public ConfirmDialog setThirdButtonText(String thirdButtonText) {
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
   * @return the message dialog
   */
  public ConfirmDialog setButtonText(String firstButtonText, String secondButtonText,
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
   * @return the message dialog
   */
  public ConfirmDialog setButtonText(String firstButtonText, String secondButtonText) {
    setFirstButtonText(firstButtonText);
    setSecondButtonText(secondButtonText);
    return this;
  }

  /**
   * Sets buttons texts.
   *
   * @param firstButtonText the text of the first button
   *
   * @return the message dialog
   */
  public ConfirmDialog setButtonText(String firstButtonText) {
    setFirstButtonText(firstButtonText);
    return this;
  }

  /**
   * Sets the timeout of the confirm dialog.
   *
   * @param timeout the timeout of the message dialog
   * @return the message dialog
   */
  public ConfirmDialog setTimeout(int timeout) {
    if (timeout < 0) {
      throw new IllegalArgumentException("Timeout must be greater than or equal to 0");
    }

    this.timeout = timeout;
    return this;
  }

  /**
   * Gets the timeout of the confirm dialog.
   *
   * @return the timeout of the message dialog
   */
  public int getTimeout() {
    return timeout;
  }

  /**
   * Shows the confirm dialog.
   *
   * @return the result of the message dialog
   */
  public Result show() {
    int result = Environment.getCurrent().getWeforjHelper().msgbox(this);
    return mapResult(result);
  }

  /**
   * Alias to {@link #show()}.
   *
   * @return the result of the message dialog
   */
  public Result open() {
    return show();
  }

  Result mapResult(int result) {
    Result endResult = Result.UNKNOWN;

    Result[] values = Result.values();
    for (Result r : values) {
      if (r.getValue() == result) {
        endResult = r;
        break;
      }
    }

    if (getOptionType().equals(OptionType.CUSTOM)) {
      switch (endResult) {
        case OK:
          endResult = Result.FIRST_CUSTOM_BUTTON;
          break;
        case CANCEL:
          endResult = Result.SECOND_CUSTOM_BUTTON;
          break;

        case ABORT:
          endResult = Result.THIRD_CUSTOM_BUTTON;
          break;

        default:
          break;
      }
    }

    return endResult;
  }
}
