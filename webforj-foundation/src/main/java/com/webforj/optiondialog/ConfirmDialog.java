package com.webforj.optiondialog;

import com.basis.bbj.proxies.SysGuiProxyConstants;
import com.google.gson.Gson;
import com.webforj.Environment;
import com.webforj.component.button.ButtonTheme;

/**
 * Represents a confirm dialog.
 *
 * <p>
 * The confirm dialog is a dialog that allows the user to choose one of a set of max 3 options. The
 * dialog is modal and will block application execution until the user dismisses the dialog or the
 * dialog is closed by timeout.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
public final class ConfirmDialog extends DwcMsgBox<ConfirmDialog> {
  static final String DEFAULT_TITLE = "Select an Option";

  /**
   * Enum for the of option buttons that appear at the bottom of the dialog box.
   */
  public enum OptionType {
    /**
     * OK button.
     */
    OK(SysGuiProxyConstants.MSGBOX_BUTTONS_OK),
    /**
     * OK and Cancel buttons.
     */
    OK_CANCEL(SysGuiProxyConstants.MSGBOX_BUTTONS_OK_CANCEL),
    /**
     * Abort, Retry, and Ignore buttons.
     */
    ABORT_RETRY_IGNORE(SysGuiProxyConstants.MSGBOX_BUTTONS_ABORT_RETRY_IGNORE),
    /**
     * Yes, No, and Cancel buttons.
     */
    YES_NO_CANCEL(SysGuiProxyConstants.MSGBOX_BUTTONS_YES_NO_CANCEL),
    /**
     * Yes and No buttons.
     */
    YES_NO(SysGuiProxyConstants.MSGBOX_BUTTONS_YES_NO),
    /**
     * Retry and Cancel buttons.
     */
    RETRY_CANCEL(SysGuiProxyConstants.MSGBOX_BUTTONS_RETRY_CANCEL),
    /**
     * Custom buttons.
     *
     * <p>
     * The number of buttons is determined by the number of button texts set.
     * </p>
     */
    CUSTOM(SysGuiProxyConstants.MSGBOX_BUTTONS_CUSTOM);

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
   * The supported buttons in the confirm dialog.
   */
  public enum Button {
    /**
     * The first button is the default button.
     */
    FIRST(SysGuiProxyConstants.MSGBOX_DEFAULT_FIRST),
    /**
     * The second button is the default button.
     */
    SECOND(SysGuiProxyConstants.MSGBOX_DEFAULT_SECOND),
    /**
     * The third button is the default button.
     */
    THIRD(SysGuiProxyConstants.MSGBOX_DEFAULT_THIRD);

    private final int value;

    Button(int value) {
      this.value = value;
    }

    /**
     * Gets the value of the button.
     *
     * @return the value of the button
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
    OK(SysGuiProxyConstants.MSGBOX_RETURN_OK),
    /**
     * The Cancel button was clicked.
     */
    CANCEL(SysGuiProxyConstants.MSGBOX_RETURN_CANCEL),
    /**
     * The Abort button was clicked.
     */
    ABORT(SysGuiProxyConstants.MSGBOX_RETURN_ABORT),
    /**
     * The Retry button was clicked.
     */
    RETRY(SysGuiProxyConstants.MSGBOX_RETURN_RETRY),
    /**
     * The Ignore button was clicked.
     */
    IGNORE(SysGuiProxyConstants.MSGBOX_RETURN_IGNORE),
    /**
     * The Yes button was clicked.
     */
    YES(SysGuiProxyConstants.MSGBOX_RETURN_YES),
    /**
     * The No button was clicked.
     */
    NO(SysGuiProxyConstants.MSGBOX_RETURN_NO),
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
    UNKNOWN(-1000);

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

  private OptionType optionType = OptionType.OK;
  private Button defaultButton = Button.FIRST;
  private Object firstButtonText = "";
  private Object secondButtonText = "";
  private Object thirdButtonText = "";

  /**
   * Creates a new instance of the confirm dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param optionType the type of buttons to display in the dialog
   * @param messageType The type of dialog style.
   */
  public ConfirmDialog(Object message, String title, OptionType optionType,
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
  public ConfirmDialog(Object message, String title, OptionType optionType) {
    this(message, title, optionType, MessageType.PLAIN);
  }

  /**
   * Creates a new instance of the confirm dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   */
  public ConfirmDialog(Object message, String title) {
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
    this("");
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
   * Sets the default button in the confirm dialog.
   *
   * @param defaultButton the defaultButton to set
   * @return the message dialog
   */
  public ConfirmDialog setDefaultButton(Button defaultButton) {
    this.defaultButton = defaultButton;
    return this;
  }

  /**
   * Gets the default button in the confirm dialog.
   *
   * @return the defaultButton of the message dialog
   */
  public Button getDefaultButton() {
    return defaultButton;
  }

  /**
   * Sets the text of the first button.
   *
   * @param firstButtonText the text of the first button
   * @return the message dialog
   */
  public ConfirmDialog setFirstButtonText(Object firstButtonText) {
    this.firstButtonText = firstButtonText == null ? "" : firstButtonText;
    return this;
  }

  /**
   * Gets the text of the first button.
   *
   * @return the text of the first button
   */
  public Object getFirstButtonText() {
    return firstButtonText;
  }

  /**
   * Sets the text of the second button.
   *
   * @param secondButtonText the text of the second button
   * @return the message dialog
   */
  public ConfirmDialog setSecondButtonText(Object secondButtonText) {
    this.secondButtonText = secondButtonText == null ? "" : secondButtonText;
    return this;
  }

  /**
   * Gets the text of the second button.
   *
   * @return the text of the second button
   */
  public Object getSecondButtonText() {
    return secondButtonText;
  }

  /**
   * Sets the text of the third button.
   *
   * @param thirdButtonText the text of the third button
   * @return the message dialog
   */
  public ConfirmDialog setThirdButtonText(Object thirdButtonText) {
    this.thirdButtonText = thirdButtonText == null ? "" : thirdButtonText;
    return this;
  }

  /**
   * Gets the text of the third button.
   *
   * @return the text of the third button
   */
  public Object getThirdButtonText() {
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
   * Sets the theme of the first button.
   *
   * @param theme the theme of the first button
   * @return the message dialog
   */
  public ConfirmDialog setFirstButtonTheme(ButtonTheme theme) {
    setAttribute("button-0-theme", new Gson().toJson(theme));
    return this;
  }

  /**
   * Sets the theme of the second button.
   *
   * @param theme the theme of the second button
   * @return the message dialog
   */
  public ConfirmDialog setSecondButtonTheme(ButtonTheme theme) {
    setAttribute("button-1-theme", new Gson().toJson(theme));
    return this;
  }

  /**
   * Sets the theme of the third button.
   *
   * @param theme the theme of the third button
   * @return the message dialog
   */
  public ConfirmDialog setThirdButtonTheme(ButtonTheme theme) {
    setAttribute("button-2-theme", new Gson().toJson(theme));
    return this;
  }

  /**
   * Sets the theme of the button.
   *
   * @param button the button
   * @param theme the theme of the button
   * @return the message dialog
   */
  public ConfirmDialog setButtonTheme(Button button, ButtonTheme theme) {
    switch (button) {
      case FIRST:
        setFirstButtonTheme(theme);
        break;
      case SECOND:
        setSecondButtonTheme(theme);
        break;
      case THIRD:
        setThirdButtonTheme(theme);
        break;
      default:
        break;
    }
    return this;
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
