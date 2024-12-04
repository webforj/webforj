package com.webforj.component.optiondialog;

import com.basis.bbj.proxies.SysGuiProxyConstants;
import com.google.gson.Gson;
import com.webforj.Environment;
import com.webforj.component.Theme;
import com.webforj.component.button.ButtonTheme;

/**
 * Represents an input dialog that prompts the user for input.
 *
 * <p>
 * The input dialog is a dialog that allows that prompts the user for input. The input dialog can be
 * will block application execution until the user closes the dialog or enters the input.
 * </p>
 *
 * @since 24.02
 */
public final class InputDialog extends DwcPromptMsgBox<InputDialog> {
  static final String DEFAULT_TITLE = "Input";

  /**
   * Defines the type of input field.
   */
  public enum InputType {
    /**
     * Text Input (Default).
     */
    TEXT("text"),
    /**
     * Password Input.
     */
    PASSWORD("password"),
    /**
     * Number Input.
     */
    NUMBER("number"),
    /**
     * Email Address Input.
     */
    EMAIL("email"),
    /**
     * URL Input.
     */
    URL("url"),
    /**
     * Search Text Input.
     */
    SEARCH("search"),
    /**
     * Date Input.
     */
    DATE("date"),
    /**
     * Time Input.
     */
    TIME("time"),
    /**
     * Local Date/Time Input.
     */
    DATETIME_LOCAL("datetime-local"),
    /**
     * Color Input.
     */
    COLOR("color");

    private final String type;

    InputType(String type) {
      this.type = type;
    }

    /**
     * Returns the value of the input type.
     *
     * @return the value of the input type.
     */
    public String getValue() {
      return type;
    }
  }

  /**
   * The supported buttons in the input dialog.
   */
  public enum Button {
    /**
     * The first button is the default button.
     */
    FIRST(SysGuiProxyConstants.MSGBOX_DEFAULT_FIRST),
    /**
     * The second button is the default button.
     */
    SECOND(SysGuiProxyConstants.MSGBOX_DEFAULT_SECOND);

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

  private InputType inputType = InputType.TEXT;
  private String defaultValue = "";
  private String firstButtonText = "";
  private String secondButtonText = "";

  /**
   * Creates a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param defaultValue the default value of the input field
   * @param messageType The type of dialog style.
   * @param inputType the type of input field
   * @param theme The theme of the dialog.
   */
  public InputDialog(Object message, String title, String defaultValue, MessageType messageType,
      InputType inputType, Theme theme) {
    setMessage(message);
    setTitle(title);
    setDefaultValue(defaultValue);
    setMessageType(messageType);
    setType(inputType);
    setTheme(theme);
    setFirstButtonTheme(detectButtonThemeFromDialogTheme(theme));
  }

  /**
   * Creates a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param defaultValue the default value of the input field
   * @param messageType The type of dialog style.
   * @param inputType the type of input field
   */
  public InputDialog(String message, String title, String defaultValue, MessageType messageType,
      InputType inputType) {
    this(message, title, defaultValue, messageType, inputType,
        detectDialogThemeFromMessageType(messageType));
  }

  /**
   * Creates a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param defaultValue the default value of the input field
   * @param messageType The type of dialog style.
   */
  public InputDialog(Object message, String title, String defaultValue, MessageType messageType) {
    this(message, title, defaultValue, messageType, InputType.TEXT,
        detectDialogThemeFromMessageType(messageType));
  }

  /**
   * Creates a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param defaultValue the default value of the input field
   * @param inputType the type of input field
   */
  public InputDialog(Object message, String title, String defaultValue, InputType inputType) {
    this(message, title, defaultValue, MessageType.PLAIN, inputType, Theme.DEFAULT);
  }

  /**
   * Creates a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param messageType The type of dialog style.
   * @param inputType the type of input field
   */
  public InputDialog(Object message, String title, MessageType messageType, InputType inputType) {
    this(message, title, "", messageType, inputType, detectDialogThemeFromMessageType(messageType));
  }

  /**
   * Creates a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param messageType The type of dialog style.
   */
  public InputDialog(Object message, String title, MessageType messageType) {
    this(message, title, "", messageType, InputType.TEXT,
        detectDialogThemeFromMessageType(messageType));
  }

  /**
   * Creates a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param inputType the type of input field
   */
  public InputDialog(Object message, String title, InputType inputType) {
    this(message, title, "", MessageType.PLAIN, inputType, Theme.DEFAULT);
  }

  /**
   * Creates a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param messageType The type of dialog style.
   * @param inputType the type of input field
   */
  public InputDialog(Object message, MessageType messageType, InputType inputType) {
    this(message, DEFAULT_TITLE, "", messageType, inputType,
        detectDialogThemeFromMessageType(messageType));
  }

  /**
   * Creates a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param messageType The type of dialog style.
   */
  public InputDialog(Object message, MessageType messageType) {
    this(message, DEFAULT_TITLE, "", messageType, InputType.TEXT,
        detectDialogThemeFromMessageType(messageType));
  }

  /**
   * Creates a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param inputType the type of input field
   */
  public InputDialog(Object message, InputType inputType) {
    this(message, DEFAULT_TITLE, "", MessageType.PLAIN, inputType, Theme.DEFAULT);
  }

  /**
   * Creates a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   * @param defaultValue the default value of the input field
   */
  public InputDialog(Object message, String title, String defaultValue) {
    this(message, title, defaultValue, MessageType.PLAIN, InputType.TEXT, Theme.DEFAULT);
  }

  /**
   * Creates a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   * @param title the title of the dialog
   */
  public InputDialog(Object message, String title) {
    this(message, title, "", MessageType.PLAIN, InputType.TEXT, Theme.DEFAULT);
  }

  /**
   * Creates a new instance of the input dialog.
   *
   * @param message the message to display in the dialog
   */
  public InputDialog(Object message) {
    this(message, DEFAULT_TITLE, "", MessageType.PLAIN, InputType.TEXT, Theme.DEFAULT);
  }

  /**
   * Creates a new instance of the input dialog.
   */
  public InputDialog() {
    this("");
  }

  /**
   * Sets the type of input field.
   *
   * @param type the type of input field.
   * @return the dialog instance
   */
  public InputDialog setType(InputType type) {
    this.inputType = type;
    setAttribute("input-type", type.getValue());
    return this;
  }

  /**
   * Gets the type of input field.
   *
   * @return the type of input field.
   */
  public InputType getType() {
    return inputType;
  }

  /**
   * Sets the default value of the input field.
   *
   * @param value the default value of the input field.
   * @return the dialog instance
   */
  public InputDialog setDefaultValue(String value) {
    this.defaultValue = value;
    return this;
  }

  /**
   * Gets the default value of the input field.
   *
   * @return the default value of the input field.
   */
  public String getDefaultValue() {
    return defaultValue;
  }

  /**
   * Sets the text of the first button.
   *
   * @param firstButtonText the text of the first button
   * @return the dialog instance
   */
  public InputDialog setFirstButtonText(String firstButtonText) {
    this.firstButtonText = firstButtonText == null ? "" : firstButtonText;
    setAttribute("button-0-label", this.firstButtonText);
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
   * @return the dialog instance
   */
  public InputDialog setSecondButtonText(String secondButtonText) {
    this.secondButtonText = secondButtonText == null ? "" : secondButtonText;
    setAttribute("button-1-label", this.secondButtonText);
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
   * Sets the text of a button.
   *
   * @param button the button
   * @param text the text of the button
   *
   * @return the dialog instance
   */
  public InputDialog setButtonText(Button button, String text) {
    switch (button) {
      case FIRST:
        setFirstButtonText(text);
        break;
      case SECOND:
        setSecondButtonText(text);
        break;
      default:
        break;
    }

    return this;
  }

  /**
   * Sets the theme of the first button.
   *
   * @param theme the theme of the first button
   * @return the dialog instance
   */
  public InputDialog setFirstButtonTheme(ButtonTheme theme) {
    setAttribute("button-0-theme", new Gson().toJson(theme));
    return this;
  }

  /**
   * Sets the theme of the second button.
   *
   * @param theme the theme of the second button
   * @return the dialog instance
   */
  public InputDialog setSecondButtonTheme(ButtonTheme theme) {
    setAttribute("button-1-theme", new Gson().toJson(theme));
    return this;
  }

  /**
   * Sets the theme of the button.
   *
   * @param button the button
   * @param theme the theme of the button
   * @return the dialog instance
   */
  public InputDialog setButtonTheme(Button button, ButtonTheme theme) {
    switch (button) {
      case FIRST:
        setFirstButtonTheme(theme);
        break;
      case SECOND:
        setSecondButtonTheme(theme);
        break;
      default:
        break;
    }

    return this;
  }

  /**
   * Shows the input dialog.
   *
   * @return the result of the input dialog
   */
  public String show() {
    String result = Environment.getCurrent().getBridge().prompt(this);
    if (!"::CANCEL::".equals(result)) {
      return result;
    }

    return null;
  }

  /**
   * Alias to {@link #show()}.
   *
   * @return the result of the input dialog
   */
  public String open() {
    return show();
  }
}
