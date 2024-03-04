package com.webforj.component.event;

import com.webforj.component.Component;
import java.util.Map;

/**
 * A Keypress event is fired when a user presses a specific key on the keyboard while an element has
 * focus.
 *
 * <p>
 * This event is commonly used to capture and respond to user keyboard interactions. When a Keypress
 * event is triggered,the key that was pressed can be determine using the key code, and a specific
 * action or actions can be performed based on the user's input. For example, it can be used to
 * validate user input in form fields, implement keyboard shortcuts, or trigger certain actions
 * based on specific key combinations.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public class KeypressEvent extends ComponentEvent<Component> {

  // @formatter:off
  /**
   * A list of keys that are reported by the KeypressEvent.
   */
  public enum Key {
    BACKSPACE("Backspace", 8),
    TAB("Tab", 9),
    CLEAR("Clear", 12),
    SHIFT("Shift", 16),
    CONTROL("Control", 17),
    ALT("Alt", 18),
    PAUSE("Pause", 19),
    ENTER("Enter", 13),
    ESCAPE("Escape", 27),
    CAPS_LOCK("CapsLock", 20),
    DELETE("Delete", 127),
    NUM_LOCK("NumLock", 144),
    SCROLL_LOCK("ScrollLock", 145),
    PRINT_SCREEN("PrintScreen", 154),
    COMMAND("Command", 157),
    UP_ARROW("UpArrow", 301),
    DOWN_ARROW("DownArrow", 302),
    RIGHT_ARROW("RightArrow", 303),
    LEFT_ARROW("LeftArrow", 304),
    PAGE_UP("PageUp", 305),
    PAGE_DOWN("PageDown", 306),
    HOME("Home", 307),
    END("End", 308),
    INSERT("Insert", 312),
    BACK_TAB("BackTab", 315),
    KEYPAD_0("Keypad0", 318),
    KEYPAD_1("Keypad1", 319),
    KEYPAD_2("Keypad2", 320),
    KEYPAD_3("Keypad3", 321),
    KEYPAD_4("Keypad4", 322),
    KEYPAD_5("Keypad5", 323),
    KEYPAD_6("Keypad6", 324),
    KEYPAD_7("Keypad7", 325),
    KEYPAD_8("Keypad8", 326),
    KEYPAD_9("Keypad9", 327),
    F1("F1", 331),
    F2("F2", 332),
    F3("F3", 333),
    F4("F4", 334),
    F5("F5", 335),
    F6("F6", 336),
    F7("F7", 337),
    F8("F8", 338),
    F9("F9", 339),
    F10("F10", 340),
    F11("F11", 341),
    F12("F12", 342),
    F13("F13", 343),
    F14("F14", 344),
    F15("F15", 345),
    F16("F16", 346),
    F17("F17", 507),
    F18("F18", 348),
    F19("F19", 349),
    F20("F20", 350),
    F21("F21", 351),
    F22("F22", 352),
    F23("F23", 353),
    F24("F24", 354),
    KEYPAD_ASTERISK("KeypadAsterisk", 372),
    KEYPAD_MINUS("KeypadMinus", 373),
    KEYPAD_PLUS("KeypadPlus", 374),
    KEYPAD_SLASH("KeypadSlash", 375);

    private final String name;
    private final int code;

    Key(String name, int code) {
      this.name = name;
      this.code = code;
    }

    /**
     * Returns the integer value of the key code.
     *
     * @return the integer value of the key code
     */
    public int getCode() {
      return code;
    }

    /**
     * Alias for {@link #getCode()}.
     *
     * @return the integer value of the key code
     */
    public int getValue() {
      return getCode();
    }

    /**
     * Returns the name of the key code.
     *
     * @return the name of the key code
     */
    public String getName() {
      return name;
    }

    /**
     * Returns the key code that matches the given integer value.
     *
     * @param value the integer value of the key code
     * @return the key code that matches the given integer value, or null if no key code matches
     */
    public static Key fromCode(int value) {
      for (Key keyCode : Key.values()) {
        if (keyCode.getCode() == value) {
          return keyCode;
        }
      }

      return null;
    }

    /**
     * Returns the key code that matches the given name.
     *
     * @param name the name of the key code
     * @return the key code that matches the given name, or null if no key code matches
     */
    public static Key fromName(String name) {
      for (Key keyCode : Key.values()) {
        if (keyCode.getName().toLowerCase().equals(name)) {
          return keyCode;
        }
      }

      return null;
    }
  }
  // @formatter:on

  /**
   * Constructs a new KeypressEvent.
   *
   * @param component the component which fired the event
   * @param payload the payload of the event
   */
  public KeypressEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Returns the key that was pressed.
   *
   * @return the code of the pressed key
   */
  public Key getKeyCode() {
    int code = (int) this.getEventMap().get("keyCode");
    return Key.fromCode(code);
  }

  /**
   * Returns whether or not the alt key was pressed when the event happened.
   *
   * @return A boolean representing whether alt was pressed.
   */
  public boolean isAltKey() {
    return (boolean) this.getEventMap().get("altKey");
  }

  /**
   * Returns whether or not the command key was pressed when the event happened.
   *
   * @return A boolean representing whether cmd was pressed.
   */
  public boolean isCmdKey() {
    return (boolean) this.getEventMap().get("cmdKey");
  }

  /**
   * Returns whether or not the control key was pressed when the event happened.
   *
   * @return A boolean representing whether ctrl was pressed.
   */
  public boolean isControlKey() {
    return (boolean) this.getEventMap().get("controlKey");
  }

  /**
   * Returns whether or not the shift key was pressed when the event happened.
   *
   * @return A boolean representing whether shift was pressed.
   */
  public boolean isShiftKey() {
    return (boolean) this.getEventMap().get("shiftKey");
  }
}
