package com.webforj.automation.act;

/**
 * The modifier keys held while a key is pressed. None is held by default.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class KeyOptions {
  private boolean altKey;
  private boolean controlKey;
  private boolean shiftKey;
  private boolean metaKey;

  /**
   * Returns whether the alt key was held.
   *
   * @return true when held
   */
  public boolean isAltKey() {
    return altKey;
  }

  /**
   * Sets whether the alt key was held.
   *
   * @param altKey true when held
   *
   * @return this modifiers
   */
  public KeyOptions setAltKey(boolean altKey) {
    this.altKey = altKey;

    return this;
  }

  /**
   * Returns whether the control key was held.
   *
   * @return true when held
   */
  public boolean isControlKey() {
    return controlKey;
  }

  /**
   * Sets whether the control key was held.
   *
   * @param controlKey true when held
   *
   * @return this modifiers
   */
  public KeyOptions setControlKey(boolean controlKey) {
    this.controlKey = controlKey;

    return this;
  }

  /**
   * Returns whether the shift key was held.
   *
   * @return true when held
   */
  public boolean isShiftKey() {
    return shiftKey;
  }

  /**
   * Sets whether the shift key was held.
   *
   * @param shiftKey true when held
   *
   * @return this modifiers
   */
  public KeyOptions setShiftKey(boolean shiftKey) {
    this.shiftKey = shiftKey;

    return this;
  }

  /**
   * Returns whether the meta key was held, the command key on a Mac.
   *
   * @return true when held
   */
  public boolean isMetaKey() {
    return metaKey;
  }

  /**
   * Sets whether the meta key was held, the command key on a Mac.
   *
   * @param metaKey true when held
   *
   * @return this modifiers
   */
  public KeyOptions setMetaKey(boolean metaKey) {
    this.metaKey = metaKey;

    return this;
  }
}
