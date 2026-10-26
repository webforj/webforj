package com.webforj.automation.act;

import com.webforj.component.Component;

/**
 * The details of a pointer gesture, the button pressed, where it happened and which modifier keys
 * were held. Every detail defaults to a plain primary click at the origin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class MouseOptions {
  private int button;
  private int screenX;
  private int screenY;
  private int positionX;
  private int positionY;
  private int clickCount = 1;
  private boolean altKey;
  private boolean controlKey;
  private boolean shiftKey;
  private boolean metaKey;
  private Component originalComponent;

  /**
   * Returns the pressed button, zero for the primary button.
   *
   * @return the button
   */
  public int getButton() {
    return button;
  }

  /**
   * Sets the pressed button, zero for the primary button.
   *
   * @param button the button
   *
   * @return this options
   */
  public MouseOptions setButton(int button) {
    this.button = button;

    return this;
  }

  /**
   * Returns the horizontal position on the screen.
   *
   * @return the screen x
   */
  public int getScreenX() {
    return screenX;
  }

  /**
   * Sets the horizontal position on the screen.
   *
   * @param screenX the screen x
   *
   * @return this options
   */
  public MouseOptions setScreenX(int screenX) {
    this.screenX = screenX;

    return this;
  }

  /**
   * Returns the vertical position on the screen.
   *
   * @return the screen y
   */
  public int getScreenY() {
    return screenY;
  }

  /**
   * Sets the vertical position on the screen.
   *
   * @param screenY the screen y
   *
   * @return this options
   */
  public MouseOptions setScreenY(int screenY) {
    this.screenY = screenY;

    return this;
  }

  /**
   * Returns the horizontal position inside the component.
   *
   * @return the x
   */
  public int getX() {
    return positionX;
  }

  /**
   * Sets the horizontal position inside the component.
   *
   * @param x the x
   *
   * @return this options
   */
  public MouseOptions setX(int x) {
    this.positionX = x;

    return this;
  }

  /**
   * Returns the vertical position inside the component.
   *
   * @return the y
   */
  public int getY() {
    return positionY;
  }

  /**
   * Sets the vertical position inside the component.
   *
   * @param y the y
   *
   * @return this options
   */
  public MouseOptions setY(int y) {
    this.positionY = y;

    return this;
  }

  /**
   * Returns the number of consecutive clicks, one for a single click.
   *
   * @return the click count
   */
  public int getClickCount() {
    return clickCount;
  }

  /**
   * Sets the number of consecutive clicks, one for a single click.
   *
   * @param clickCount the click count
   *
   * @return this options
   */
  public MouseOptions setClickCount(int clickCount) {
    this.clickCount = clickCount;

    return this;
  }

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
   * @return this options
   */
  public MouseOptions setAltKey(boolean altKey) {
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
   * @return this options
   */
  public MouseOptions setControlKey(boolean controlKey) {
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
   * @return this options
   */
  public MouseOptions setShiftKey(boolean shiftKey) {
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
   * @return this options
   */
  public MouseOptions setMetaKey(boolean metaKey) {
    this.metaKey = metaKey;

    return this;
  }

  /**
   * Returns the component the pointer was on when the event reached the component.
   *
   * @return the original component, or null when the event's own component is meant
   */
  public Component getOriginalComponent() {
    return originalComponent;
  }

  /**
   * Sets the component the pointer was on when the event reached the component, for a click that
   * reaches a window from a component inside it.
   *
   * @param originalComponent the original component
   *
   * @return this options
   */
  public MouseOptions setOriginalComponent(Component originalComponent) {
    this.originalComponent = originalComponent;
    return this;
  }
}
