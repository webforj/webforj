package com.webforj.component.optiondialog;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.StringJoiner;

/**
 * The base class for blocking dialogs.
 *
 * @param <T> the type of the dialog
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
class Dialog<T> {
  /**
   * The dialog alignments.
   */
  public enum Alignment {
    /** The dialog will be aligned to the bottom of the screen. */
    BOTTOM,

    /** The dialog will be aligned to the center of the screen. */
    CENTER,

    /** The dialog will be aligned to the top of the screen. */
    TOP;
  }

  private Alignment alignment = Alignment.CENTER;
  private boolean blurred = false;
  private String breakpoint = "";
  private boolean fullscreen = false;
  private boolean moveable = true;
  private String maxHeight = "";
  private String maxWidth = "";
  private String horizontalPosition = "";
  private String verticalPosition = "";
  private int snapThreshold = 0;
  private boolean snapToEdge = true;
  private Map<String, String> attributes = new HashMap<>();

  /**
   * Sets the alignment of the dialog.
   */
  public T setAlignment(Alignment alignment) {
    this.alignment = alignment;
    toggleAttribute("alignment", alignment.toString().toLowerCase(), alignment != null);
    return getSelf();
  }

  /**
   * Gets the alignment of the dialog.
   *
   * @return the alignment of the dialog
   */
  public Alignment getAlignment() {
    return alignment;
  }

  /**
   * Sets whether the dialog should be blurred.
   *
   * @param blurred whether the dialog should be blurred
   */
  public T setBlurred(boolean blurred) {
    this.blurred = blurred;
    toggleAttribute("blurred", "", blurred);

    return getSelf();
  }

  /**
   * Gets whether the dialog should be blurred.
   *
   * @return whether the dialog should be blurred
   */
  public boolean isBlurred() {
    return blurred;
  }

  /**
   * Sets the breakpoint of the dialog.
   *
   * @param breakpoint the breakpoint of the dialog
   */
  public T setBreakpoint(String breakpoint) {
    this.breakpoint = breakpoint == null ? "" : breakpoint;
    toggleAttribute("breakpoint", breakpoint, breakpoint != null);
    return getSelf();
  }

  /**
   * Gets the breakpoint of the dialog.
   *
   * @return the breakpoint of the dialog
   */
  public String getBreakpoint() {
    return breakpoint;
  }

  /**
   * Sets whether the dialog should be fullscreen.
   *
   * @param fullscreen whether the dialog should be fullscreen
   */
  public T setFullscreen(boolean fullscreen) {
    this.fullscreen = fullscreen;
    toggleAttribute("fullscreen", "", fullscreen);
    return getSelf();
  }

  /**
   * Gets whether the dialog should be fullscreen.
   *
   * @return whether the dialog should be fullscreen
   */
  public boolean isFullscreen() {
    return fullscreen;
  }

  /**
   * Sets whether the dialog should be moveable.
   *
   * @param moveable whether the dialog should be moveable
   */
  public T setMoveable(boolean moveable) {
    this.moveable = moveable;
    setAttribute("moveable", String.valueOf(moveable));
    return getSelf();
  }

  /**
   * Gets whether the dialog should be moveable.
   *
   * @return whether the dialog should be moveable
   */
  public boolean isMoveable() {
    return moveable;
  }

  /**
   * Sets the x position of the message box.
   *
   * @param x the x position of the message box
   * @return the message box
   */
  public T setHorizontalPosition(String x) {
    this.horizontalPosition = x == null ? "" : x;
    toggleAttribute("posx", x, x != null && !x.isEmpty());
    return getSelf();
  }

  /**
   * Sets the x position of the message box in pixels.
   *
   * @param x the x position of the message box
   * @return the message box
   */
  public T setHorizontalPosition(int x) {
    return setHorizontalPosition(x + "px");
  }

  /**
   * Gets the x position of the message box.
   *
   * @return the x position of the message box
   */
  public String getHorizontalPosition() {
    return horizontalPosition;
  }

  /**
   * Sets the y position of the message box.
   *
   * @param y the y position of the message box
   * @return the message box
   */
  public T setVerticalPosition(String y) {
    this.verticalPosition = y == null ? "" : y;
    toggleAttribute("posy", y, y != null && !y.isEmpty());
    return getSelf();
  }

  /**
   * Sets the y position of the message box in pixels.
   *
   * @param y the y position of the message box
   * @return the message box
   */
  public T setVerticalPosition(int y) {
    return setVerticalPosition(y + "px");
  }

  /**
   * Gets the y position of the message box.
   *
   * @return the y position of the message box
   */
  public String getVerticalPosition() {
    return verticalPosition;
  }

  /**
   * Sets the position of the message box.
   *
   * @param x the x position of the message box
   * @param y the y position of the message box
   *
   * @return the message box
   */
  public T setPosition(String x, String y) {
    setHorizontalPosition(x);
    setVerticalPosition(y);
    return getSelf();
  }

  /**
   * Sets the position of the message box in pixels.
   *
   * @param x the x position of the message box
   * @param y the y position of the message box
   *
   * @return the message box
   */
  public T setPosition(int x, int y) {
    setHorizontalPosition(x);
    setVerticalPosition(y);
    return getSelf();
  }

  /**
   * Sets the position of the message box.
   *
   * @param x the x position of the message box
   * @return the message box
   */
  public T setPosition(String x) {
    setHorizontalPosition(x);
    return getSelf();
  }

  /**
   * Sets the position of the message box in pixels.
   *
   * @param x the x position of the message box
   * @return the message box
   */
  public T setPosition(int x) {
    setHorizontalPosition(x);
    return getSelf();
  }

  /**
   * Sets the width of the message box.
   *
   * @param width the width of the message box
   * @return the message box
   */
  public T setMaxWidth(String width) {
    this.maxWidth = width;
    toggleAttribute("max-width", width, width != null && !width.isEmpty());
    return getSelf();
  }

  /**
   * Sets the width of the message box in pixels.
   *
   * @param width the width of the message box
   * @return the message box
   */
  public T setMaxWidth(int width) {
    return setMaxWidth(width + "px");
  }

  /**
   * Gets the width of the message box.
   *
   * @return the width of the message box
   */
  public String getMaxWidth() {
    return maxWidth;
  }

  /**
   * Sets the height of the message box.
   *
   * @param height the height of the message box
   * @return the message box
   */
  public T setMaxHeight(String height) {
    this.maxHeight = height;
    toggleAttribute("max-height", height, height != null && !height.isEmpty());
    return getSelf();
  }

  /**
   * Sets the height of the message box in pixels.
   *
   * @param height the height of the message box
   * @return the message box
   */
  public T setMaxHeight(int height) {
    return setMaxHeight(height + "px");
  }

  /**
   * Gets the height of the message box.
   *
   * @return the height of the message box
   */
  public String getMaxHeight() {
    return maxHeight;
  }

  /**
   * Sets the snap threshold of the message box in pixels.
   *
   * @param snapThreshold the snap threshold of the message box
   * @return the message box
   */
  public T setSnapThreshold(int snapThreshold) {
    this.snapThreshold = snapThreshold;
    toggleAttribute("snap-threshold", snapThreshold + "px", snapThreshold > 0);
    return getSelf();
  }

  /**
   * Gets the snap threshold of the message box.
   *
   * @return the snap threshold of the message box
   */
  public int getSnapThreshold() {
    return snapThreshold;
  }

  /**
   * Sets whether the message box should snap to the edge.
   *
   * @param snapToEdge whether the message box should snap to the edge
   * @return the message box
   */
  public T setSnapToEdge(boolean snapToEdge) {
    this.snapToEdge = snapToEdge;
    setAttribute("snap-to-edge", String.valueOf(snapToEdge));
    return getSelf();
  }

  /**
   * Gets whether the message box should snap to the edge.
   *
   * @return whether the message box should snap to the edge
   */
  public boolean isSnapToEdge() {
    return snapToEdge;
  }

  /**
   * Adds an attribute to the message box.
   *
   * @param name the name of the attribute
   * @param value the value of the attribute
   * @return the message box
   */
  public T setAttribute(String name, String value) {
    attributes.put(name, value);
    return getSelf();
  }

  /**
   * Removes an attribute from the message box.
   *
   * @param name the name of the attribute
   * @return the message box
   */
  public T removeAttribute(String name) {
    attributes.remove(name);
    return getSelf();
  }

  /**
   * Sets the attributes of the message box.
   *
   * @return the attributes of the message box
   */
  public T setAttributes(Map<String, String> attributes) {
    this.attributes.clear();
    this.attributes.putAll(attributes);
    return getSelf();
  }

  /**
   * Gets the attributes of the message box.
   *
   * @return the attributes of the message box
   */
  public Map<String, String> getAttributes() {
    return Collections.unmodifiableMap(attributes);
  }

  /**
   * Gets the attributes of the message box as a string.
   *
   * @return the attributes of the message box as a string
   */
  public String getAttributesAsString() {
    Map<String, String> attrs = getAttributes();
    StringJoiner joiner = new StringJoiner(",");

    for (Map.Entry<String, String> entry : attrs.entrySet()) {
      joiner.add(entry.getKey() + "=" + entry.getValue() + "");
    }

    return joiner.toString();
  }

  /**
   * Returns an instance of the current class, casted to its generic type. This method is primarily
   * used for method chaining in subclasses of TypedDwcComponent.
   *
   * @return An instance of the current class, casted to its generic type.
   */
  protected final T getSelf() {
    @SuppressWarnings("unchecked")
    T self = (T) this;

    return self;
  }

  void toggleAttribute(String name, String value, boolean condition) {
    if (condition) {
      setAttribute(name, value);
    } else {
      removeAttribute(name);
    }
  }
}
