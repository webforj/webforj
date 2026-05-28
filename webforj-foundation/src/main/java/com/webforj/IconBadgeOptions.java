package com.webforj;

import com.webforj.component.field.ColorField;
import java.awt.Color;

/**
 * Options for {@link Page#setIconBadge(Integer, IconBadgeOptions)}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class IconBadgeOptions {

  /**
   * The shape used to draw the badge background.
   */
  public enum Shape {
    /**
     * A filled circle.
     */
    CIRCLE,
    /**
     * A filled square.
     */
    SQUARE
  }

  private Color color = new Color(0xe5, 0x39, 0x35);
  private Shape shape = Shape.CIRCLE;
  private double size = 1.0;

  /**
   * Sets the badge color.
   *
   * <p>
   * Used as the background fill of the badge. The text color is derived automatically from the
   * background so that the digits stay readable on any seed color. The default value is
   * {@code #e53935}.
   * </p>
   *
   * @param color the badge color
   * @return the icon badge options
   */
  public IconBadgeOptions setColor(Color color) {
    this.color = color;

    return this;
  }

  /**
   * Gets the badge color.
   *
   * @return the badge color
   * @see #setColor(Color)
   */
  public Color getColor() {
    return color;
  }

  /**
   * Sets the shape of the badge background.
   *
   * <p>
   * The default value is {@link Shape#CIRCLE}.
   * </p>
   *
   * @param shape the shape
   * @return the icon badge options
   */
  public IconBadgeOptions setShape(Shape shape) {
    this.shape = shape;

    return this;
  }

  /**
   * Gets the shape of the badge background.
   *
   * @return the shape
   * @see #setShape(Shape)
   */
  public Shape getShape() {
    return shape;
  }

  /**
   * Sets the relative size of the badge.
   *
   * <p>
   * A value of {@code 1.0} is the default size. Values below {@code 1.0} produce a smaller badge,
   * values above {@code 1.0} produce a larger one. The badge always anchors at the bottom right
   * corner of the favicon.
   * </p>
   *
   * @param size the relative size
   * @return the icon badge options
   * @throws IllegalArgumentException if {@code size} is not positive
   */
  public IconBadgeOptions setSize(double size) {
    if (size <= 0) {
      throw new IllegalArgumentException("Badge size must be greater than zero.");
    }

    this.size = size;

    return this;
  }

  /**
   * Gets the relative size of the badge.
   *
   * @return the relative size
   * @see #setSize(double)
   */
  public double getSize() {
    return size;
  }

  String colorHex() {
    return ColorField.toHex(color);
  }
}
