package com.webforj;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

/**
 * A class for logging styled messages to the browser console.
 *
 * <p>
 * This class provides methods to log messages with custom styles such as font styles, font sizes,
 * text transformations, colors, and background colors.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public final class BrowserConsole {
  private static final Map<String, String> COLORS = new HashMap<>();
  private final Map<String, String> styles = new HashMap<>();

  static {
    COLORS.put("white", "white");
    COLORS.put("black", "black");
    COLORS.put("silver", "silver");
    COLORS.put("gray", "gray");
    COLORS.put("red", "#E86C5D");
    COLORS.put("green", "#74ED7B");
    COLORS.put("blue", "#3F6FFB");
    COLORS.put("gold", "gold");
    COLORS.put("yellow", "yellow");
    COLORS.put("pink", "pink");
    COLORS.put("cyan", "cyan");
  }

  @FunctionalInterface
  interface Builder {
    BrowserConsole getInstance();
  }

  BrowserConsole() {}

  /**
   * Represents the font style builder.
   *
   * <p>
   * This builder is used to set the font style of the text.
   * </p>
   */
  public interface FontStyleBuilder extends Builder {
    /**
     * Sets the font style to italic.
     *
     * @return the console instance
     */
    default BrowserConsole italic() {
      getInstance().setStyle("font-style", "italic");
      return getInstance();
    }

    /**
     * Sets the font style to normal.
     *
     * @return the console instance
     */
    default BrowserConsole normal() {
      getInstance().setStyle("font-style", "normal");
      return getInstance();
    }
  }

  /**
   * Represents the font weight builder.
   *
   * <p>
   * This builder is used to set the font weight of the text.
   * </p>
   */
  public interface FontWeightBuilder extends Builder {
    /**
     * Sets the font weight to bold.
     *
     * @return the console instance
     */
    default BrowserConsole bold() {
      getInstance().setStyle("font-weight", "bold");
      return getInstance();
    }

    /**
     * Sets the font weight to normal.
     *
     * @return the console instance
     */
    default BrowserConsole normal() {
      getInstance().setStyle("font-weight", "normal");
      return getInstance();
    }

    /**
     * Sets the font weight to bolder.
     *
     * @return the console instance
     */
    default BrowserConsole bolder() {
      getInstance().setStyle("font-weight", "bolder");
      return getInstance();
    }

    /**
     * Sets the font weight to lighter.
     *
     * @return the console instance
     */
    default BrowserConsole lighter() {
      getInstance().setStyle("font-weight", "lighter");
      return getInstance();
    }
  }

  /**
   * Represents the font size builder.
   *
   * <p>
   * This builder is used to set the font size of the text.
   * </p>
   */
  public interface FontSizeBuilder extends Builder {
    /**
     * Sets the font size to small.
     *
     * @return the console instance
     */
    default BrowserConsole small() {
      getInstance().setStyle("font-size", "small");
      return getInstance();
    }

    /**
     * Sets the font size to medium.
     *
     * @return the console instance
     */
    default BrowserConsole medium() {
      getInstance().setStyle("font-size", "medium");
      return getInstance();
    }

    /**
     * Sets the font size to large.
     *
     * @return the console instance
     */
    default BrowserConsole large() {
      getInstance().setStyle("font-size", "large");
      return getInstance();
    }

    /**
     * Sets the font size to smaller.
     *
     * @return the console instance
     */
    default BrowserConsole smaller() {
      getInstance().setStyle("font-size", "x-small");
      return getInstance();
    }

    /**
     * Sets the font size to larger.
     *
     * @return the console instance
     */
    default BrowserConsole larger() {
      getInstance().setStyle("font-size", "x-large");
      return getInstance();
    }

    /**
     * Sets the font size to the given value.
     *
     * @param value the font size value
     * @return the console instance
     */
    default BrowserConsole from(String value) {
      getInstance().setStyle("font-size", value);
      return getInstance();
    }
  }

  /**
   * Represents the text transformation builder.
   *
   * <p>
   * This builder is used to set the text transformation of the text.
   * </p>
   */
  public interface TextTransformBuilder extends Builder {
    /**
     * Sets the text transformation to none.
     *
     * @return the console instance
     */
    default BrowserConsole none() {
      getInstance().setStyle("text-transform", "none");
      return getInstance();
    }

    /**
     * Sets the text transformation to capitalize.
     *
     * @return the console instance
     */
    default BrowserConsole capitalize() {
      getInstance().setStyle("text-transform", "capitalize");
      return getInstance();
    }

    /**
     * Sets the text transformation to uppercase.
     *
     * @return the console instance
     */
    default BrowserConsole uppercase() {
      getInstance().setStyle("text-transform", "uppercase");
      return getInstance();
    }

    /**
     * Sets the text transformation to lowercase.
     *
     * @return the console instance
     */
    default BrowserConsole lowercase() {
      getInstance().setStyle("text-transform", "lowercase");
      return getInstance();
    }
  }

  /**
   * Represents a color builder for a given property.
   *
   * <p>
   * This builder is used to set the text and background color.
   * </p>
   */
  interface BaseColorBuilder extends Builder {
    /**
     * Gets the property.
     *
     * @return the property
     */
    String getProperty();

    /**
     * Sets the text color to white.
     *
     * @return the console instance
     */
    default BrowserConsole white() {
      getInstance().setStyle(getProperty(), COLORS.get("white"));
      return getInstance();
    }

    /**
     * Sets the text color to black.
     *
     * @return the console instance
     */
    default BrowserConsole black() {
      getInstance().setStyle(getProperty(), COLORS.get("black"));
      return getInstance();
    }

    /**
     * Sets the text color to silver.
     *
     * @return the console instance
     */
    default BrowserConsole silver() {
      getInstance().setStyle(getProperty(), COLORS.get("silver"));
      return getInstance();
    }

    /**
     * Sets the text color to gray.
     *
     * @return the console instance
     */
    default BrowserConsole gray() {
      getInstance().setStyle(getProperty(), COLORS.get("gray"));
      return getInstance();
    }

    /**
     * Sets the text color to red.
     *
     * @return the console instance
     */
    default BrowserConsole red() {
      getInstance().setStyle(getProperty(), COLORS.get("red"));
      return getInstance();
    }

    /**
     * Sets the text color to green.
     *
     * @return the console instance
     */
    default BrowserConsole green() {
      getInstance().setStyle(getProperty(), COLORS.get("green"));
      return getInstance();
    }

    /**
     * Sets the text color to blue.
     *
     * @return the console instance
     */
    default BrowserConsole blue() {
      getInstance().setStyle(getProperty(), COLORS.get("blue"));
      return getInstance();
    }

    /**
     * Sets the text color to gold.
     *
     * @return the console instance
     */
    default BrowserConsole gold() {
      getInstance().setStyle(getProperty(), COLORS.get("gold"));
      return getInstance();
    }

    /**
     * Sets the text color to yellow.
     *
     * @return the console instance
     */
    default BrowserConsole yellow() {
      getInstance().setStyle(getProperty(), COLORS.get("yellow"));
      return getInstance();
    }

    /**
     * Sets the text color to pink.
     *
     * @return the console instance
     */
    default BrowserConsole pink() {
      getInstance().setStyle(getProperty(), COLORS.get("pink"));
      return getInstance();
    }

    /**
     * Sets the text color to cyan.
     *
     * @return the console instance
     */
    default BrowserConsole cyan() {
      getInstance().setStyle(getProperty(), COLORS.get("cyan"));
      return getInstance();
    }

    /**
     * Sets the text color to the given color.
     *
     * @param color the color
     * @return the console instance
     */
    default BrowserConsole colored(String color) {
      getInstance().setStyle(getProperty(), color);
      return getInstance();
    }
  }

  /**
   * Represents the text color builder.
   *
   * <p>
   * This builder is used to set the color of the text.
   * </p>
   */
  public interface ColorBuilder extends BaseColorBuilder {
    /**
     * {@inheritDoc}
     */
    @Override
    default String getProperty() {
      return "color";
    }
  }

  /**
   * Represents the background color builder.
   *
   * <p>
   * This builder is used to set the background color.
   * </p>
   */
  public interface BackgroundColorBuilder extends BaseColorBuilder {
    /**
     * {@inheritDoc}
     */
    @Override
    default String getProperty() {
      return "background-color";
    }
  }

  /**
   * Adds a style property.
   *
   * @param property the style property
   * @param value the style value
   * @return the console instance
   */
  public BrowserConsole setStyle(String property, String value) {
    styles.put(property, value);
    return this;
  }

  /**
   * Removes a style property.
   *
   * @param property the style property
   * @return the console instance
   */
  public BrowserConsole removeStyle(String property) {
    styles.remove(property);
    return this;
  }

  /**
   * Gets the computed styles.
   *
   * @return the computed styles
   */
  Map<String, String> getStyles() {
    return styles;
  }

  /**
   * Gets the text style builder.
   *
   * @return the text style builder
   */
  public FontStyleBuilder style() {
    return () -> this;
  }

  /**
   * Gets the text weight builder.
   *
   * @return the text weight builder
   */
  public FontWeightBuilder weight() {
    return () -> this;
  }

  /**
   * Gets the text size builder.
   *
   * @return the text size builder
   */
  public FontSizeBuilder size() {
    return () -> this;
  }

  /**
   * Gets the text transform builder.
   *
   * @return the text transform builder
   */
  public TextTransformBuilder transform() {
    return () -> this;
  }

  /**
   * Gets the text color builder.
   *
   * @return the text color builder
   */
  public ColorBuilder color() {
    return () -> this;
  }

  /**
   * Gets the background color builder.
   *
   * @return the background color builder
   */
  public BackgroundColorBuilder background() {
    return () -> this;
  }

  /**
   * Logs the given message.
   *
   * @param message the message
   * @param forced whether to force the log even if debug mode is off
   * @return the console instance
   */
  public BrowserConsole log(Object message, boolean forced) {
    return print(message, "log", forced);
  }

  /**
   * Logs the given message.
   *
   * @param message the message
   * @return the console instance
   */
  public BrowserConsole log(Object message) {
    return log(message, false);
  }

  /**
   * Logs the given message as info.
   *
   * @param message the message
   * @param forced whether to force the log even if debug mode is off
   * @return the console instance
   */
  public BrowserConsole info(Object message, boolean forced) {
    return print(message, "info", forced);
  }

  /**
   * Logs the given message as info.
   *
   * @param message the message
   * @return the console instance
   */
  public BrowserConsole info(Object message) {
    return info(message, false);
  }

  /**
   * Logs the given message as a warning.
   *
   * @param message the message
   * @param forced whether to force the log even if debug mode is off
   * @return the console instance
   */
  public BrowserConsole warn(Object message, boolean forced) {
    return print(message, "warn", forced);
  }

  /**
   * Logs the given message as a warning.
   *
   * @param message the message
   * @return the console instance
   */
  public BrowserConsole warn(Object message) {
    return warn(message, false);
  }

  /**
   * Logs the given message as an error.
   *
   * @param message the message
   * @param forced whether to force the log even if debug mode is off
   * @return the console instance
   */
  public BrowserConsole error(Object message, boolean forced) {
    return print(message, "error", forced);
  }

  /**
   * Logs the given message as an error.
   *
   * @param message the message
   * @return the console instance
   */
  public BrowserConsole error(Object message) {
    return error(message, false);
  }

  /**
   * Logs the given message as debug.
   *
   * @param message the message
   * @param forced whether to force the log even if debug mode is off
   * @return the console instance
   */
  public BrowserConsole debug(Object message, boolean forced) {
    return print(message, "debug", forced);
  }

  /**
   * Logs the given message as debug.
   *
   * @param message the message
   * @return the console instance
   */
  public BrowserConsole debug(Object message) {
    return debug(message, false);
  }

  private BrowserConsole print(Object message, String level, boolean forced) {
    if (!forced) {
      Environment env = Environment.getCurrent();
      boolean isDebug = env != null && env.isDebug();
      if (!isDebug) {
        return this;
      }
    }

    String theMessage = String.valueOf(message);

    // Handle exceptions
    if (message instanceof Exception ex) {
      StringWriter sw = new StringWriter();
      PrintWriter pw = new PrintWriter(sw);
      ex.printStackTrace(pw);
      theMessage = sw.toString();
    }

    message = Base64.getEncoder().encodeToString(theMessage.getBytes());
    String code = String.format("console.%s('%%c ' + atob('%s'), '%s')", level, message,
        buildComputedStyles());
    Page.ifPresent(page -> {
      try {
        page.executeJsAsync(code);
      } catch (Exception e) {
        // ignore - likely during app termination when connection is closed
      }
    });
    styles.clear();
    return this;
  }

  private String buildComputedStyles() {
    StringBuilder sb = new StringBuilder();
    for (Map.Entry<String, String> entry : styles.entrySet()) {
      sb.append(entry.getKey()).append(":").append(entry.getValue()).append("; ");
    }

    return sb.toString().trim();
  }
}
