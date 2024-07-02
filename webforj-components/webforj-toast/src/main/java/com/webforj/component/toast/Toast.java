package com.webforj.component.toast;

import com.google.gson.annotations.SerializedName;
import com.webforj.App;
import com.webforj.component.ComponentLifecycleObserver;
import com.webforj.component.Theme;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.annotation.PropertyExclude;
import com.webforj.component.element.annotation.PropertyMethods;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasHtml;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.exceptions.WebforjAppInitializeException;
import java.util.List;

/**
 * A subtle notification commonly used in modern applications. It provides feedback about an
 * operation or displays a system message.
 *
 * <p>
 * Example usage:
 *
 * <pre>
 * {@code
 * Toast.show("Operation completed", Theme.GRAY);
 * }
 * </pre>
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
@NodeName("dwc-toast")
public class Toast extends ElementCompositeContainer
    implements HasClassName<Toast>, HasStyle<Toast>, HasText<Toast>, HasHtml<Toast> {

  static final Placement DEFAULT_PLACEMENT = Placement.BOTTOM;
  static final Theme DEFAULT_THEME = Theme.DEFAULT;
  static final int DEFAULT_DURATION = 3000;

  /**
   * Describes the placement of the toast.
   */
  public enum Placement {
    /**
     * Place the toast at the bottom of the screen.
     */
    @SerializedName("bottom")
    BOTTOM("bottom"),

    /**
     * Place the toast at the bottom-left of the screen.
     */
    @SerializedName("bottom-left")
    BOTTOM_LEFT("bottom-left"),

    /**
     * Place the toast at the bottom-right of the screen.
     */
    @SerializedName("bottom-right")
    BOTTOM_RIGHT("bottom-right"),

    /**
     * Place the toast at the center of the screen.
     */
    @SerializedName("center")
    CENTER("center"),

    /**
     * Place the toast at the top of the screen.
     */
    @SerializedName("top")
    TOP("top"),

    /**
     * Place the toast at the top-left of the screen.
     */
    @SerializedName("top-left")
    TOP_LEFT("top-left"),

    /**
     * Place the toast at the top-right of the screen.
     */
    @SerializedName("top-right")
    TOP_RIGHT("top-right");

    private final String value;

    Placement(String value) {
      this.value = value;
    }

    /**
     * Get the value of the enum.
     *
     * @return the value of the enum
     */
    public String getValue() {
      return value;
    }
  }

  private Frame frame;

  @PropertyMethods(setter = "setText", getter = "getText")
  private final PropertyDescriptor<String> messageProp = PropertyDescriptor.property("message", "");
  private final PropertyDescriptor<Integer> durationProp =
      PropertyDescriptor.property("duration", 3000);
  @PropertyExclude
  private final PropertyDescriptor<Boolean> openedProp =
      PropertyDescriptor.property("opened", false);
  private final PropertyDescriptor<Placement> placementProp =
      PropertyDescriptor.property("placement", Placement.BOTTOM);
  private final PropertyDescriptor<Theme> themeProp =
      PropertyDescriptor.property("theme", Theme.DEFAULT);

  /**
   * Constructs a new toast with specified text, duration, theme, and placement.
   *
   * @param text the text of the toast.
   * @param duration the duration in milliseconds to show the toast.
   * @param theme the theme of the toast.
   * @param placement the toast placement.
   */
  public Toast(String text, int duration, Theme theme, Placement placement) {
    setText(text);
    setDuration(duration);
    setTheme(theme);
    setPlacement(placement);
  }

  /**
   * Constructs a new toast with specified text, duration, and theme.
   *
   * @param text the text of the toast.
   * @param duration the duration in milliseconds to show the toast.
   * @param theme the theme of the toast.
   */
  public Toast(String text, int duration, Theme theme) {
    this(text, duration, theme, DEFAULT_PLACEMENT);
  }

  /**
   * Constructs a new toast with specified text and theme.
   *
   * @param text the text of the toast.
   * @param theme the theme of the toast.
   */
  public Toast(String text, Theme theme) {
    this(text, DEFAULT_DURATION, theme);
  }

  /**
   * Constructs a new toast with specified text, duration, and placement.
   *
   * @param text the text of the toast.
   * @param duration the duration in milliseconds to show the toast.
   * @param placement the toast placement.
   */
  public Toast(String text, int duration, Placement placement) {
    this(text, duration, DEFAULT_THEME, placement);
  }

  /**
   * Constructs a new toast with specified text and placement.
   *
   * @param text the text of the toast.
   * @param placement the toast placement.
   */
  public Toast(String text, Placement placement) {
    this(text, DEFAULT_DURATION, placement);
  }

  /**
   * Constructs a new toast with specified text and duration.
   *
   * @param text the text of the toast.
   * @param duration the duration in milliseconds to show the toast.
   */
  public Toast(String text, int duration) {
    this(text, duration, DEFAULT_THEME);
  }

  /**
   * Constructs a new toast with specified text.
   *
   * @param text the text of the toast.
   */
  public Toast(String text) {
    this(text, DEFAULT_DURATION);
  }

  /**
   * Constructs a new toast with an empty text.
   */
  public Toast() {
    this("");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Toast setText(String text) {
    setHtml(sanitizeHtml(text));
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    return sanitizeHtml(getHtml());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Toast setHtml(String html) {
    set(messageProp, html);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getHtml() {
    return get(messageProp);
  }

  /**
   * Sets the duration in milliseconds to show the toast. After this time elapses, the toast will
   * hide automatically. If the duration is negative, the toast will not close automatically.
   *
   * @param duration the duration in milliseconds to show the toast.
   * @return the component itself.
   */
  public Toast setDuration(int duration) {
    set(durationProp, duration);
    return this;
  }

  /**
   * Returns the duration in milliseconds to show the toast.
   *
   * @return the duration in milliseconds to show the toast.
   */
  public int getDuration() {
    return get(durationProp);
  }

  /**
   * Sets the toast placement.
   *
   * @param placement the toast placement.
   * @return the component itself.
   */
  public Toast setPlacement(Placement placement) {
    set(placementProp, placement);
    return this;
  }

  /**
   * Returns the toast placement.
   *
   * @return the toast placement.
   */
  public Placement getPlacement() {
    return get(placementProp);
  }

  /**
   * Sets the theme of the toast.
   *
   * @param theme the theme of the toast.
   * @return the component itself.
   */
  public Toast setTheme(Theme theme) {
    set(themeProp, theme);
    return this;
  }

  /**
   * Returns the theme of the toast.
   *
   * @return the theme of the toast.
   */
  public Theme getTheme() {
    return get(themeProp);
  }

  /**
   * Opens the toast.
   *
   * @return the component itself.
   */
  public Toast open() {
    if (!isAttached()) {
      getFrame(true).add(this);
    }

    set(openedProp, true);
    return this;
  }

  /**
   * Closes and destroys the toast.
   */
  public void close() {
    set(openedProp, false);
    destroy();
  }

  /**
   * Shows a toast with the given text, duration, theme, and placement.
   *
   * @param text the text of the toast.
   * @param duration the duration in milliseconds to show the toast.
   * @param theme the theme of the toast.
   * @param placement the toast placement.
   *
   * @return the component itself.
   */
  public static Toast show(String text, int duration, Theme theme, Placement placement) {
    Toast toast = new Toast(text, duration, theme, placement);
    toast.open();
    return toast;
  }

  /**
   * Shows a toast with the given text, duration, and theme.
   *
   * @param text the text of the toast.
   * @param duration the duration in milliseconds to show the toast.
   * @param theme the theme of the toast.
   *
   * @return the component itself.
   */
  public static Toast show(String text, int duration, Theme theme) {
    Toast toast = new Toast(text, duration, theme);
    toast.open();
    return toast;
  }

  /**
   * Shows a toast with the given text and theme.
   *
   * @param text the text of the toast.
   * @param theme the theme of the toast.
   *
   * @return the component itself.
   */
  public static Toast show(String text, Theme theme) {
    Toast toast = new Toast(text, theme);
    toast.open();
    return toast;
  }

  /**
   * Shows a toast with the given text, duration, and placement.
   *
   * @param text the text of the toast.
   * @param duration the duration in milliseconds to show the toast.
   * @param placement the toast placement.
   *
   * @return the component itself.
   */
  public static Toast show(String text, int duration, Placement placement) {
    Toast toast = new Toast(text, duration, placement);
    toast.open();
    return toast;
  }

  /**
   * Shows a toast with the given text and placement.
   *
   * @param text the text of the toast.
   * @param placement the toast placement.
   *
   * @return the component itself.
   */
  public static Toast show(String text, Placement placement) {
    Toast toast = new Toast(text, placement);
    toast.open();
    return toast;
  }

  /**
   * Shows a toast with the given text and duration.
   *
   * @param text the text of the toast.
   * @param duration the duration in milliseconds to show the toast.
   *
   * @return the component itself.
   */
  public static Toast show(String text, int duration) {
    Toast toast = new Toast(text, duration);
    toast.open();
    return toast;
  }

  /**
   * Shows a toast with the given text.
   *
   * @param text the text of the toast.
   * @return the component itself.
   */
  public static Toast show(String text) {
    Toast toast = new Toast(text);
    toast.open();
    return toast;
  }

  private Frame getFrame(boolean createIfNotExist) {
    if (frame != null) {
      return frame;
    }
    List<Frame> frames = App.getFrames();
    if (!frames.isEmpty()) {
      for (Frame f : frames) {
        if (!f.isDestroyed()) {
          frame = f;
          break;
        }
      }
    }
    if (frame == null && createIfNotExist) {
      try {
        frame = new Frame();
        frame.setVisible(false);
      } catch (WebforjAppInitializeException e) {
        throw new IllegalStateException("Failed to create a frame container for the toast", e);
      }
    }
    if (frame != null) {
      frame.addLifecycleObserver((component, event) -> {
        if (event == ComponentLifecycleObserver.LifecycleEvent.DESTROY) {
          frame = null;
        }
      });
    }
    return frame;
  }

  private String sanitizeHtml(String html) {
    return html.replaceAll("\\<[^>]*>", "");
  }
}
