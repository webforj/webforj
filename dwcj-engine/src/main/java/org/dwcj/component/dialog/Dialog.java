package org.dwcj.component.dialog;

import org.dwcj.component.HasClassName;
import org.dwcj.component.HasStyle;
import org.dwcj.component.dialog.event.DialogCloseEvent;
import org.dwcj.component.dialog.event.DialogOpenEvent;
import org.dwcj.component.webcomponent.PropertyDescriptor;
import org.dwcj.component.webcomponent.WebComponent;
import org.dwcj.component.webcomponent.annotations.NodeName;
import org.dwcj.component.webcomponent.events.EventListener;
import org.dwcj.component.window.Panel;

/**
 * A dialog component.
 *
 * @author Hyyan Abo Fakher
 */
@NodeName("bbj-dialog")
public class Dialog extends WebComponent implements HasClassName, HasStyle {

  /**
   * The dialog alignments
   */
  public enum Alignment {
    /** The dialog will be aligned to the bottom of the screen. */
    BOTTOM("bottom"),

    /** The dialog will be aligned to the center of the screen. */
    CENTER("center"),

    /** The dialog will be aligned to the top of the screen. */
    TOP("top");

    private final String value;

    Alignment(String value) {
      this.value = value;
    }

    /**
     * Get the value of the enum.
     *
     * @return the value
     */
    public String getValue() {
      return value;
    }

    /**
     * Get the enum from the value.
     *
     * @param value the value
     * @return the enum
     */
    public static Alignment fromValue(String value) {
      for (Alignment alignment : Alignment.values()) {
        if (alignment.value.equalsIgnoreCase(value)) {
          return alignment;
        }
      }

      return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
      return value;
    }
  }

  /**
   * The dialog themes
   */
  public enum Theme {
    /** The dialog will be themed as a danger dialog. */
    DANGER("danger"),

    /** The dialog will be themed as a default dialog. */
    DEFAULT("default"),

    /** The dialog will be themed as a gray dialog. */
    GRAY("gray"),

    /** The dialog will be themed as an info dialog. */
    INFO("info"),

    /** The dialog will be themed as a primary dialog. */
    PRIMARY("primary"),

    /** The dialog will be themed as a success dialog. */
    SUCCESS("success"),

    /** The dialog will be themed as a warning dialog. */
    WARNING("warning");

    private final String value;

    Theme(String value) {
      this.value = value;
    }

    /**
     * Get the value of the enum.
     *
     * @return the value
     */
    public String getValue() {
      return value;
    }

    /**
     * Get the enum from the value.
     *
     * @param value the value
     * @return the enum
     */
    public static Theme fromValue(String value) {
      for (Theme theme : Theme.values()) {
        if (theme.value.equalsIgnoreCase(value)) {
          return theme;
        }
      }

      return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
      return value;
    }
  }

  // Panels
  private Panel header;
  private Panel content;
  private Panel footer;

  // Properties
  private final PropertyDescriptor<String> ALIGNMENT =
      PropertyDescriptor.property("alignment", Alignment.CENTER.getValue());
  private final PropertyDescriptor<Boolean> AUTOFOCUS =
      PropertyDescriptor.property("autofocus", false);
  private final PropertyDescriptor<Boolean> BACKDROP =
      PropertyDescriptor.property("backdrop", true);
  private final PropertyDescriptor<Boolean> BLURRED = PropertyDescriptor.property("blurred", false);
  private final PropertyDescriptor<String> BREAKPOINT =
      PropertyDescriptor.property("breakpoint", "");
  private final PropertyDescriptor<Boolean> CANCEL_ON_ESC_KEY =
      PropertyDescriptor.property("cancelOnEscKey", false);
  private final PropertyDescriptor<Boolean> CANCEL_ON_OUTSIDE_CLICK =
      PropertyDescriptor.property("cancelOnOutsideClick", true);
  private final PropertyDescriptor<Boolean> FULLSCREEN =
      PropertyDescriptor.property("fullscreen", false);
  private final PropertyDescriptor<String> MAX_HEIGHT =
      PropertyDescriptor.property("maxHeight", "");
  private final PropertyDescriptor<String> MAX_WIDTH = PropertyDescriptor.property("maxWidth", "");
  private final PropertyDescriptor<Boolean> MOVEABLE =
      PropertyDescriptor.property("moveable", true);
  private final PropertyDescriptor<Boolean> OPENED = PropertyDescriptor.property("opened", false);
  private final PropertyDescriptor<String> POSX = PropertyDescriptor.property("posx", "");
  private final PropertyDescriptor<String> POSY = PropertyDescriptor.property("posy", "");
  private final PropertyDescriptor<Integer> SNAP_THRESHOLD =
      PropertyDescriptor.property("snapThreshold", 0);
  private final PropertyDescriptor<Boolean> SNAP_TO_EDGE =
      PropertyDescriptor.property("snapToEdge", true);
  private final PropertyDescriptor<String> THEME =
      PropertyDescriptor.property("theme", Theme.DEFAULT.getValue());

  /**
   * Instantiates a new app layout.
   */
  public Dialog() {
    // Make sure to append the dialog to the end of the body to avoid
    // any stacking issues.
    // This call should be made very early in the lifecycle of the component
    // before any events are added because moving the element will remove all
    // the event listeners
    executeAsyncExpression("document.body.appendChild(component)");

    this.setHeader(new Panel());
    this.setContent(new Panel());
    this.setFooter(new Panel());
  }

  /**
   * Use the passed panel in header slot.
   *
   * @param header the header panel
   * @return the dialog
   */
  public Dialog setHeader(Panel header) {
    this.header = header;
    addSlot("header", header);
    return this;
  }

  /**
   * Get the header panel instance.
   *
   * @param drawer the drawer panel
   * @return the app layout
   */
  public Panel getHeader() {
    return header;
  }

  /**
   * Use the passed panel in content slot.
   *
   * @param content the content panel
   * @return the dialog
   */
  public Dialog setContent(Panel content) {
    this.content = content;
    this.content.setStyle("overflow", "auto");
    addSlot("content", content);
    return this;
  }

  /**
   * Get the content panel instance.
   *
   * @return the content panel
   */
  public Panel getContent() {
    return content;
  }

  /**
   * Use the passed panel in footer slot.
   *
   * @param footer the footer panel
   * @return the dialog
   */
  public Dialog setFooter(Panel footer) {
    this.footer = footer;
    addSlot("footer", footer);
    return this;
  }

  /**
   * Get the footer panel instance.
   *
   * @return the footer panel
   */
  public Panel getFooter() {
    return footer;
  }

  /**
   * Set the dialog alignment.
   *
   * @param alignment the alignment
   * @return the dialog
   * @see Alignment
   */
  public Dialog setAlignment(Alignment alignment) {
    set(ALIGNMENT, alignment.getValue());
    return this;
  }

  /**
   * Get the dialog alignment.
   *
   * @return the alignment
   * @see Alignment
   */
  public Alignment getAlignment() {
    return Alignment.fromValue(get(ALIGNMENT));
  }

  /**
   * Set the dialog autofocus.
   *
   * When true then automatically focus the first focusable element in the dialog.
   *
   * @param autofocus the autofocus
   * @return the dialog
   */
  public Dialog setAutoFocus(boolean autofocus) {
    set(AUTOFOCUS, autofocus);
    return this;
  }

  /**
   * Get the dialog autofocus.
   *
   * @return the autofocus
   */
  public boolean isAutoFocus() {
    return get(AUTOFOCUS);
  }

  /**
   * Set the dialog backdrop.
   *
   * When true then a backdrop will be displayed behind the dialog.
   *
   * @param backdrop the backdrop
   * @return the dialog
   */
  public Dialog setBackdrop(boolean backdrop) {
    set(BACKDROP, backdrop);
    return this;
  }

  /**
   * Get the dialog backdrop.
   *
   * @return the backdrop
   */
  public boolean isBackdrop() {
    return get(BACKDROP);
  }

  /**
   * Set the dialog blurred.
   *
   * When true then the dialog's backdrop will be blurred.
   *
   * @param blurred the blurred
   * @return the dialog
   */
  public Dialog setBlurred(boolean blurred) {
    set(BLURRED, blurred);
    return this;
  }

  /**
   * Get the dialog blurred.
   *
   * @return the blurred
   */
  public boolean isBlurred() {
    return get(BLURRED);
  }

  /**
   * Set the dialog breakpoint.
   *
   * A media query to control when the dialog will automatically flip to the full screen mode.When
   * the media query matches, the dialog will be full screen, otherwise it will be positioned.When
   * the auto full screen is enabled, the dialog cannot be moved or positioned.
   *
   * @param breakpoint the breakpoint
   * @return the dialog
   */
  public Dialog setBreakpoint(String breakpoint) {
    set(BREAKPOINT, breakpoint);
    return this;
  }

  /**
   * Get the dialog breakpoint.
   *
   * @return the breakpoint
   */
  public String getBreakpoint() {
    return get(BREAKPOINT);
  }

  /**
   * Set the dialog cancel on esc key.
   *
   * When true then the dialog can be cancelled by pressing the esc key.
   *
   * @param cancelOnEscKey the cancel on esc key
   * @return the dialog
   */
  public Dialog setCancelOnEscKey(boolean cancelOnEscKey) {
    set(CANCEL_ON_ESC_KEY, cancelOnEscKey);
    return this;
  }

  /**
   * Get the dialog cancel on esc key.
   *
   * @return the cancel on esc key
   */
  public boolean isCancelOnEscKey() {
    return get(CANCEL_ON_ESC_KEY);
  }

  /**
   * Set the dialog cancel on outside click.
   *
   * When true then the dialog can be cancelled by clicking outside of it.
   *
   * @param cancelOnOutsideClick the cancel on outside click
   * @return the dialog
   */
  public Dialog setCancelOnOutsideClick(boolean cancelOnOutsideClick) {
    set(CANCEL_ON_OUTSIDE_CLICK, cancelOnOutsideClick);
    return this;
  }

  /**
   * Get the dialog cancel on outside click.
   *
   * @return the cancel on outside click
   */
  public boolean isCancelOnOutsideClick() {
    return get(CANCEL_ON_OUTSIDE_CLICK);
  }

  /**
   * Disable closing the dialog by clicking outside or pressing the esc key.
   *
   * @param closeable the closeable
   * @return the dialog
   * @see #setCancelOnOutsideClick(boolean)
   * @see #setCancelOnEscKey(boolean)
   */
  public Dialog setCloseable(boolean closeable) {
    setCancelOnOutsideClick(false);
    setCancelOnEscKey(false);
    return this;
  }

  /**
   * Set the dialog fullscreen.
   *
   * When true then the dialog will be fullscreen.
   *
   * @param fullscreen the fullscreen
   * @return the dialog
   */
  public Dialog setFullScreen(boolean fullscreen) {
    set(FULLSCREEN, fullscreen);
    return this;
  }

  /**
   * Get the dialog fullscreen.
   *
   * @return the fullscreen
   */
  public boolean isFullScreen() {
    return get(FULLSCREEN, true, Boolean.class);
  }

  /**
   * Set the dialog max height.
   *
   * The maximum height of the dialog.
   *
   * @param maxHeight the max height
   * @return the dialog
   */
  public Dialog setMaxHeight(String maxHeight) {
    set(MAX_HEIGHT, maxHeight);
    return this;
  }

  /**
   * Get the dialog max height.
   *
   * @return the max height
   */
  public String getMaxHeight() {
    return get(MAX_HEIGHT);
  }

  /**
   * Set the dialog max width.
   *
   * The maximum width of the dialog.
   *
   * @param maxWidth the max width
   * @return the dialog
   */
  public Dialog setMaxWidth(String maxWidth) {
    set(MAX_WIDTH, maxWidth);
    return this;
  }

  /**
   * Get the dialog max width.
   *
   * @return the max width
   */
  public String getMaxWidth() {
    return get(MAX_WIDTH);
  }

  /**
   * Make the dialog moveable.
   *
   * When true , then the dialog can be dragged to move it.
   *
   * @param moveable the moveable
   * @return the dialog
   */
  public Dialog setMoveable(boolean moveable) {
    set(MOVEABLE, moveable);
    return this;
  }

  /**
   * Get the dialog moveable.
   *
   * @return the moveable
   */
  public boolean isMoveable() {
    return get(MOVEABLE);
  }

  /**
   * Show the dialog.
   *
   * @return the dialog
   */
  public Dialog show() {
    set(OPENED, true);
    return this;
  }

  /**
   * Hide the dialog.
   *
   * @return the dialog
   */
  public Dialog hide() {
    set(OPENED, false);
    return this;
  }

  /**
   * Check if the dialog is opened.
   *
   * @return true if the dialog is opened, false otherwise
   */
  public boolean isOpened() {
    return get(OPENED, true, Boolean.class);
  }

  /**
   * specify the X position for the dialog
   *
   * @param posx the X position
   * @return the dialog
   */
  public Dialog setPosx(String posx) {
    set(POSX, posx);
    return this;
  }

  /**
   * Get the dialog X position.
   *
   * @return the X position
   */
  public String getPosx() {
    return get(POSX, true, String.class);
  }

  /**
   * specify the Y position for the dialog
   *
   * @param posy the Y position
   * @return the dialog
   */
  public Dialog setPosy(String posy) {
    set(POSY, posy);
    return this;
  }

  /**
   * Get the dialog Y position.
   *
   * @return the Y position
   */
  public String getPosy() {
    return get(POSY, true, String.class);
  }

  /**
   * Set the number of pixels to take into count before the dialog is considered outside the
   * viewport.
   *
   * @param snapThreshold the snap threshold
   * @return the dialog
   */
  public Dialog setSnapThreshold(int snapThreshold) {
    set(SNAP_THRESHOLD, snapThreshold);
    return this;
  }

  /**
   * Get the dialog snap threshold.
   *
   * @return the snap threshold
   */
  public int getSnapThreshold() {
    return get(SNAP_THRESHOLD);
  }

  /**
   * Set the dialog snap to edge.
   *
   * When true ten the dialog cannot be dragged outside the viewport.
   *
   * @param snapToEdge the snap to edge
   * @return the dialog
   */
  public Dialog setSnapToEdge(boolean snapToEdge) {
    set(SNAP_TO_EDGE, snapToEdge);
    return this;
  }

  /**
   * Get the dialog snap to edge.
   *
   * @return the snap to edge
   */
  public boolean isSnapToEdge() {
    return get(SNAP_TO_EDGE);
  }

  /**
   * Set the dialog theme.
   *
   * The theme name to use for the dialog.
   *
   * @param theme the theme
   * @return the dialog
   * @see Theme
   */
  public Dialog setTheme(Theme theme) {
    set(THEME, theme.getValue());
    return this;
  }

  /**
   * Get the dialog theme.
   *
   * @return the theme
   */
  public Theme getTheme() {
    return Theme.fromValue(get(THEME));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Dialog addClassName(String className) {
    addComponentClassName(className);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Dialog removeClassName(String className) {
    removeComponentClassName(className);
    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Dialog setStyle(String property, String value) {
    setComponentStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Dialog removeStyle(String property) {
    removeComponentStyle(property);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getStyle(String property) {
    return getComponentStyle(property);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getComputedStyle(String property) {
    return getComponentComputedStyle(property);
  }

  /**
   * Add a listener for the opened event
   *
   * @param listener the listener
   * @return the dialog
   */
  public Dialog addOpenListener(EventListener<DialogOpenEvent> listener) {
    addEventListener(DialogOpenEvent.class, listener);
    return this;
  }

  /**
   * Alias for {@link #addOpenListener(EventListener)}.
   *
   * @param listener the listener
   * @return the dialog
   */
  public Dialog onOpen(EventListener<DialogOpenEvent> listener) {
    return addOpenListener(listener);
  }

  /**
   * Remove a listener for the opened event
   *
   * @param listener the listener
   * @return the dialog
   */
  public Dialog removeOpenListener(EventListener<DialogOpenEvent> listener) {
    removeEventListener(DialogOpenEvent.class, listener);
    return this;
  }

  /**
   * Add a listener for the closed event
   *
   * @param listener the listener
   * @return the dialog
   */
  public Dialog addCloseListener(EventListener<DialogCloseEvent> listener) {
    addEventListener(DialogCloseEvent.class, listener);
    return this;
  }

  /**
   * Alias for {@link #addCloseListener(EventListener)}.
   *
   * @param listener the listener
   * @return the dialog
   */
  public Dialog onClose(EventListener<DialogCloseEvent> listener) {
    return addCloseListener(listener);
  }

  /**
   * Remove a listener for the closed event
   *
   * @param listener the listener
   * @return the dialog
   */
  public Dialog removeCloseListener(EventListener<DialogCloseEvent> listener) {
    removeEventListener(DialogCloseEvent.class, listener);
    return this;
  }
}
