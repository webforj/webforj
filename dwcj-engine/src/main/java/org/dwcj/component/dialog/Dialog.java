package org.dwcj.component.dialog;

import org.dwcj.component.dialog.event.DialogCloseEvent;
import org.dwcj.component.dialog.event.DialogOpenEvent;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.webcomponent.PropertyDescriptor;
import org.dwcj.component.webcomponent.WebComponent;
import org.dwcj.component.webcomponent.annotation.NodeName;
import org.dwcj.component.window.Panel;
import org.dwcj.concern.HasClassName;
import org.dwcj.concern.HasStyle;

/**
 * A dialog component.
 *
 * @author Hyyan Abo Fakher
 */
@NodeName("bbj-dialog")
public class Dialog extends WebComponent implements HasClassName, HasStyle {

  /**
   * The dialog alignments.
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
   * The dialog themes.
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
  private final PropertyDescriptor<String> alignmentProp =
      PropertyDescriptor.property("alignment", Alignment.CENTER.getValue());
  private final PropertyDescriptor<Boolean> autoFocusProp =
      PropertyDescriptor.property("autofocus", false);
  private final PropertyDescriptor<Boolean> backdropProp =
      PropertyDescriptor.property("backdrop", true);
  private final PropertyDescriptor<Boolean> blurredProp =
      PropertyDescriptor.property("blurred", false);
  private final PropertyDescriptor<String> breakpointProp =
      PropertyDescriptor.property("breakpoint", "");
  private final PropertyDescriptor<Boolean> cancelOnEscKeyProp =
      PropertyDescriptor.property("cancelOnEscKey", false);
  private final PropertyDescriptor<Boolean> cancelOnOutsideClickProp =
      PropertyDescriptor.property("cancelOnOutsideClick", true);
  private final PropertyDescriptor<Boolean> fullscreenProp =
      PropertyDescriptor.property("fullscreen", false);
  private final PropertyDescriptor<String> maxHeightProp =
      PropertyDescriptor.property("maxHeight", "");
  private final PropertyDescriptor<String> maxWidthProp =
      PropertyDescriptor.property("maxWidth", "");
  private final PropertyDescriptor<Boolean> moveableProp =
      PropertyDescriptor.property("moveable", true);
  private final PropertyDescriptor<Boolean> openedProp =
      PropertyDescriptor.property("opened", false);
  private final PropertyDescriptor<String> posxProp = PropertyDescriptor.property("posx", "");
  private final PropertyDescriptor<String> posyProp = PropertyDescriptor.property("posy", "");
  private final PropertyDescriptor<Integer> snapThresholdProp =
      PropertyDescriptor.property("snapThreshold", 0);
  private final PropertyDescriptor<Boolean> snapToEdgeProp =
      PropertyDescriptor.property("snapToEdge", true);
  private final PropertyDescriptor<String> theme =
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
    set(alignmentProp, alignment.getValue());
    return this;
  }

  /**
   * Get the dialog alignment.
   *
   * @return the alignment
   * @see Alignment
   */
  public Alignment getAlignment() {
    return Alignment.fromValue(get(alignmentProp));
  }

  /**
   * Set the dialog autofocus.
   *
   * <p>
   * When true then automatically focus the first focusable element in the dialog.
   * </p>
   *
   * @param autofocus the autofocus
   * @return the dialog
   */
  public Dialog setAutoFocus(boolean autofocus) {
    set(autoFocusProp, autofocus);
    return this;
  }

  /**
   * Get the dialog autofocus.
   *
   * @return the autofocus
   */
  public boolean isAutoFocus() {
    return get(autoFocusProp);
  }

  /**
   * Set the dialog backdrop.
   *
   * <p>
   * When true then a backdrop will be displayed behind the dialog.
   * </p>
   *
   * @param backdrop the backdrop
   * @return the dialog
   */
  public Dialog setBackdrop(boolean backdrop) {
    set(backdropProp, backdrop);
    return this;
  }

  /**
   * Get the dialog backdrop.
   *
   * @return the backdrop
   */
  public boolean isBackdrop() {
    return get(backdropProp);
  }

  /**
   * Set the dialog blurred.
   *
   * <p>
   * When true then the dialog's backdrop will be blurred.
   * </p>
   *
   * @param blurred the blurred
   * @return the dialog
   */
  public Dialog setBlurred(boolean blurred) {
    set(blurredProp, blurred);
    return this;
  }

  /**
   * Get the dialog blurred.
   *
   * @return the blurred
   */
  public boolean isBlurred() {
    return get(blurredProp);
  }

  /**
   * Set the dialog breakpoint.
   *
   * <p>
   * A media query to control when the dialog will automatically flip to the full screen mode.When
   * the media query matches, the dialog will be full screen, otherwise it will be positioned.When
   * the auto full screen is enabled, the dialog cannot be moved or positioned.
   * </p>
   *
   * @param breakpoint the breakpoint
   * @return the dialog
   */
  public Dialog setBreakpoint(String breakpoint) {
    set(breakpointProp, breakpoint);
    return this;
  }

  /**
   * Get the dialog breakpoint.
   *
   * @return the breakpoint
   */
  public String getBreakpoint() {
    return get(breakpointProp);
  }

  /**
   * Set the dialog cancel on esc key.
   *
   * <p>
   * When true then the dialog can be cancelled by pressing the esc key.
   * </p>
   *
   * @param cancelOnEscKey the cancel on esc key
   * @return the dialog
   */
  public Dialog setCancelOnEscKey(boolean cancelOnEscKey) {
    set(cancelOnEscKeyProp, cancelOnEscKey);
    return this;
  }

  /**
   * Get the dialog cancel on esc key.
   *
   * @return the cancel on esc key
   */
  public boolean isCancelOnEscKey() {
    return get(cancelOnEscKeyProp);
  }

  /**
   * Set the dialog cancel on outside click.
   *
   * <p>
   * When true then the dialog can be cancelled by clicking outside of it.
   * </p>
   *
   * @param cancelOnOutsideClick the cancel on outside click
   * @return the dialog
   */
  public Dialog setCancelOnOutsideClick(boolean cancelOnOutsideClick) {
    set(cancelOnOutsideClickProp, cancelOnOutsideClick);
    return this;
  }

  /**
   * Get the dialog cancel on outside click.
   *
   * @return the cancel on outside click
   */
  public boolean isCancelOnOutsideClick() {
    return get(cancelOnOutsideClickProp);
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
   * <p>
   * When true then the dialog will be fullscreen.
   * </p>
   *
   * @param fullscreen the fullscreen
   * @return the dialog
   */
  public Dialog setFullScreen(boolean fullscreen) {
    set(fullscreenProp, fullscreen);
    return this;
  }

  /**
   * Get the dialog fullscreen.
   *
   * @return the fullscreen
   */
  public boolean isFullScreen() {
    return get(fullscreenProp, true, Boolean.class);
  }

  /**
   * Set the dialog max height.
   *
   * <p>
   * The maximum height of the dialog.
   * </p>
   *
   * @param maxHeight the max height
   * @return the dialog
   */
  public Dialog setMaxHeight(String maxHeight) {
    set(maxHeightProp, maxHeight);
    return this;
  }

  /**
   * Get the dialog max height.
   *
   * @return the max height
   */
  public String getMaxHeight() {
    return get(maxHeightProp);
  }

  /**
   * Set the dialog max width.
   *
   * <p>
   * The maximum width of the dialog.
   * </p>
   *
   * @param maxWidth the max width
   * @return the dialog
   */
  public Dialog setMaxWidth(String maxWidth) {
    set(maxWidthProp, maxWidth);
    return this;
  }

  /**
   * Get the dialog max width.
   *
   * @return the max width
   */
  public String getMaxWidth() {
    return get(maxWidthProp);
  }

  /**
   * Make the dialog moveable.
   *
   * <p>
   * When true , then the dialog can be dragged to move it.
   * </p>
   *
   * @param moveable the moveable
   * @return the dialog
   */
  public Dialog setMoveable(boolean moveable) {
    set(moveableProp, moveable);
    return this;
  }

  /**
   * Get the dialog moveable.
   *
   * @return the moveable
   */
  public boolean isMoveable() {
    return get(moveableProp);
  }

  /**
   * Show the dialog.
   *
   * @return the dialog
   */
  public Dialog show() {
    set(openedProp, true);
    return this;
  }

  /**
   * Hide the dialog.
   *
   * @return the dialog
   */
  public Dialog hide() {
    set(openedProp, false);
    return this;
  }

  /**
   * Check if the dialog is opened.
   *
   * @return true if the dialog is opened, false otherwise
   */
  public boolean isOpened() {
    return get(openedProp, true, Boolean.class);
  }

  /**
   * specify the X position for the dialog.
   *
   * @param posx the X position
   * @return the dialog
   */
  public Dialog setPosx(String posx) {
    set(posxProp, posx);
    return this;
  }

  /**
   * Get the dialog X position.
   *
   * @return the X position
   */
  public String getPosx() {
    return get(posxProp, true, String.class);
  }

  /**
   * specify the Y position for the dialog.
   *
   * @param posy the Y position
   * @return the dialog
   */
  public Dialog setPosy(String posy) {
    set(posyProp, posy);
    return this;
  }

  /**
   * Get the dialog Y position.
   *
   * @return the Y position
   */
  public String getPosy() {
    return get(posyProp, true, String.class);
  }

  /**
   * Set the number of pixels to take into count before the dialog is considered outside the
   * viewport.
   *
   * @param snapThreshold the snap threshold
   * @return the dialog
   */
  public Dialog setSnapThreshold(int snapThreshold) {
    set(snapThresholdProp, snapThreshold);
    return this;
  }

  /**
   * Get the dialog snap threshold.
   *
   * @return the snap threshold
   */
  public int getSnapThreshold() {
    return get(snapThresholdProp);
  }

  /**
   * Set the dialog snap to edge.
   *
   * <p>
   * When true ten the dialog cannot be dragged outside the viewport.
   * </p>
   *
   * @param snapToEdge the snap to edge
   * @return the dialog
   */
  public Dialog setSnapToEdge(boolean snapToEdge) {
    set(snapToEdgeProp, snapToEdge);
    return this;
  }

  /**
   * Get the dialog snap to edge.
   *
   * @return the snap to edge
   */
  public boolean isSnapToEdge() {
    return get(snapToEdgeProp);
  }

  /**
   * Set the dialog theme.
   *
   * <p>
   * The theme name to use for the dialog.
   * </p>
   *
   * @param theme the theme
   * @return the dialog
   * @see Theme
   */
  public Dialog setTheme(Theme theme) {
    set(this.theme, theme.getValue());
    return this;
  }

  /**
   * Get the dialog theme.
   *
   * @return the theme
   */
  public Theme getTheme() {
    return Theme.fromValue(get(theme));
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
   * Add a listener for the opened event.
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
   * Remove a listener for the opened event.
   *
   * @param listener the listener
   * @return the dialog
   */
  public Dialog removeOpenListener(EventListener<DialogOpenEvent> listener) {
    removeEventListener(DialogOpenEvent.class, listener);
    return this;
  }

  /**
   * Add a listener for the closed event.
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
   * Remove a listener for the closed event.
   *
   * @param listener the listener
   * @return the dialog
   */
  public Dialog removeCloseListener(EventListener<DialogCloseEvent> listener) {
    removeEventListener(DialogCloseEvent.class, listener);
    return this;
  }
}
