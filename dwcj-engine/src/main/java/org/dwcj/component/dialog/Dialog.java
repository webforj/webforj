package org.dwcj.component.dialog;

import com.google.gson.annotations.SerializedName;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.component.Component;
import org.dwcj.component.Theme;
import org.dwcj.component.dialog.event.DialogCloseEvent;
import org.dwcj.component.dialog.event.DialogOpenEvent;
import org.dwcj.component.element.Element;
import org.dwcj.component.element.ElementCompositeContainer;
import org.dwcj.component.element.PropertyDescriptor;
import org.dwcj.component.element.annotation.NodeName;
import org.dwcj.concern.HasClassName;
import org.dwcj.concern.HasStyle;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;

/**
 * A dialog component.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@NodeName("dwc-dialog")
public class Dialog extends ElementCompositeContainer
    implements HasClassName<Dialog>, HasStyle<Dialog> {

  /**
   * The dialog alignments.
   */
  public enum Alignment {
    /** The dialog will be aligned to the bottom of the screen. */
    @SerializedName("bottom")
    BOTTOM,

    /** The dialog will be aligned to the center of the screen. */
    @SerializedName("center")
    CENTER,

    /** The dialog will be aligned to the top of the screen. */
    @SerializedName("top")
    TOP;
  }

  // Slots
  private static final String HEADER_SLOT = "header";
  private static final String CONTENT_SLOT = "content";
  private static final String FOOTER_SLOT = "footer";

  // Properties
  private final PropertyDescriptor<Alignment> alignmentProp =
      PropertyDescriptor.property("alignment", Alignment.CENTER);
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
  private final PropertyDescriptor<Theme> theme =
      PropertyDescriptor.property("theme", Theme.DEFAULT);

  /**
   * Instantiates a new Dialog.
   */
  public Dialog() {
    // Make sure to append the dialog to the end of the body to avoid
    // any stacking issues.
    // This call should be made very early in the lifecycle of the component
    // before any events are added because moving the element will remove all
    // the event listeners
    getElement().whenDefined().thenAccept(c -> {
      getElement().executeJsAsync("document.body.appendChild(component)");
    });
  }

  /**
   * Add the given component to the dialog header slot.
   *
   * @param component the component to add
   * @return the component itself
   */
  public Dialog addToHeader(Component... component) {
    getElement().add(HEADER_SLOT, component);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void add(Component... components) {
    getElement().add(CONTENT_SLOT, components);
  }

  /**
   * Alias for {@link #add(Component)}.
   *
   * <p>
   * Add the given component to the dialog content slot.
   * </p>
   *
   * @param component the component to add
   * @return the component itself
   */
  public Dialog addToContent(Component... component) {
    add(component);
    return this;
  }

  /**
   * Add the given component to the dialog footer slot.
   *
   * @param component the component to add
   * @return the component itself
   */
  public Dialog addToFooter(Component... component) {
    getElement().add(FOOTER_SLOT, component);
    return this;
  }

  /**
   * Sets the dialog alignment.
   *
   * @param alignment the alignment
   * @return the component itself
   * @see Alignment
   */
  public Dialog setAlignment(Alignment alignment) {
    set(alignmentProp, alignment);
    return this;
  }

  /**
   * Gets the dialog alignment.
   *
   * @return the alignment
   * @see Alignment
   */
  public Alignment getAlignment() {
    return get(alignmentProp);
  }

  /**
   * Sets the dialog autofocus.
   *
   * <p>
   * When true then automatically focus the first focusable element in the dialog.
   * </p>
   *
   * @param autofocus the autofocus
   * @return the component itself
   */
  public Dialog setAutofocus(boolean autofocus) {
    set(autoFocusProp, autofocus);
    return this;
  }

  /**
   * Gets the dialog autofocus.
   *
   * @return the autofocus
   */
  public boolean isAutofocus() {
    return get(autoFocusProp);
  }

  /**
   * Sets whether the dialog will have a backdrop.
   *
   * <p>
   * When true then a backdrop will be displayed behind the dialog.
   * </p>
   *
   * @param backdrop the backdrop
   * @return the component itself
   */
  public Dialog setBackdrop(boolean backdrop) {
    set(backdropProp, backdrop);
    return this;
  }

  /**
   * Gets whether the dialog has a backdrop.
   *
   * @return the backdrop
   */
  public boolean isBackdrop() {
    return get(backdropProp);
  }

  /**
   * Sets whether the backdrop of the dialog should be blurred.
   *
   * <p>
   * When true then the dialog's backdrop will be blurred.
   * </p>
   *
   * @param blurred the blurred
   * @return the component itself
   */
  public Dialog setBlurred(boolean blurred) {
    set(blurredProp, blurred);
    return this;
  }

  /**
   * Gets whether the dialog's backdrop is blurred.
   *
   * @return the blurred
   */
  public boolean isBlurred() {
    return get(blurredProp);
  }

  /**
   * Sets the dialog's breakpoint.
   *
   * <p>
   * A media query to control when the dialog will automatically flip to the full screen mode.When
   * the media query matches, the dialog will be full screen, otherwise it will be positioned.When
   * the auto full screen is enabled, the dialog cannot be moved or positioned.
   * </p>
   *
   * @param breakpoint the breakpoint
   * @return the component itself
   */
  public Dialog setBreakpoint(String breakpoint) {
    set(breakpointProp, breakpoint);
    return this;
  }

  /**
   * Gets the dialog's breakpoint.
   *
   * @return the breakpoint
   */
  public String getBreakpoint() {
    return get(breakpointProp);
  }

  /**
   * Sets whether the dialog is cancelled when the escape key is pressed.
   *
   * <p>
   * When true then the dialog can be cancelled by pressing the esc key.
   * </p>
   *
   * @param cancelOnEscKey the cancel on esc key
   * @return the component itself
   */
  public Dialog setCancelOnEscKey(boolean cancelOnEscKey) {
    set(cancelOnEscKeyProp, cancelOnEscKey);
    return this;
  }

  /**
   * Gets whether the dialog is cancelled when the escape key is pressed.
   *
   * @return the cancel on esc key
   */
  public boolean isCancelOnEscKey() {
    return get(cancelOnEscKeyProp);
  }

  /**
   * Sets whether the dialog cancels on outside click.
   *
   * <p>
   * When true then the dialog can be cancelled by clicking outside of it.
   * </p>
   *
   * @param cancelOnOutsideClick the cancel on outside click
   * @return the component itself
   */
  public Dialog setCancelOnOutsideClick(boolean cancelOnOutsideClick) {
    set(cancelOnOutsideClickProp, cancelOnOutsideClick);
    return this;
  }

  /**
   * Gets whether the dialog cancels on outside click.
   *
   * @return the cancel on outside click
   */
  public boolean isCancelOnOutsideClick() {
    return get(cancelOnOutsideClickProp);
  }

  /**
   * Sets whether closing the dialog by clicking outside or pressing the esc key is enabled.
   *
   * @param closeable the closeable
   * @return the component itself
   * @see #setCancelOnOutsideClick(boolean)
   * @see #setCancelOnEscKey(boolean)
   */
  public Dialog setCloseable(boolean closeable) {
    setCancelOnOutsideClick(closeable);
    setCancelOnEscKey(closeable);
    return this;
  }

  /**
   * Sets the dialog to be fullscreen.
   *
   * <p>
   * When true then the dialog will be fullscreen.
   * </p>
   *
   * @param fullscreen the fullscreen
   * @return the component itself
   */
  public Dialog setFullscreen(boolean fullscreen) {
    set(fullscreenProp, fullscreen);
    return this;
  }

  /**
   * Gets whether the dialog is set to be fullscreen.
   *
   * @return the fullscreen
   */
  public boolean isFullscreen() {
    return get(fullscreenProp, true, Boolean.class);
  }

  /**
   * Sets the dialog's max height.
   *
   * <p>
   * The maximum height of the dialog.
   * </p>
   *
   * @param maxHeight the max height
   * @return the component itself
   */
  public Dialog setMaxHeight(String maxHeight) {
    set(maxHeightProp, maxHeight);
    return this;
  }

  /**
   * Gets the dialog's max height.
   *
   * @return the max height
   */
  public String getMaxHeight() {
    return get(maxHeightProp);
  }

  /**
   * Sets the dialog's max width.
   *
   * <p>
   * The maximum width of the dialog.
   * </p>
   *
   * @param maxWidth the max width
   * @return the component itself
   */
  public Dialog setMaxWidth(String maxWidth) {
    set(maxWidthProp, maxWidth);
    return this;
  }

  /**
   * Gets the dialog's max width.
   *
   * @return the max width
   */
  public String getMaxWidth() {
    return get(maxWidthProp);
  }

  /**
   * Makes the dialog moveable.
   *
   * <p>
   * When true , then the dialog can be dragged to move it.
   * </p>
   *
   * @param moveable the moveable
   * @return the component itself
   */
  public Dialog setMoveable(boolean moveable) {
    set(moveableProp, moveable);
    return this;
  }

  /**
   * Gets whether the dialog is moveable.
   *
   * @return the moveable
   */
  public boolean isMoveable() {
    return get(moveableProp);
  }

  /**
   * Shows the dialog.
   *
   * @return the component itself
   */
  public Dialog open() {
    set(openedProp, true);
    return this;
  }

  /**
   * Hides the dialog.
   *
   * @return the component itself
   */
  public Dialog close() {
    set(openedProp, false);
    return this;
  }

  /**
   * Checks if the dialog is opened.
   *
   * @return true if the dialog is opened, false otherwise
   */
  public boolean isOpened() {
    return get(openedProp, true, Boolean.class);
  }

  /**
   * Specifies the X position for the dialog.
   *
   * @param posx the X position
   * @return the component itself
   */
  public Dialog setPosx(String posx) {
    set(posxProp, posx);
    return this;
  }

  /**
   * Gets the dialog X position.
   *
   * @return the X position
   */
  public String getPosx() {
    return get(posxProp, true, String.class);
  }

  /**
   * Specifies the Y position for the dialog.
   *
   * @param posy the Y position
   * @return the component itself
   */
  public Dialog setPosy(String posy) {
    set(posyProp, posy);
    return this;
  }

  /**
   * Gets the dialog Y position.
   *
   * @return the Y position
   */
  public String getPosy() {
    return get(posyProp, true, String.class);
  }

  /**
   * Sets the number of pixels to take into account before the dialog is considered outside the
   * viewport.
   *
   * @param snapThreshold the snap threshold
   * @return the component itself
   */
  public Dialog setSnapThreshold(int snapThreshold) {
    set(snapThresholdProp, snapThreshold);
    return this;
  }

  /**
   * Gets the dialog snap threshold.
   *
   * @return the snap threshold
   */
  public int getSnapThreshold() {
    return get(snapThresholdProp);
  }

  /**
   * Sets whether the dialog will snap to the edge of the screen.
   *
   * <p>
   * When true ten the dialog cannot be dragged outside the viewport.
   * </p>
   *
   * @param snapToEdge the snap to edge
   * @return the component itself
   */
  public Dialog setSnapToEdge(boolean snapToEdge) {
    set(snapToEdgeProp, snapToEdge);
    return this;
  }

  /**
   * Get whether the dialog will snap to the edge of the screen.
   *
   * @return the snap to edge
   */
  public boolean isSnapToEdge() {
    return get(snapToEdgeProp);
  }

  /**
   * Sets the dialog's theme.
   *
   * <p>
   * The theme name to use for the dialog.
   * </p>
   *
   * @param theme the theme
   * @return the component itself
   * @see Theme
   */
  public Dialog setTheme(Theme theme) {
    set(this.theme, theme);
    return this;
  }

  /**
   * Gets the dialog's theme.
   *
   * @return the theme
   */
  public Theme getTheme() {
    return get(theme);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Dialog addClassName(String... className) {
    getElement().addClassName(className);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Dialog removeClassName(String... className) {
    getElement().removeClassName(className);
    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Dialog setStyle(String property, String value) {
    getElement().setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Dialog removeStyle(String property) {
    getElement().removeStyle(property);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public String getStyle(String property) {
    return getElement().getStyle(property);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public String getComputedStyle(String property) {
    return getElement().getComputedStyle(property);
  }

  /**
   * Adds a listener for the opened event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DialogOpenEvent> addOpenListener(
      EventListener<DialogOpenEvent> listener) {
    return addEventListener(DialogOpenEvent.class, listener);
  }

  /**
   * Alias for {@link #addOpenListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DialogOpenEvent> onOpen(EventListener<DialogOpenEvent> listener) {
    return addOpenListener(listener);
  }

  /**
   * Adds a listener for the closed event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DialogCloseEvent> addCloseListener(
      EventListener<DialogCloseEvent> listener) {
    return addEventListener(DialogCloseEvent.class, listener);
  }

  /**
   * Alias for {@link #addCloseListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DialogCloseEvent> onClose(EventListener<DialogCloseEvent> listener) {
    return addCloseListener(listener);
  }

  Element getOriginalElement() {
    return getElement();
  }
}
