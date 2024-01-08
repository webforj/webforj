package org.dwcj.component.layout.applayout;

import com.google.gson.annotations.SerializedName;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.component.Component;
import org.dwcj.component.element.Element;
import org.dwcj.component.element.ElementCompositeContainer;
import org.dwcj.component.element.PropertyDescriptor;
import org.dwcj.component.element.annotation.NodeName;
import org.dwcj.component.layout.applayout.event.AppLayoutDrawerCloseEvent;
import org.dwcj.component.layout.applayout.event.AppLayoutDrawerOpenEvent;
import org.dwcj.concern.HasClassName;
import org.dwcj.concern.HasStyle;
import org.dwcj.concern.HasVisibility;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;

/**
 * AppLayout is a web component that provides a responsive layout for web apps.
 *
 * <p>
 * The layout is responsive and it provides a header, a footer , a drawer, and content area. The
 * header and footer are fixed and the drawer slides in and out of the viewport and the content is
 * scrollable.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@NodeName("bbj-app-layout")
public class AppLayout extends ElementCompositeContainer
    implements HasClassName<AppLayout>, HasStyle<AppLayout>, HasVisibility<AppLayout> {

  /**
   * The drawer placement.
   */
  public enum DrawerPlacement {
    /** The drawer is placed on the right side. */
    @SerializedName("right")
    RIGHT,

    /** The drawer is placed on the left side. */
    @SerializedName("left")
    LEFT,

    /** The drawer is hidden. */
    @SerializedName("hidden")
    HIDDEN;
  }

  /**
   * The header and footer shadow.
   */
  public enum Shadow {
    /**
     * No shadow is applied.
     */
    @SerializedName("none")
    HIDDEN,
    /**
     * The shadow is applied when the content is scrolled.
     */
    @SerializedName("scroll")
    SCROLL,
    /**
     * The shadow is always applied.
     */
    @SerializedName("always")
    ALWAYS;
  }

  // Slots
  private static final String HEADER_SLOT = "header";
  private static final String FOOTER_SLOT = "footer";
  private static final String DRAWER_SLOT = "drawer";
  private static final String DRAWER_TITLE_SLOT = "drawer-title";
  private static final String HEADER_ACTIONS_SLOT = "drawer-header-actions";
  private static final String DRAWER_FOOTER_SLOT = "drawer-footer";

  // Property descriptors
  private final PropertyDescriptor<Boolean> drawerOpenProp =
      PropertyDescriptor.property("drawerOpened", false);
  private final PropertyDescriptor<Boolean> drawerPopoverProp =
      PropertyDescriptor.property("drawerPopover", false);
  private final PropertyDescriptor<Boolean> drawerOverlayProp =
      PropertyDescriptor.property("drawerOverlay", false);
  private final PropertyDescriptor<DrawerPlacement> drawerPlacementProp =
      PropertyDescriptor.property("drawerPlacement", DrawerPlacement.LEFT);
  private final PropertyDescriptor<String> drawerBreakPointProp =
      PropertyDescriptor.property("drawerBreakpoint", "(max-width: 800px)");
  private final PropertyDescriptor<Boolean> drawerFooterVisibleProp =
      PropertyDescriptor.property("drawerFooterVisible", false);
  private final PropertyDescriptor<Boolean> drawerHeaderVisibleProp =
      PropertyDescriptor.property("drawerHeaderVisible", false);
  private final PropertyDescriptor<Shadow> footerShadowProp =
      PropertyDescriptor.property("footerShadow", Shadow.HIDDEN);
  private final PropertyDescriptor<Boolean> footerFixedProp =
      PropertyDescriptor.property("footerFixed", true);
  private final PropertyDescriptor<Boolean> footerOffscreenProp =
      PropertyDescriptor.property("footerOffscreen", true);
  private final PropertyDescriptor<Boolean> footerRevealProp =
      PropertyDescriptor.property("footerReveal", false);
  private final PropertyDescriptor<Shadow> headerShadowProp =
      PropertyDescriptor.property("headerShadow", Shadow.SCROLL);
  private final PropertyDescriptor<Boolean> headerFixedProp =
      PropertyDescriptor.property("headerFixed", true);
  private final PropertyDescriptor<Boolean> headerOffscreenProp =
      PropertyDescriptor.property("headerOffscreen", true);
  private final PropertyDescriptor<Boolean> headerRevealProp =
      PropertyDescriptor.property("headerReveal", false);

  /**
   * Instantiates a new app layout.
   */
  public AppLayout() {
    super();
    getElement().setAttribute("fit-viewport", "");
  }

  /**
   * Add the given component to the applayout header slot.
   *
   * @param component the component to add
   * @return the component itself
   */
  public AppLayout addToHeader(Component... component) {
    getElement().add(HEADER_SLOT, component);
    return this;
  }

  /**
   * Alias for {@link #add(Component)}.
   *
   * <p>
   * Add the given component to the applayout content slot.
   * </p>
   *
   * @param component the component to add
   * @return the component itself
   */
  public AppLayout addToContent(Component... component) {
    add(component);
    return this;
  }

  /**
   * Add the given component to the applayout footer slot.
   *
   * @param component the component to add
   * @return the component itself
   */
  public AppLayout addToFooter(Component... component) {
    getElement().add(FOOTER_SLOT, component);
    return this;
  }

  /**
   * Add the given component to the applayout drawer slot.
   *
   * @param component the component to add
   * @return the component itself
   */
  public AppLayout addToDrawer(Component... component) {
    getElement().add(DRAWER_SLOT, component);
    return this;
  }

  /**
   * Add the given component to the applayout drawer title slot.
   *
   * @param component the component to add
   * @return the component itself
   */
  public AppLayout addToDrawerTitle(Component... component) {
    getElement().add(DRAWER_TITLE_SLOT, component);
    return this;
  }

  /**
   * Add the given component to the applayout drawer header actions slot.
   *
   * @param component the component to add
   * @return the component itself
   */
  public AppLayout addToDrawerHeaderActions(Component... component) {
    getElement().add(HEADER_ACTIONS_SLOT, component);
    return this;
  }

  /**
   * Add the given component to the applayout drawer footer slot.
   *
   * @param component the component to add
   * @return the component itself
   */
  public AppLayout addToDrawerFooter(Component... component) {
    getElement().add(DRAWER_FOOTER_SLOT, component);
    return this;
  }

  /**
   * Opens or closes the drawer based on the boolean passed.
   *
   * @param drawerOpened true to open the drawer, false to close it
   * @return the app layout
   */
  public AppLayout setDrawerOpened(boolean drawerOpened) {
    set(drawerOpenProp, drawerOpened);
    return this;
  }

  /**
   * Check if the drawer is opened.
   *
   * @return true if the drawer is opened, false otherwise
   */
  public boolean isDrawerOpened() {
    // always return the property value from the client side.
    return get(drawerOpenProp, true, Boolean.class);
  }

  /**
   * Set drawer popover.
   *
   * @param drawerPopover When true, then the drawer will pop over the header , footer and the
   *        content area.
   * @return the app layout
   */
  public AppLayout setDrawerPopover(boolean drawerPopover) {
    set(drawerPopoverProp, drawerPopover);
    return this;
  }

  /**
   * Check if the drawer is popover.
   *
   * @return true if the drawer is popover, false otherwise
   */
  public boolean isDrawerPopover() {
    // always return the property value from the client side.
    return get(drawerPopoverProp, true, Boolean.class);
  }

  /**
   * Set the drawer overlay.
   *
   * @param drawerOverlay When true and the `drawerPopover` is enabled then the drawer will have an
   *        overlay behind it when it is opened.
   * @return the app layout
   * @see #setDrawerPopover(boolean)
   */
  public AppLayout setDrawerOverlay(boolean drawerOverlay) {
    set(drawerOverlayProp, drawerOverlay);
    return this;
  }

  /**
   * Check if the drawer is overlay.
   *
   * @return true if the drawer is overlay, false otherwise
   */
  public boolean isDrawerOverlay() {
    // always return the property value from the client side.
    return get(drawerOverlayProp, true, Boolean.class);
  }

  /**
   * Sets the drawer placement.
   *
   * @param drawerPlacement the drawer placement
   * @return the app layout
   */
  public AppLayout setDrawerPlacement(DrawerPlacement drawerPlacement) {
    set(drawerPlacementProp, drawerPlacement);
    return this;
  }

  /**
   * Gets the drawer placement.
   *
   * @return the drawer placement
   */
  public DrawerPlacement getDrawerPlacement() {
    return get(drawerPlacementProp);
  }

  /**
   * Set the drawer breakpoint.
   *
   * <p>
   * The breakpoint is a media query to control when the drawer will be switched to popover mode.
   * Useful for small screens.
   *
   * By default the drawer popover mode will be enabled when the screen size is `800px` or less.
   * </p>
   *
   * @param drawerBreakpoint the drawer breakpoint
   * @return the app layout
   */
  public AppLayout setDrawerBreakpoint(String drawerBreakpoint) {
    set(drawerBreakPointProp, drawerBreakpoint);
    return this;
  }

  /**
   * Gets the drawer breakpoint.
   *
   * @return the drawer breakpoint
   */
  public String getDrawerBreakpoint() {
    return get(drawerBreakPointProp);
  }

  /**
   * Sets the drawer footer visible.
   *
   * @param drawerFooterVisible When true, the drawer footer will be visible.
   * @return the app layout
   */
  public AppLayout setDrawerFooterVisible(boolean drawerFooterVisible) {
    set(drawerFooterVisibleProp, drawerFooterVisible);
    return this;
  }

  /**
   * Check if the drawer footer is visible.
   *
   * @return true if the drawer footer is visible, false otherwise
   */
  public boolean isDrawerFooterVisible() {
    return get(drawerFooterVisibleProp);
  }

  /**
   * Sets the drawer header visible.
   *
   * @param drawerHeaderVisible When true, the drawer header will be visible.
   * @return the app layout
   */
  public AppLayout setDrawerHeaderVisible(boolean drawerHeaderVisible) {
    set(drawerHeaderVisibleProp, drawerHeaderVisible);
    return this;
  }

  /**
   * Check if the drawer header is visible.
   *
   * @return true if the drawer header is visible, false otherwise
   */
  public boolean isDrawerHeaderVisible() {
    return get(drawerHeaderVisibleProp);
  }

  /**
   * Sets the footer shadow.
   *
   * @param footerShadow the footer shadow
   * @return the app layout
   */
  public AppLayout setFooterShadow(Shadow footerShadow) {
    set(footerShadowProp, footerShadow);
    return this;
  }

  /**
   * Gets the footer shadow.
   *
   * @return the footer shadow
   */
  public Shadow getFooterShadow() {
    return get(footerShadowProp);
  }

  /**
   * Sets the footer fixed.
   *
   * @param footerFixed When true, the footer will be fixed at the top and won't move when the user
   *        scrolls.
   * @return the app layout
   */
  public AppLayout setFooterFixed(boolean footerFixed) {
    set(footerFixedProp, footerFixed);
    return this;
  }

  /**
   * Check if the footer is fixed.
   *
   * @return true if the footer is fixed, false otherwise
   */
  public boolean isFooterFixed() {
    return get(footerFixedProp);
  }

  /**
   * Sets the footer offscreen.
   *
   * @param footerOffscreen When true, the footer position will be shifted to fit beside the opened
   *        drawer.
   * @return the app layout
   */
  public AppLayout setFooterOffscreen(boolean footerOffscreen) {
    set(footerOffscreenProp, footerOffscreen);
    return this;
  }

  /**
   * Checks if the footer is offscreen.
   *
   * @return true if the footer is offscreen, false otherwise
   */
  public boolean isFooterOffscreen() {
    return get(footerOffscreenProp);
  }

  /**
   * Sets the footer reveal.
   *
   * @param footerReveal When true, the footer will be revealed when the user scrolls down.
   * @return the app layout
   */
  public AppLayout setFooterReveal(boolean footerReveal) {
    set(footerRevealProp, footerReveal);
    return this;
  }

  /**
   * Checks if the footer is reveal.
   *
   * @return true if the footer is reveal, false otherwise
   */
  public boolean isFooterReveal() {
    return get(footerRevealProp);
  }

  /**
   * Sets the header shadow.
   *
   * @param headerShadow the header shadow
   * @return the app layout
   */
  public AppLayout setHeaderShadow(Shadow headerShadow) {
    set(headerShadowProp, headerShadow);
    return this;
  }

  /**
   * Gets the header shadow.
   *
   * @return the header shadow
   */
  public Shadow getHeaderShadow() {
    return get(headerShadowProp);
  }

  /**
   * Sets the header fixed.
   *
   * @param headerFixed When true, the header will be fixed at the top and won't move when the user
   *        scrolls.
   * @return the app layout
   */
  public AppLayout setHeaderFixed(boolean headerFixed) {
    set(headerFixedProp, headerFixed);
    return this;
  }

  /**
   * Checks if the header is fixed.
   *
   * @return true if the header is fixed, false otherwise
   */
  public boolean isHeaderFixed() {
    return get(headerFixedProp);
  }

  /**
   * Sets the header offscreen.
   *
   * @param headerOffscreen When true, the header position will be shifted to fit beside the opened
   *        drawer.
   * @return the app layout
   */
  public AppLayout setHeaderOffscreen(boolean headerOffscreen) {
    set(headerOffscreenProp, headerOffscreen);
    return this;
  }

  /**
   * Checks if the header is offscreen.
   *
   * @return true if the header is offscreen, false otherwise
   */
  public boolean isHeaderOffscreen() {
    return get(headerOffscreenProp);
  }

  /**
   * Sets the header reveal.
   *
   * @param headerReveal When true, the header will be revealed when the user scrolls up.
   * @return the app layout
   */
  public AppLayout setHeaderReveal(boolean headerReveal) {
    set(headerRevealProp, headerReveal);
    return this;
  }

  /**
   * Checks if the header is reveal.
   *
   * @return true if the header is reveal, false otherwise
   */
  public boolean isHeaderReveal() {
    return get(headerRevealProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public AppLayout addClassName(String className) {
    getElement().addClassName(className);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public AppLayout removeClassName(String className) {
    getElement().removeClassName(className);
    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public AppLayout setStyle(String property, String value) {
    getElement().setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public AppLayout removeStyle(String property) {
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
   * {@inheritDoc}
   */
  @Override
  public boolean isVisible() {
    return getElement().isVisible();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AppLayout setVisible(boolean visible) {
    getElement().setVisible(visible);
    return this;
  }

  /**
   * Adds a listener for the drawer opened event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<AppLayoutDrawerOpenEvent> addDrawerOpenListener(
      EventListener<AppLayoutDrawerOpenEvent> listener) {
    return addEventListener(AppLayoutDrawerOpenEvent.class, listener);
  }

  /**
   * Alias for {@link #addDrawerOpenListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<AppLayoutDrawerOpenEvent> onDrawerOpen(
      EventListener<AppLayoutDrawerOpenEvent> listener) {
    return addDrawerOpenListener(listener);
  }

  /**
   * Adds a listener for the drawer close event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<AppLayoutDrawerCloseEvent> addDrawerCloseListener(
      EventListener<AppLayoutDrawerCloseEvent> listener) {
    return addEventListener(AppLayoutDrawerCloseEvent.class, listener);
  }

  /**
   * Alias for {@link #addDrawerCloseListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<AppLayoutDrawerCloseEvent> onDrawerClose(
      EventListener<AppLayoutDrawerCloseEvent> listener) {
    return addDrawerCloseListener(listener);
  }

  Element getOriginalElement() {
    return getElement();
  }
}
