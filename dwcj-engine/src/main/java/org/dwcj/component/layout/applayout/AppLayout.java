package org.dwcj.component.layout.applayout;

import org.dwcj.component.layout.applayout.event.AppLayoutDrawerCloseEvent;
import org.dwcj.component.layout.applayout.event.AppLayoutDrawerOpenEvent;
import org.dwcj.component.webcomponent.PropertyDescriptor;
import org.dwcj.component.webcomponent.WebComponent;
import org.dwcj.component.webcomponent.annotation.NodeAttribute;
import org.dwcj.component.webcomponent.annotation.NodeName;
import org.dwcj.component.window.Panel;
import org.dwcj.concern.HasStyle;
import org.dwcj.concern.legacy.LegacyHasAttribute;
import org.dwcj.concern.legacy.LegacyHasClassName;
import org.dwcj.concern.legacy.LegacyHasVisibility;
import org.dwcj.dispatcher.EventListener;

/**
 * AppLayout is a web component that provides a responsive layout for web apps.
 *
 * The layout is responsive and it provides a header, a footer , a drawer, and content area. The
 * header and footer are fixed and the drawer slides in and out of the viewport and the content is
 * scrollable.
 *
 * @author Hyyan Abo Fakher
 */
@NodeName("bbj-app-layout")
@NodeAttribute(name = "fit-viewport")
public class AppLayout extends WebComponent
    implements LegacyHasClassName, HasStyle, LegacyHasVisibility, LegacyHasAttribute {

  /**
   * The drawer placement.
   */
  public enum DrawerPlacement {
    /** The drawer is placed on the right side. */
    RIGHT("right"),

    /** The drawer is placed on the left side. */
    LEFT("left"),

    /** The drawer is hidden. */
    HIDDEN("hidden");

    /** The drawer placement value. */
    private final String value;

    /**
     * Instantiates a new drawer placement.
     *
     * @param value the value
     */
    DrawerPlacement(String value) {
      this.value = value;
    }

    /**
     * Gets the value.
     *
     * @return the value
     */
    public String getValue() {
      return value;
    }

    /**
     * Gets the drawer placement from value.
     *
     * @param value the value to parse
     * @return the drawer placement
     */
    public static DrawerPlacement fromValue(String value) {
      for (DrawerPlacement placement : DrawerPlacement.values()) {
        if (placement.getValue().equals(value)) {
          return placement;
        }
      }

      return null;
    }

    /**
     * Gets the drawer placement value as string.
     *
     * @return the string
     */
    @Override
    public String toString() {
      return value;
    }
  }

  /**
   * The header and footer shadow.
   */
  public enum Shadow {
    /**
     * No shadow is applied.
     */
    HIDDEN("none"),
    /**
     * The shadow is applied when the content is scrolled.
     */
    SCROLL("scroll"),
    /**
     * The shadow is always applied.
     */
    ALWAYS("always");

    /** The header shadow value. */
    private final String value;

    /**
     * Instantiates a new header shadow.
     *
     * @param value the value
     */
    Shadow(String value) {
      this.value = value;
    }

    /**
     * Gets the value.
     *
     * @return the value
     */
    public String getValue() {
      return value;
    }

    /**
     * Gets the header shadow from value.
     *
     * @param value the value to parse
     * @return the header shadow
     */
    public static Shadow fromValue(String value) {
      for (Shadow shadow : Shadow.values()) {
        if (shadow.getValue().equals(value)) {
          return shadow;
        }
      }

      return null;
    }

    /**
     * Gets the header shadow value as string.
     *
     * @return the string
     */
    @Override
    public String toString() {
      return value;
    }
  }

  // Panels
  private Panel header;
  private Panel drawer;
  private Panel content;
  private Panel footer;

  // Property descriptors
  private final PropertyDescriptor<Boolean> drawerOpenProp =
      PropertyDescriptor.property("drawerOpened", false);
  private final PropertyDescriptor<Boolean> drawerPopoverProp =
      PropertyDescriptor.property("drawerPopover", false);
  private final PropertyDescriptor<Boolean> drawerOverlayProp =
      PropertyDescriptor.property("drawerOverlay", false);
  private final PropertyDescriptor<String> drawerWidthProp =
      PropertyDescriptor.property("drawerWidth", "16em");
  private final PropertyDescriptor<String> drawerPlacementProp =
      PropertyDescriptor.property("drawerPlacement", DrawerPlacement.LEFT.getValue());
  private final PropertyDescriptor<String> drawerBreakPointProp =
      PropertyDescriptor.property("drawerBreakpoint", "(max-width: 800px)");
  private final PropertyDescriptor<String> footerShadowProp =
      PropertyDescriptor.property("footerShadow", Shadow.HIDDEN.getValue());
  private final PropertyDescriptor<Boolean> footerFixedProp =
      PropertyDescriptor.property("footerFixed", true);
  private final PropertyDescriptor<Boolean> footerOffscreenProp =
      PropertyDescriptor.property("footerOffscreen", true);
  private final PropertyDescriptor<Boolean> footerRevealProp =
      PropertyDescriptor.property("footerReveal", false);
  private final PropertyDescriptor<String> headerSahdowProp =
      PropertyDescriptor.property("headerShadow", Shadow.SCROLL.getValue());
  private final PropertyDescriptor<Boolean> headerFixedProp =
      PropertyDescriptor.property("headerFixed", true);
  private final PropertyDescriptor<Boolean> headerOffscreenProp =
      PropertyDescriptor.property("headerOffscreen", true);
  private final PropertyDescriptor<Boolean> headerRevealProp =
      PropertyDescriptor.property("headerReveal", false);

  private boolean visible = true;

  /**
   * Instantiates a new app layout.
   */
  public AppLayout() {
    super();

    this.setHeader(new Panel());
    this.setDrawer(new Panel());
    this.setContent(new Panel());
    this.setFooter(new Panel());
  }

  /**
   * Uses the passed panel in header slot.
   *
   * @param header the header panel
   * @return the app layout
   */
  public AppLayout setHeader(Panel header) {
    this.header = header;
    addSlot("header", header);
    return this;
  }

  /**
   * Gets the header panel instance.
   *
   * @return the header panel
   */
  public Panel getHeader() {
    return header;
  }

  /**
   * Uses the passed panel in drawer slot.
   *
   * @param drawer the drawer panel
   * @return the app layout
   */
  public AppLayout setDrawer(Panel drawer) {
    this.drawer = drawer;
    addSlot("drawer", drawer);
    return this;
  }

  /**
   * Gets the drawer panel instance.
   *
   * @return the drawer panel
   */
  public Panel getDrawer() {
    return drawer;
  }

  /**
   * Uses the passed panel in content slot.
   *
   * @param content the content panel
   * @return the app layout
   */
  public AppLayout setContent(Panel content) {
    this.content = content;
    addSlot(content);
    return this;
  }

  /**
   * Gets the content panel instance.
   *
   * @return the content panel
   */
  public Panel getContent() {
    return content;
  }

  /**
   * Uses the passed panel in footer slot.
   *
   * @param footer the footer panel
   * @return the app layout
   */
  public AppLayout setFooter(Panel footer) {
    this.footer = footer;
    addSlot("footer", footer);
    return this;
  }

  /**
   * Gets the footer panel instance.
   *
   * @return the footer panel
   */
  public Panel getFooter() {
    return footer;
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
   * Set the drawer width.
   *
   * <p>
   * If the drawer width set in CSS using the `--bbj-app-layout-drawer-width` custom property then
   * this property will be ignored.
   * </p>
   *
   * @param drawerWidth the drawer width
   * @return the app layout
   */
  public AppLayout setDrawerWidth(String drawerWidth) {
    set(drawerWidthProp, drawerWidth);
    return this;
  }

  /**
   * Gets the drawer width.
   *
   * @return the drawer width
   */
  public String getDrawerWidth() {
    return get(drawerWidthProp);
  }

  /**
   * Sets the drawer placement.
   *
   * @param drawerPlacement the drawer placement
   * @return the app layout
   */
  public AppLayout setDrawerPlacement(DrawerPlacement drawerPlacement) {
    set(drawerPlacementProp, drawerPlacement.getValue());
    return this;
  }

  /**
   * Gets the drawer placement.
   *
   * @return the drawer placement
   */
  public DrawerPlacement getDrawerPlacement() {
    return DrawerPlacement.fromValue(get(drawerPlacementProp));
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
   * Sets the footer shadow.
   *
   * @param footerShadow the footer shadow
   * @return the app layout
   */
  public AppLayout setFooterShadow(Shadow footerShadow) {
    set(footerShadowProp, footerShadow.getValue());
    return this;
  }

  /**
   * Gets the footer shadow.
   *
   * @return the footer shadow
   */
  public Shadow getFooterShadow() {
    return Shadow.fromValue(get(footerShadowProp));
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
    set(headerSahdowProp, headerShadow.getValue());
    return this;
  }

  /**
   * Gets the header shadow.
   *
   * @return the header shadow
   */
  public Shadow getHeaderShadow() {
    return Shadow.fromValue(get(headerSahdowProp));
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
   * {@inheritDoc}
   */
  @Override
  public LegacyHasClassName addClassName(String className) {
    addComponentClassName(className);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LegacyHasClassName removeClassName(String className) {
    removeComponentClassName(className);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AppLayout removeStyle(String property) {
    removeComponentStyle(property);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AppLayout setStyle(String property, String value) {
    setComponentStyle(property, value);
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
   * {@inheritDoc}
   */
  @Override
  public String getAttribute(String attribute) {
    return getComponentAttribute(attribute);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LegacyHasAttribute setAttribute(String attribute, String value) {
    setComponentAttribute(attribute, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LegacyHasAttribute removeAttribute(String attribute) {
    removeComponentAttribute(attribute);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Boolean isVisible() {
    return visible;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AppLayout setVisible(Boolean visible) {
    this.visible = visible;
    return setStyle("visibility", Boolean.TRUE.equals(visible) ? "visible" : "hidden");
  }

  /**
   * Adds a listener for the drawer opened event.
   *
   * @param listener the listener
   * @return the app layout
   */
  public AppLayout addDrawerOpenListener(EventListener<AppLayoutDrawerOpenEvent> listener) {
    addEventListener(AppLayoutDrawerOpenEvent.class, listener);
    return this;
  }

  /**
   * Alias for {@link #addDrawerOpenListener(EventListener)}.
   *
   * @param listener the listener
   * @return the app layout
   */
  public AppLayout onDrawerOpen(EventListener<AppLayoutDrawerOpenEvent> listener) {
    return addDrawerOpenListener(listener);
  }

  /**
   * Removes a listener for the drawer opened event.
   *
   * @param listener the listener
   * @return the app layout
   */
  public AppLayout removeDrawerOpenListener(EventListener<AppLayoutDrawerOpenEvent> listener) {
    removeEventListener(AppLayoutDrawerOpenEvent.class, listener);
    return this;
  }

  /**
   * Adds a listener for the drawer closed event.
   *
   * @param listener the listener
   * @return the app layout
   */
  public AppLayout addDrawerCloseListener(EventListener<AppLayoutDrawerCloseEvent> listener) {
    addEventListener(AppLayoutDrawerCloseEvent.class, listener);
    return this;
  }

  /**
   * Alias for {@link #addDrawerCloseListener(EventListener)}.
   *
   * @param listener the listener
   * @return the app layout
   */
  public AppLayout onDrawerClose(EventListener<AppLayoutDrawerCloseEvent> listener) {
    return addDrawerCloseListener(listener);
  }

  /**
   * Removes a listener for the drawer closed event.
   *
   * @param listener the listener
   * @return the app layout
   */
  public AppLayout removeDrawerCloseListener(EventListener<AppLayoutDrawerCloseEvent> listener) {
    removeEventListener(AppLayoutDrawerCloseEvent.class, listener);
    return this;
  }
}
