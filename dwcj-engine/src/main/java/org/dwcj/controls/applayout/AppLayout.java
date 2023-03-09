package org.dwcj.controls.applayout;

import org.dwcj.controls.applayout.events.AppLayoutDrawerClosedEvent;
import org.dwcj.controls.applayout.events.AppLayoutDrawerOpenedEvent;

import org.dwcj.interfaces.HasClassName;
import org.dwcj.interfaces.HasStyle;
import org.dwcj.interfaces.HasVisibility;
import org.dwcj.controls.panels.Div;
import org.dwcj.webcomponent.PropertyDescriptor;
import org.dwcj.webcomponent.WebComponent;
import org.dwcj.webcomponent.annotations.NodeAttribute;
import org.dwcj.webcomponent.annotations.NodeName;
import org.dwcj.webcomponent.events.EventListener;

/**
 * AppLayout is a web component that provides a responsive layout for web apps.
 * 
 * The layout is responsive and it provides a header, a footer , a drawer, and
 * content area. The header and footer are fixed and the drawer slides in and
 * out of the viewport and the content is scrollable.
 * 
 * @author Hyyan Abo Fakher
 */
@NodeName("bbj-app-layout")
@NodeAttribute(name = "fit-viewport")
public class AppLayout extends WebComponent implements HasClassName, HasStyle, HasVisibility {

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
  private Div header;
  private Div drawer;
  private Div content;
  private Div footer;

  // Property descriptors
  private final PropertyDescriptor<Boolean> DRAWER_OPENED = PropertyDescriptor.property("drawerOpened", false);
  private final PropertyDescriptor<Boolean> DRAWER_POPOVER = PropertyDescriptor.property("drawerPopover", false);
  private final PropertyDescriptor<Boolean> DRAWER_OVERLAY = PropertyDescriptor.property("drawerOverlay", false);
  private final PropertyDescriptor<String> DRAWER_WIDTH = PropertyDescriptor.property("drawerWidth", "16em");
  private final PropertyDescriptor<String> DRAWER_PLACEMENT = PropertyDescriptor.property("drawerPlacement",
      DrawerPlacement.LEFT.getValue());
  private final PropertyDescriptor<String> DRAWER_BREAKPOINT = PropertyDescriptor.property("drawerBreakpoint",
      "(max-width: 800px)");
  private final PropertyDescriptor<String> FOOTER_SHADOW = PropertyDescriptor.property("footerShadow",
      Shadow.HIDDEN.getValue());
  private final PropertyDescriptor<Boolean> FOOTER_FIXED = PropertyDescriptor.property("footerFixed", true);
  private final PropertyDescriptor<Boolean> FOOTER_OFFSCREEN = PropertyDescriptor.property("footerOffscreen", true);
  private final PropertyDescriptor<Boolean> FOOTER_REVEAL = PropertyDescriptor.property("footerReveal", false);
  private final PropertyDescriptor<String> HEADER_SHADOW = PropertyDescriptor.property("headerShadow",
      Shadow.SCROLL.getValue());
  private final PropertyDescriptor<Boolean> HEADER_FIXED = PropertyDescriptor.property("headerFixed", true);
  private final PropertyDescriptor<Boolean> HEADER_OFFSCREEN = PropertyDescriptor.property("headerOffscreen", true);
  private final PropertyDescriptor<Boolean> HEADER_REVEAL = PropertyDescriptor.property("headerReveal", false);

  private boolean visible = true;

  /**
   * Instantiates a new app layout.
   */
  public AppLayout() {
    super();

    this.setHeader(new Div());
    this.setDrawer(new Div());
    this.setContent(new Div());
    this.setFooter(new Div());
  }

  /**
   * Use the passed panel in header slot.
   * 
   * @param header the header panel
   * @return the app layout
   */
  public AppLayout setHeader(Div header) {
    this.header = header;
    addSlot("header", header);
    return this;
  }

  /**
   * Get the header panel instance.
   * 
   * @return the app layout
   */
  public Div getHeader() {
    return header;
  }

  /**
   * Use the passed panel in drawer slot.
   * 
   * @param drawer the drawer panel
   * @return the app layout
   */
  public AppLayout setDrawer(Div drawer) {
    this.drawer = drawer;
    addSlot("drawer", drawer);
    return this;
  }

  /**
   * Get the drawer panel instance.
   * 
   * @return the app layout
   */
  public Div getDrawer() {
    return drawer;
  }

  /**
   * Use the passed panel in content slot.
   * 
   * @param content the content panel
   * @return the app layout
   */
  public AppLayout setContent(Div content) {
    this.content = content;
    addSlot(content);
    return this;
  }

  /**
   * Get the content panel instance.
   * 
   * @return the app layout
   */
  public Div getContent() {
    return content;
  }

  /**
   * Use the passed panel in footer slot.
   * 
   * @param footer the footer panel
   * @return the app layout
   */
  public AppLayout setFooter(Div footer) {
    this.footer = footer;
    addSlot("footer", footer);
    return this;
  }

  /**
   * Get the footer panel instance.
   * 
   * @return the app layout
   */
  public Div getFooter() {
    return footer;
  }

  /**
   * Open/close the drawer.
   * 
   * @param drawerOpened true to open the drawer, false to close it
   * @return the app layout
   */
  public AppLayout setDrawerOpened(boolean drawerOpened) {
    set(DRAWER_OPENED, drawerOpened);
    return this;
  }

  /**
   * Check if the drawer is opened.
   * 
   * @return true if the drawer is opened, false otherwise
   */
  public boolean isDrawerOpened() {
    // always return the property value from the client side.
    return get(DRAWER_OPENED, true, Boolean.class);
  }

  /**
   * Set drawer popover.
   * 
   * @param drawerPopover When true, then the drawer will pop over
   *                      the header , footer and the content area.
   * @return the app layout
   */
  public AppLayout setDrawerPopover(boolean drawerPopover) {
    set(DRAWER_POPOVER, drawerPopover);
    return this;
  }

  /**
   * Check if the drawer is popover.
   * 
   * @return true if the drawer is popover, false otherwise
   */
  public boolean isDrawerPopover() {
    // always return the property value from the client side.
    return get(DRAWER_POPOVER, true, Boolean.class);
  }

  /**
   * Set the drawer overlay.
   * 
   * @param drawerOverlay When true and the `drawerPopover` is enabled then the
   *                      drawer will have an overlay behind it when it is opened.
   * @return the app layout
   * @see #setDrawerPopover(boolean)
   */
  public AppLayout setDrawerOverlay(boolean drawerOverlay) {
    set(DRAWER_OVERLAY, drawerOverlay);
    return this;
  }

  /**
   * Check if the drawer is overlay.
   * 
   * @return true if the drawer is overlay, false otherwise
   */
  public boolean isDrawerOverlay() {
    // always return the property value from the client side.
    return get(DRAWER_OVERLAY, true, Boolean.class);
  }

  /**
   * Set the drawer width.
   * 
   * If the drawer width set in CSS using the `--bbj-app-layout-drawer-width`
   * custom property then this property will be ignored.
   * 
   * @param drawerWidth the drawer width
   * @return the app layout
   */
  public AppLayout setDrawerWidth(String drawerWidth) {
    set(DRAWER_WIDTH, drawerWidth);
    return this;
  }

  /**
   * Get the drawer width.
   * 
   * @return the drawer width
   */
  public String getDrawerWidth() {
    return get(DRAWER_WIDTH);
  }

  /**
   * Set the drawer placement.
   * 
   * @param drawerPlacement the drawer placement
   * @return the app layout
   */
  public AppLayout setDrawerPlacement(DrawerPlacement drawerPlacement) {
    set(DRAWER_PLACEMENT, drawerPlacement.getValue());
    return this;
  }

  /**
   * Get the drawer placement.
   * 
   * @return the drawer placement
   */
  public DrawerPlacement getDrawerPlacement() {
    return DrawerPlacement.fromValue(get(DRAWER_PLACEMENT));
  }

  /**
   * Set the drawer breakpoint.
   * 
   * The breakpoint is a media query to control when the drawer will be switched
   * to popover mode. Useful for small screens.
   * 
   * By default the drawer popover mode will be enabled
   * when the screen size is `800px` or less.
   * 
   * @param drawerBreakpoint the drawer breakpoint
   * @return the app layout
   */
  public AppLayout setDrawerBreakpoint(String drawerBreakpoint) {
    set(DRAWER_BREAKPOINT, drawerBreakpoint);
    return this;
  }

  /**
   * Get the drawer breakpoint.
   * 
   * @return the drawer breakpoint
   */
  public String getDrawerBreakpoint() {
    return get(DRAWER_BREAKPOINT);
  }

  /**
   * Set the footer shadow.
   * 
   * @param footerShadow the footer shadow
   * @return the app layout
   */
  public AppLayout setFooterShadow(Shadow footerShadow) {
    set(FOOTER_SHADOW, footerShadow.getValue());
    return this;
  }

  /**
   * Get the footer shadow.
   * 
   * @return the footer shadow
   */
  public Shadow getFooterShadow() {
    return Shadow.fromValue(get(FOOTER_SHADOW));
  }

  /**
   * Set the footer fixed.
   * 
   * @param footerFixed When true, the footer will be fixed at the top and won't
   *                    move when the user scrolls.
   * @return the app layout
   */
  public AppLayout setFooterFixed(boolean footerFixed) {
    set(FOOTER_FIXED, footerFixed);
    return this;
  }

  /**
   * Check if the footer is fixed.
   * 
   * @return true if the footer is fixed, false otherwise
   */
  public boolean isFooterFixed() {
    return get(FOOTER_FIXED);
  }

  /**
   * Set the footer offscreen.
   * 
   * @param footerOffscreen When true, the footer position will be shifted to fit
   *                        beside the opened drawer.
   * @return the app layout
   */
  public AppLayout setFooterOffscreen(boolean footerOffscreen) {
    set(FOOTER_OFFSCREEN, footerOffscreen);
    return this;
  }

  /**
   * Check if the footer is offscreen.
   * 
   * @return true if the footer is offscreen, false otherwise
   */
  public boolean isFooterOffscreen() {
    return get(FOOTER_OFFSCREEN);
  }

  /**
   * Set the footer reveal.
   * 
   * @param footerReveal When true, the footer will be revealed when the user
   *                     scrolls down.
   * @return the app layout
   */
  public AppLayout setFooterReveal(boolean footerReveal) {
    set(FOOTER_REVEAL, footerReveal);
    return this;
  }

  /**
   * Check if the footer is reveal.
   * 
   * @return true if the footer is reveal, false otherwise
   */
  public boolean isFooterReveal() {
    return get(FOOTER_REVEAL);
  }

  /**
   * Set the header shadow.
   * 
   * @param headerShadow the header shadow
   * @return the app layout
   */
  public AppLayout setHeaderShadow(Shadow headerShadow) {
    set(HEADER_SHADOW, headerShadow.getValue());
    return this;
  }

  /**
   * Get the header shadow.
   * 
   * @return the header shadow
   */
  public Shadow getHeaderShadow() {
    return Shadow.fromValue(get(HEADER_SHADOW));
  }

  /**
   * Set the header fixed.
   * 
   * @param headerFixed When true, the header will be fixed at the top and won't
   *                    move when the user scrolls.
   * @return the app layout
   */
  public AppLayout setHeaderFixed(boolean headerFixed) {
    set(HEADER_FIXED, headerFixed);
    return this;
  }

  /**
   * Check if the header is fixed.
   * 
   * @return true if the header is fixed, false otherwise
   */
  public boolean isHeaderFixed() {
    return get(HEADER_FIXED);
  }

  /**
   * Set the header offscreen.
   * 
   * @param headerOffscreen When true, the header position will be shifted to fit
   *                        beside the opened drawer.
   * @return the app layout
   */
  public AppLayout setHeaderOffscreen(boolean headerOffscreen) {
    set(HEADER_OFFSCREEN, headerOffscreen);
    return this;
  }

  /**
   * Check if the header is offscreen.
   * 
   * @return true if the header is offscreen, false otherwise
   */
  public boolean isHeaderOffscreen() {
    return get(HEADER_OFFSCREEN);
  }

  /**
   * Set the header reveal.
   * 
   * @param headerReveal When true, the header will be revealed when the user
   *                     scrolls up.
   * @return the app layout
   */
  public AppLayout setHeaderReveal(boolean headerReveal) {
    set(HEADER_REVEAL, headerReveal);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HasClassName addClassName(String className) {
    addComponentClassName(className);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HasClassName removeClassName(String className) {
    removeComponentClassName(className);
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
   * Add a listener for the drawer opened event.
   * 
   * @param listener the listener
   * @return the app layout
   */
  public AppLayout addDrawerOpenedListener(EventListener<AppLayoutDrawerOpenedEvent> listener) {
    addEventListener(AppLayoutDrawerOpenedEvent.class, listener);
    return this;
  }

  /**
   * Alias for {@link #addDrawerOpenedListener(EventListener)}.
   * 
   * @param listener the listener
   * @return the app layout
   */
  public AppLayout onDrawerOpen(EventListener<AppLayoutDrawerOpenedEvent> listener) {
    return addDrawerOpenedListener(listener);
  }

  /**
   * Remove a listener for the drawer opened event.
   * 
   * @param listener the listener
   * @return the app layout
   */
  public AppLayout removeDrawerOpenedListener(EventListener<AppLayoutDrawerOpenedEvent> listener) {
    removeEventListener(AppLayoutDrawerOpenedEvent.class, listener);
    return this;
  }

  /**
   * Add a listener for the drawer closed event.
   * 
   * @param listener the listener
   * @return the app layout
   */
  public AppLayout addDrawerClosedListener(EventListener<AppLayoutDrawerClosedEvent> listener) {
    addEventListener(AppLayoutDrawerClosedEvent.class, listener);
    return this;
  }

  /**
   * Alias for {@link #addDrawerClosedListener(EventListener)}.
   * 
   * @param listener the listener
   * @return the app layout
   */
  public AppLayout onDrawerClose(EventListener<AppLayoutDrawerClosedEvent> listener) {
    return addDrawerClosedListener(listener);
  }

  /**
   * Remove a listener for the drawer closed event.
   * 
   * @param listener the listener
   * @return the app layout
   */
  public AppLayout removeDrawerClosedListener(EventListener<AppLayoutDrawerClosedEvent> listener) {
    removeEventListener(AppLayoutDrawerClosedEvent.class, listener);
    return this;
  }
}
