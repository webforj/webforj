package org.dwcj.component.drawer;

import org.dwcj.component.drawer.events.DrawerClosedEvent;
import org.dwcj.component.drawer.events.DrawerOpenedEvent;
import org.dwcj.component.panels.Div;
import org.dwcj.interfaces.HasClassName;
import org.dwcj.interfaces.HasStyle;
import org.dwcj.webcomponent.PropertyDescriptor;
import org.dwcj.webcomponent.WebComponent;
import org.dwcj.webcomponent.annotations.NodeName;
import org.dwcj.webcomponent.events.EventListener;

/**
 * The drawer component allows developers to create a container which slides
 * into the viewport to expose additional options and information.
 * There is no limit on the number of drawers that an application can create. In
 * this case drawers will be stacked above each other.
 * 
 * @author Hyyan Abo Fakher
 */
@NodeName("bbj-drawer")
public class Drawer extends WebComponent implements HasClassName, HasStyle {

  /**
   * The drawer placement.
   */
  public enum Placement {
    /** The drawer will be placed at the top of the viewport. */
    TOP("top"),

    /** The drawer will be placed at the top center of the viewport. */
    TOP_CENTER("top-center"),

    /** The drawer will be placed at the bottom of the viewport. */
    BOTTOM("bottom"),

    /** The drawer will be placed at the bottom center of the viewport. */
    BOTTOM_CENTER("bottom-center"),

    /** The drawer will be placed at the left of the viewport. */
    LEFT("left"),

    /** The drawer will be placed at the right of the viewport. */
    RIGHT("right");

    /** The drawer placement value. */
    private final String value;

    /**
     * Instantiates a new drawer placement.
     *
     * @param value the value
     */
    Placement(String value) {
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
    public static Placement fromValue(String value) {
      for (Placement placement : Placement.values()) {
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

  private final PropertyDescriptor<Boolean> AUTO_FOCUS = PropertyDescriptor.property("autoFocus", false);
  private final PropertyDescriptor<String> LABEL = PropertyDescriptor.property("label", "Drawer");
  private final PropertyDescriptor<String> MAX_SIZE = PropertyDescriptor.property("maxSize", "100%");
  private final PropertyDescriptor<Boolean> OPENED = PropertyDescriptor.property("opened", false);
  private final PropertyDescriptor<String> PLACEMENT = PropertyDescriptor.property("placement",
      Placement.LEFT.getValue());
  private final PropertyDescriptor<String> SIZE = PropertyDescriptor.property("size", "16EM");

  /**
   * Instantiates a new drawer.
   */
  public Drawer() {
    super();
    setContent(new Div());
  }

  /**
   * Set the drawer content
   * 
   * @param content The drawer content.
   * @return the drawer
   */
  public Drawer setContent(Div content) {
    addSlot(content);
    return this;
  }

  /**
   * Get the drawer content.
   * 
   * @return the drawer content
   */
  public Div getContent() {
    return (Div) getSlot();
  }

  /**
   * Set Drawer auto focus.
   * 
   * @param autoFocus When true then automatically focus the first focusable
   *                  element in the drawer.
   * @return the drawer
   */
  public Drawer setAutoFocus(boolean autoFocus) {
    set(AUTO_FOCUS, autoFocus);
    return this;
  }

  /**
   * Get Drawer auto focus.
   * 
   * @return the drawer auto focus
   */
  public boolean isAutoFocus() {
    return get(AUTO_FOCUS);
  }

  /**
   * Set Drawer label. (Used for accessibility)
   * 
   * @param label The drawer label.
   * @return the drawer
   */
  public Drawer setLabel(String label) {
    set(LABEL, label);
    return this;
  }

  /**
   * Get Drawer label.
   * 
   * @return the drawer label
   */
  public String getLabel() {
    return get(LABEL);
  }

  /**
   * Set Drawer max size.
   * 
   * @param maxSize The Drawer max width. Max width in case placement is `left` or
   *                `right` or max height in case placement is `top` or `bottom`.
   * @return the drawer
   */
  public Drawer setMaxSize(String maxSize) {
    set(MAX_SIZE, maxSize);
    return this;
  }

  /**
   * Get Drawer max size.
   * 
   * @return the drawer max size
   */
  public String getMaxSize() {
    return get(MAX_SIZE);
  }

  /**
   * Open or close the drawer.
   * 
   * @param opened When true, the drawer is shown.
   * @return the drawer
   */
  public Drawer toggle(boolean opened) {
    set(OPENED, opened);
    return this;
  }

  /**
   * Open the drawer.
   * 
   * @return the drawer
   */
  public Drawer open() {
    return toggle(true);
  }

  /**
   * Close the drawer.
   * 
   * @return the drawer
   */
  public Drawer close() {
    return toggle(false);
  }

  /**
   * Get Drawer opened.
   * 
   * @return the drawer opened
   */
  public boolean isOpened() {
    return get(OPENED, true, Boolean.class);
  }

  /**
   * Set Drawer placement.
   * 
   * @param placement The drawer placement.
   * @return the drawer
   */
  public Drawer setPlacement(Placement placement) {
    set(PLACEMENT, placement.getValue());
    return this;
  }

  /**
   * Get Drawer placement.
   * 
   * @return the drawer placement
   */
  public Placement getPlacement() {
    return Placement.fromValue(get(PLACEMENT));
  }

  /**
   * Set Drawer size.
   * 
   * @param size The Drawer size. Width in case placement is `left` or `right` or
   *             height in case placement is `top` or `bottom`.
   * @return the drawer
   */
  public Drawer setSize(String size) {
    set(SIZE, size);
    return this;
  }

  /**
   * Get Drawer size.
   * 
   * @return the drawer size
   */
  public String getSize() {
    return get(SIZE);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Drawer addClassName(String className) {
    addComponentClassName(className);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Drawer removeClassName(String className) {
    removeComponentClassName(className);
    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Drawer setStyle(String property, String value) {
    setComponentStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Drawer removeStyle(String property) {
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
   * Add Drawer opened listener.
   * 
   * @param listener the listener
   * @return the drawer
   */
  public Drawer addOpenedListener(EventListener<DrawerOpenedEvent> listener) {
    addEventListener(DrawerOpenedEvent.class, listener);
    return this;
  }

  /**
   * Alias for {@link #addOpenedListener(EventListener)}.
   * 
   * @param listener the listener
   * @return the drawer
   */
  public Drawer onOpen(EventListener<DrawerOpenedEvent> listener) {
    return addOpenedListener(listener);
  }

  /**
   * Remove Drawer opened listener.
   * 
   * @param listener the listener
   * @return the drawer
   */
  public Drawer removeOpenedListener(EventListener<DrawerOpenedEvent> listener) {
    removeEventListener(DrawerOpenedEvent.class, listener);
    return this;
  }

  /**
   * Add Drawer closed listener.
   * 
   * @param listener the listener
   * @return the drawer
   */
  public Drawer addClosedListener(EventListener<DrawerClosedEvent> listener) {
    addEventListener(DrawerClosedEvent.class, listener);
    return this;
  }

  /**
   * Alias for {@link #addClosedListener(EventListener)}.
   * 
   * @param listener the listener
   * @return the drawer
   */
  public Drawer onClose(EventListener<DrawerClosedEvent> listener) {
    return addClosedListener(listener);
  }

  /**
   * Remove Drawer closed listener.
   * 
   * @param listener the listener
   * @return the drawer
   */
  public Drawer removeClosedListener(EventListener<DrawerClosedEvent> listener) {
    removeEventListener(DrawerClosedEvent.class, listener);
    return this;
  }
}
