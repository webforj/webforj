package org.dwcj.component.drawer;

import org.dwcj.component.drawer.event.DrawerCloseEvent;
import org.dwcj.component.drawer.event.DrawerOpenEvent;
import org.dwcj.component.event.ComponentEventListener;
import org.dwcj.component.webcomponent.PropertyDescriptor;
import org.dwcj.component.webcomponent.WebComponent;
import org.dwcj.component.webcomponent.annotation.NodeName;
import org.dwcj.component.window.Panel;
import org.dwcj.concern.HasClassName;
import org.dwcj.concern.HasStyle;

/**
 * The drawer component allows developers to create a container which slides into the viewport to
 * expose additional options and information. There is no limit on the number of drawers that an
 * application can create. In this case drawers will be stacked above each other.
 *
 * @author Hyyan Abo Fakher
 */
@NodeName("bbj-drawer")
public class Drawer extends WebComponent implements HasClassName<Drawer>, HasStyle<Drawer> {

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

  private final PropertyDescriptor<Boolean> autoFocusProp =
      PropertyDescriptor.property("autoFocus", false);
  private final PropertyDescriptor<String> labelProp =
      PropertyDescriptor.property("label", "Drawer");
  private final PropertyDescriptor<String> maxSizeProp =
      PropertyDescriptor.property("maxSize", "100%");
  private final PropertyDescriptor<Boolean> openedProp =
      PropertyDescriptor.property("opened", false);
  private final PropertyDescriptor<String> placementProp =
      PropertyDescriptor.property("placement", Placement.LEFT.getValue());
  private final PropertyDescriptor<String> sizeProp = PropertyDescriptor.property("size", "16EM");

  /**
   * Instantiates a new drawer.
   */
  public Drawer() {
    super();
    setContent(new Panel());
  }

  /**
   * Sets the drawer content.
   *
   * @param content The drawer content.
   * @return the drawer
   */
  public Drawer setContent(Panel content) {
    addSlot(content);
    return this;
  }

  /**
   * Gets the drawer content.
   *
   * @return the drawer content
   */
  public Panel getContent() {
    return (Panel) getSlot();
  }

  /**
   * Sets Drawer auto focus.
   *
   * @param autoFocus When true then automatically focus the first focusable element in the drawer.
   * @return the drawer
   */
  public Drawer setAutoFocus(boolean autoFocus) {
    set(autoFocusProp, autoFocus);
    return this;
  }

  /**
   * Gets Drawer auto focus.
   *
   * @return the drawer auto focus
   */
  public boolean isAutoFocus() {
    return get(autoFocusProp);
  }

  /**
   * Sets Drawer label. (Used for accessibility)
   *
   * @param label The drawer label.
   * @return the drawer
   */
  public Drawer setLabel(String label) {
    set(labelProp, label);
    return this;
  }

  /**
   * Gets Drawer label.
   *
   * @return the drawer label
   */
  public String getLabel() {
    return get(labelProp);
  }

  /**
   * Sets Drawer max size.
   *
   * @param maxSize The Drawer max width. Max width in case placement is `left` or `right` or max
   *        height in case placement is `top` or `bottom`.
   * @return the drawer
   */
  public Drawer setMaxSize(String maxSize) {
    set(maxSizeProp, maxSize);
    return this;
  }

  /**
   * Gets Drawer max size.
   *
   * @return the drawer max size
   */
  public String getMaxSize() {
    return get(maxSizeProp);
  }

  /**
   * Opens or closes the drawer.
   *
   * @param opened When true, the drawer is shown.
   * @return the drawer
   */
  public Drawer toggle(boolean opened) {
    set(openedProp, opened);
    return this;
  }

  /**
   * Opens the drawer.
   *
   * @return the drawer
   */
  public Drawer open() {
    return toggle(true);
  }

  /**
   * Closes the drawer.
   *
   * @return the drawer
   */
  public Drawer close() {
    return toggle(false);
  }

  /**
   * Gets Drawer opened.
   *
   * @return the drawer opened
   */
  public boolean isOpened() {
    return get(openedProp, true, Boolean.class);
  }

  /**
   * Sets Drawer placement.
   *
   * @param placement The drawer placement.
   * @return the drawer
   */
  public Drawer setPlacement(Placement placement) {
    set(placementProp, placement.getValue());
    return this;
  }

  /**
   * Gets Drawer placement.
   *
   * @return the drawer placement
   */
  public Placement getPlacement() {
    return Placement.fromValue(get(placementProp));
  }

  /**
   * Sets Drawer size.
   *
   * @param size The Drawer size. Width in case placement is `left` or `right` or height in case
   *        placement is `top` or `bottom`.
   * @return the drawer
   */
  public Drawer setSize(String size) {
    set(sizeProp, size);
    return this;
  }

  /**
   * Gets Drawer size.
   *
   * @return the drawer size
   */
  public String getSize() {
    return get(sizeProp);
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
   * Adds Drawer opened listener.
   *
   * @param listener the listener
   * @return the drawer
   */
  public Drawer addOpenListener(ComponentEventListener<DrawerOpenEvent> listener) {
    addEventListener(DrawerOpenEvent.class, listener);
    return this;
  }

  /**
   * Alias for {@link #addOpenListener(ComponentEventListener)}.
   *
   * @param listener the listener
   * @return the drawer
   */
  public Drawer onOpen(ComponentEventListener<DrawerOpenEvent> listener) {
    return addOpenListener(listener);
  }

  /**
   * Removes Drawer opened listener.
   *
   * @param listener the listener
   * @return the drawer
   */
  public Drawer removeOpenListener(ComponentEventListener<DrawerOpenEvent> listener) {
    removeEventListener(DrawerOpenEvent.class, listener);
    return this;
  }

  /**
   * Adds Drawer closed listener.
   *
   * @param listener the listener
   * @return the drawer
   */
  public Drawer addCloseListener(ComponentEventListener<DrawerCloseEvent> listener) {
    addEventListener(DrawerCloseEvent.class, listener);
    return this;
  }

  /**
   * Alias for {@link #addCloseListener(ComponentEventListener)}.
   *
   * @param listener the listener
   * @return the drawer
   */
  public Drawer onClose(ComponentEventListener<DrawerCloseEvent> listener) {
    return addCloseListener(listener);
  }

  /**
   * Removes Drawer closed listener.
   *
   * @param listener the listener
   * @return the drawer
   */
  public Drawer removeCloseListener(ComponentEventListener<DrawerCloseEvent> listener) {
    removeEventListener(DrawerCloseEvent.class, listener);
    return this;
  }
}
