package org.dwcj.component.drawer;

import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.component.drawer.Drawer;
import org.dwcj.component.drawer.event.DrawerCloseEvent;
import org.dwcj.component.drawer.event.DrawerOpenEvent;
import org.dwcj.component.element.Element;
import org.dwcj.component.element.ElementCompositeContainer;
import org.dwcj.component.element.PropertyDescriptor;
import org.dwcj.component.element.annotation.NodeName;
import org.dwcj.concern.HasClassName;
import org.dwcj.concern.HasStyle;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;

/**
 * The drawer component allows developers to create a container which slides into the viewport to
 * expose additional options and information. There is no limit on the number of drawers that an
 * application can create. In this case drawers will be stacked above each other.
 *
 * @author Hyyan Abo Fakher
 */
@NodeName("bbj-drawer")
public class Drawer extends ElementCompositeContainer
    implements HasClassName<Drawer>, HasStyle<Drawer> {

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
  private final PropertyDescriptor<String> sizeProp = PropertyDescriptor.property("size", "16em");

  /**
   * Instantiates a new drawer.
   */
  public Drawer() {
    super();
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
  @ExcludeFromJacocoGeneratedReport
  public Drawer addClassName(String className) {
    getElement().addClassName(className);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Drawer removeClassName(String className) {
    getElement().removeClassName(className);
    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Drawer setStyle(String property, String value) {
    getElement().setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Drawer removeStyle(String property) {
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
  public ListenerRegistration<DrawerOpenEvent> addOpenListener(
      EventListener<DrawerOpenEvent> listener) {
    return addEventListener(DrawerOpenEvent.class, listener);
  }

  /**
   * Alias for {@link #addOpenListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DrawerOpenEvent> onOpen(EventListener<DrawerOpenEvent> listener) {
    return addOpenListener(listener);
  }

  /**
   * Adds a listener for the closed event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DrawerCloseEvent> addCloseListener(
      EventListener<DrawerCloseEvent> listener) {
    return addEventListener(DrawerCloseEvent.class, listener);
  }

  /**
   * Alias for {@link #addCloseListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DrawerCloseEvent> onClose(EventListener<DrawerCloseEvent> listener) {
    return addCloseListener(listener);
  }

  Element getOriginalElement() {
    return getElement();
  }
}
