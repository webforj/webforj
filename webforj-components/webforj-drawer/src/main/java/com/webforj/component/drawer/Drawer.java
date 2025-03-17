package com.webforj.component.drawer;

import com.google.gson.annotations.SerializedName;
import com.webforj.component.Component;
import com.webforj.component.drawer.Drawer;
import com.webforj.component.drawer.event.DrawerCloseEvent;
import com.webforj.component.drawer.event.DrawerOpenEvent;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.annotation.PropertyMethods;
import com.webforj.concern.HasAutoFocus;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasStyle;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

/**
 * The drawer component allows developers to create a container which slides into the viewport to
 * expose additional options and information. There is no limit on the number of drawers that an
 * application can create. In this case drawers will be stacked above each other.
 *
 * @author Hyyan Abo Fakher
 */
@NodeName("dwc-drawer")
public class Drawer extends ElementCompositeContainer
    implements HasClassName<Drawer>, HasStyle<Drawer>, HasAutoFocus<Drawer> {

  /**
   * The drawer placement.
   */
  public enum Placement {
    /** The drawer will be placed at the top of the viewport. */
    @SerializedName("top")
    TOP,

    /** The drawer will be placed at the top center of the viewport. */
    @SerializedName("top-center")
    TOP_CENTER,

    /** The drawer will be placed at the bottom of the viewport. */
    @SerializedName("bottom")
    BOTTOM,

    /** The drawer will be placed at the bottom center of the viewport. */
    @SerializedName("bottom-center")
    BOTTOM_CENTER,

    /** The drawer will be placed at the left of the viewport. */
    @SerializedName("left")
    LEFT,

    /** The drawer will be placed at the right of the viewport. */
    @SerializedName("right")
    RIGHT;
  }

  // Slots
  private static final String TITTLE_SLOT = "title";
  private static final String HEADER_ACTIONS_SLOT = "header-actions";
  private static final String FOOTER_SLOT = "footer";

  // Property descriptors
  @PropertyMethods(setter = "setAutoFocus", getter = "isAutoFocus")
  private final PropertyDescriptor<Boolean> autoFocusProp =
      PropertyDescriptor.property("autofocus", false);
  private final PropertyDescriptor<String> labelProp =
      PropertyDescriptor.property("label", "Drawer");
  private final PropertyDescriptor<Boolean> openedProp =
      PropertyDescriptor.property("opened", false);
  private final PropertyDescriptor<Placement> placementProp =
      PropertyDescriptor.property("placement", Placement.LEFT);

  /**
   * Instantiates a new drawer.
   */
  public Drawer() {
    super();
  }

  public Drawer(String label) {
    this();
    setLabel(label);
  }

  /**
   * Add the given component to the title slot.
   *
   * @param component the component to add
   * @return the component itself
   */
  public Drawer addToTitle(Component... component) {
    getElement().add(TITTLE_SLOT, component);
    return this;
  }

  /**
   * Add the given component to the header actions slot.
   *
   * @param component the component to add
   * @return the component itself
   */
  public Drawer addToHeaderActions(Component... component) {
    getElement().add(HEADER_ACTIONS_SLOT, component);
    return this;
  }

  /**
   * Add the given component to the footer slot.
   *
   * @param component the component to add
   * @return the component itself
   */
  public Drawer addToFooter(Component... component) {
    getElement().add(FOOTER_SLOT, component);
    return this;
  }

  /**
   * Auto focus the drawer when it is opened.
   *
   * <p>
   * When true then automatically focus the first focusable element in the drawer.
   * </p>
   *
   * @param autoFocus When true then automatically focus the first focusable element in the drawer.
   * @return the drawer
   */
  @Override
  public Drawer setAutoFocus(boolean autoFocus) {
    set(autoFocusProp, autoFocus);
    return this;
  }

  /**
   * Checks if the drawer should be auto focused when it is opened.
   *
   * @return the drawer auto focus
   */
  @Override
  public boolean isAutoFocus() {
    return get(autoFocusProp);
  }

  /**
   * Sets Drawer label.
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
    set(placementProp, placement);
    return this;
  }

  /**
   * Gets Drawer placement.
   *
   * @return the drawer placement
   */
  public Placement getPlacement() {
    return get(placementProp);
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
