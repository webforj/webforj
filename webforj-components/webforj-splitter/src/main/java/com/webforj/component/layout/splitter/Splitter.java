package com.webforj.component.layout.splitter;

import com.google.gson.annotations.SerializedName;
import com.webforj.component.Component;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.layout.splitter.event.SplitterResizeEvent;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasVisibility;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.webstorage.LocalStorage;

/**
 * Splitter is a component that encapsulates two resizable components separated by a divider that
 * enables the user to dynamically resize them.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@NodeName("dwc-splitter")
public class Splitter extends ElementComposite
    implements HasClassName<Splitter>, HasStyle<Splitter>, HasVisibility<Splitter> {

  /**
   * The layout's orientation.
   */
  public enum Orientation {
    @SerializedName("horizontal")
    HORIZONTAL,

    @SerializedName("vertical")
    VERTICAL
  }

  private Component master;
  private Component detail;

  // Slots
  private static final String DETAIL_SLOT = "detail";
  private static final String MASTER_SLOT = "master";

  // Property descriptors
  private final PropertyDescriptor<Boolean> autosaveProp =
      PropertyDescriptor.property("autosave", false);
  private final PropertyDescriptor<String> masterMaxSizeProp =
      PropertyDescriptor.property("masterMaxSize", "");
  private final PropertyDescriptor<String> masterMinSizeProp =
      PropertyDescriptor.property("masterMinSize", "");
  private final PropertyDescriptor<Orientation> orientationProp =
      PropertyDescriptor.property("orientation", Orientation.HORIZONTAL);
  private final PropertyDescriptor<Double> positionRelativeProp =
      PropertyDescriptor.property("positionRelative", 50d);

  /**
   * Creates a new Splitter with the given id, master component and detail component.
   *
   * @param id the id of the splitter
   * @param master the master component
   * @param detail the detail component
   */
  public Splitter(String id, Component master, Component detail) {
    super();

    if (id != null) {
      getElement().setAttribute("id", id);
    }

    if (master != null) {
      setMaster(master);
    }

    if (detail != null) {
      setDetail(detail);
    }
  }

  /**
   * Creates a new Splitter with the given master and detail component.
   *
   * @param master the master component
   * @param detail the detail component
   */
  public Splitter(Component master, Component detail) {
    this(null, master, detail);
  }

  /**
   * Creates a new Splitter with the given id.
   *
   * @param id the id of the splitter
   */
  public Splitter(String id) {
    this(id, null, null);
  }

  /**
   * Creates a new Splitter.
   */
  public Splitter() {
    this(null, null, null);
  }

  /**
   * Sets the Splitter master component.
   *
   * @param component the component to set
   * @return the component itself
   */
  public Splitter setMaster(Component component) {
    if (master != null) {
      getElement().remove(master);
    }

    master = component;
    getElement().add(MASTER_SLOT, component);
    return this;
  }

  /**
   * Gets the Splitter master component.
   *
   * @return the master component
   */
  public Component getMaster() {
    return master;
  }

  /**
   * Sets the Splitter detail component.
   *
   * @param component the component to set
   * @return the component itself
   */
  public Splitter setDetail(Component component) {
    if (detail != null) {
      getElement().remove(detail);
    }

    detail = component;
    getElement().add(DETAIL_SLOT, component);
    return this;
  }

  /**
   * Gets the Splitter detail component.
   *
   * @return the detail component
   */
  public Component getDetail() {
    return detail;
  }

  /**
   * Activates/Deactivates state autosave.
   *
   * <p>
   * When true, the size of the panels will be saved in local storage and restored on reload.
   * </p>
   *
   * @param autosave true to activate the autosave feature, false to deactivate it
   * @return the component itself.
   */
  public Splitter setAutosave(boolean autosave) {
    set(autosaveProp, autosave);
    return this;
  }

  /**
   * Checks if the Splitter has autosave enabled.
   *
   * @return true if the drawer is popover, false otherwise
   */
  public boolean isAutosave() {
    return get(autosaveProp);
  }

  /**
   * Sets the maximum width of the master panel when orientation is horizontal and the maximum
   * height when vertical.
   *
   * @param masterMaxSize the maximum size of the master panel
   * @return the component itself
   */
  public Splitter setMasterMaxSize(String masterMaxSize) {
    set(masterMaxSizeProp, masterMaxSize);
    return this;
  }

  /**
   * Gets the maximum size of the master panel.
   *
   * @return the maximum size of the master panel
   */
  public String getMasterMaxSize() {
    return get(masterMaxSizeProp);
  }

  /**
   * Sets the minimum width of the master panel when orientation is horizontal and the minimum
   * height when vertical.
   *
   * @param masterMinSize the minimum size of the master panel
   * @return the component itself
   */
  public Splitter setMasterMinSize(String masterMinSize) {
    set(masterMinSizeProp, masterMinSize);
    return this;
  }

  /**
   * Gets the minimum size of the master panel.
   *
   * @return the minimum size of the master panel
   */
  public String getMasterMinSize() {
    return get(masterMinSizeProp);
  }

  /**
   * Sets the orientation of the splitter.
   *
   * @param orientation the orientation of the splitter
   * @return the component itself
   */
  public Splitter setOrientation(Orientation orientation) {
    set(orientationProp, orientation);
    return this;
  }

  /**
   * Gets the orientation of the splitter.
   *
   * @return the orientation of the splitter
   */
  public Orientation getOrientation() {
    return get(orientationProp);
  }

  /**
   * Sets the relative position of the splitter.
   *
   * @param positionRelative the relative position of the splitter
   * @return the component itself
   *
   * @throws IllegalArgumentException if the position is not between 0 and 100
   */
  public Splitter setPositionRelative(double positionRelative) {
    if (Double.isNaN(positionRelative) || positionRelative < 0 || positionRelative > 100) {
      throw new IllegalArgumentException("The position must be a number between 0 and 100");
    }

    set(positionRelativeProp, positionRelative);
    return this;
  }

  /**
   * Gets the relative position of the splitter.
   *
   * @return the relative position of the splitter
   */
  public double getPositionRelative() {
    return get(positionRelativeProp, true, Boolean.class);
  }

  /**
   * If autosave is enabled, the state of the splitter will be cleaned from the local storage.
   *
   * @return the component itself
   */
  public Splitter cleanState() {
    String id = getElement().getAttribute("id");
    if (id != null) {
      LocalStorage.getCurrent().remove("dwc-splitter-" + id);
    }

    return this;
  }

  /**
   * Adds a listener for the Splitter resize event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<SplitterResizeEvent> addResizeListener(
      EventListener<SplitterResizeEvent> listener) {
    return addEventListener(SplitterResizeEvent.class, listener);
  }

  /**
   * Alias for {@link #addResizeListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<SplitterResizeEvent> onResize(
      EventListener<SplitterResizeEvent> listener) {
    return addResizeListener(listener);
  }

  Element getOriginalElement() {
    return getElement();
  }
}
