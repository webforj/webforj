package com.webforj.component.layout.splitter;

import com.google.gson.annotations.SerializedName;
import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.component.Component;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.layout.splitter.event.SplitterResizeEvent;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasVisibility;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.UUID;

/**
 * Splitter is a component that encapsulates two resizable components separated by a divider that
 * enables the user to dynamically resize them.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@NodeName("dwc-splitter")
public class Splitter extends ElementCompositeContainer
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
   * Creates a new Splitter with the given id.
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
      addToMaster(master);
    }

    if (detail != null) {
      addToDetail(detail);
    }
  }

  /**
   * Creates a new Splitter.
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
   * Add the given component to the Splitter detail slot.
   *
   * @param component the component to add
   * @return the component itself
   */
  public Splitter addToDetail(Component... component) {
    getElement().add(DETAIL_SLOT, component);
    return this;
  }

  /**
   * Add the given component to the Splitter master slot.
   *
   * @param component the component to add
   * @return the component itself
   */
  public Splitter addToMaster(Component... component) {
    getElement().add(MASTER_SLOT, component);
    return this;
  }

  /**
   * Activate/Deactivate state autosave.
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
   * Check if the drawer is popover.
   *
   * @return true if the drawer is popover, false otherwise
   */
  public boolean isAutosave() {
    return get(autosaveProp);
  }

  /**
   * The maximum width of the master panel when orientation is horizontal and the maximum height
   * when vertical.
   *
   * @param masterMaxSize the maximum size of the master panel
   * @return the component itself
   */
  public Splitter setMasterMaxSize(String masterMaxSize) {
    set(masterMaxSizeProp, masterMaxSize);
    return this;
  }

  /**
   * Get the maximum size of the master panel.
   *
   * @return the maximum size of the master panel
   */
  public String getMasterMaxSize() {
    return get(masterMaxSizeProp);
  }

  /**
   * The minimum width of the master panel when orientation is horizontal and the minimum height
   * when vertical.
   *
   * @param masterMinSize the minimum size of the master panel
   * @return the component itself
   */
  public Splitter setMasterMinSize(String masterMinSize) {
    set(masterMinSizeProp, masterMinSize);
    return this;
  }

  /**
   * Get the minimum size of the master panel.
   *
   * @return the minimum size of the master panel
   */
  public String getMasterMinSize() {
    return get(masterMinSizeProp);
  }

  /**
   * Set the orientation of the splitter.
   *
   * @param orientation the orientation of the splitter
   * @return the component itself
   */
  public Splitter setOrientation(Orientation orientation) {
    set(orientationProp, orientation);
    return this;
  }

  /**
   * Get the orientation of the splitter.
   *
   * @return the orientation of the splitter
   */
  public Orientation getOrientation() {
    return get(orientationProp);
  }

  /**
   * Set the relative position of the splitter.
   *
   * @param positionRelative the relative position of the splitter
   * @return the component itself
   *
   * @throws IllegalArgumentException if the position is not between 0 and 100
   */
  public Splitter setPositionRelative(double positionRelative) {
    if (positionRelative < 0 || positionRelative > 100) {
      throw new IllegalArgumentException("The position must be between 0 and 100");
    }

    set(positionRelativeProp, positionRelative);
    return this;
  }

  /**
   * Get the relative position of the splitter.
   *
   * @return the relative position of the splitter
   */
  public double getPositionRelative() {
    return get(positionRelativeProp, true, Boolean.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Splitter addClassName(String... className) {
    getElement().addClassName(className);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Splitter removeClassName(String... className) {
    getElement().removeClassName(className);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Splitter setStyle(String property, String value) {
    getElement().setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Splitter removeStyle(String property) {
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
  public Splitter setVisible(boolean visible) {
    getElement().setVisible(visible);
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
