package com.webforj.component.accordion;

import com.webforj.component.Component;
import com.webforj.component.accordion.event.AccordionPanelCloseEvent;
import com.webforj.component.accordion.event.AccordionPanelOpenEvent;
import com.webforj.component.accordion.event.AccordionPanelToggleEvent;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.annotation.PropertyMethods;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasEnablement;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.concern.HasVisibility;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.List;

/**
 * The accordion panel provides a collapsible/expandable content panel with a clickable header. It
 * can be used standalone or inside an {@link Accordion} group for coordinated behavior.
 *
 * <p>
 * When used inside an {@code Accordion}, panels are coordinated so that opening one panel
 * automatically closes the others unless the accordion allows multiple open panels.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("dwc-accordion-panel")
public class AccordionPanel extends ElementCompositeContainer
    implements HasClassName<AccordionPanel>, HasStyle<AccordionPanel>,
    HasVisibility<AccordionPanel>, HasEnablement<AccordionPanel>, HasText<AccordionPanel> {

  // Slots
  private static final String HEADER_SLOT = "header";
  private static final String ICON_SLOT = "icon";

  // Property descriptors
  private final PropertyDescriptor<Boolean> openedProp =
      PropertyDescriptor.property("opened", false);
  private final PropertyDescriptor<Boolean> disabledProp =
      PropertyDescriptor.property("disabled", false);
  @PropertyMethods(setter = "setText", getter = "getText")
  private final PropertyDescriptor<String> labelProp = PropertyDescriptor.property("label", "");

  /**
   * Instantiates a new accordion panel.
   */
  public AccordionPanel() {
    super();
  }

  /**
   * Instantiates a new accordion panel with a label.
   *
   * @param label the header label text
   */
  public AccordionPanel(String label) {
    this();
    setLabel(label);
  }

  /**
   * Instantiates a new accordion panel with a label and content.
   *
   * @param label the header label text
   * @param content the body content components
   */
  public AccordionPanel(String label, Component... content) {
    this(label);
    add(content);
  }

  /**
   * Adds the given components to the header slot, replacing the label text.
   *
   * @param component the components to add
   * @return the component itself
   */
  public AccordionPanel addToHeader(Component... component) {
    getElement().add(HEADER_SLOT, component);
    return this;
  }

  /**
   * Sets the icon component in the icon slot, replacing the default chevron.
   *
   * <p>
   * Any previously set icon will be removed. Pass {@code null} to restore the default chevron.
   * </p>
   *
   * @param icon the icon component to set
   * @return the component itself
   */
  public AccordionPanel setIcon(Component icon) {
    List<Component> existing = getElement().getComponentsInSlot(ICON_SLOT);
    if (!existing.isEmpty()) {
      getElement().remove(existing.toArray(new Component[0]));
    }

    if (icon != null) {
      getElement().add(ICON_SLOT, icon);
    }

    return this;
  }

  /**
   * Gets the icon component from the icon slot.
   *
   * @return the icon component, or {@code null} if using the default chevron
   */
  public Component getIcon() {
    return getElement().getFirstComponentInSlot(ICON_SLOT);
  }

  /**
   * Opens the panel.
   *
   * @return the component itself
   */
  public AccordionPanel open() {
    set(openedProp, true);
    return this;
  }

  /**
   * Closes the panel.
   *
   * @return the component itself
   */
  public AccordionPanel close() {
    set(openedProp, false);
    return this;
  }

  /**
   * Returns whether the panel is currently opened.
   *
   * @return true if the panel body is visible
   */
  public boolean isOpened() {
    return get(openedProp, true, Boolean.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled() {
    return !get(disabledProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AccordionPanel setEnabled(boolean enabled) {
    set(disabledProp, !enabled);
    return this;
  }

  /**
   * Sets the label text rendered in the header.
   *
   *
   * @param label the header label text
   * @return the component itself
   */
  public AccordionPanel setLabel(String label) {
    set(labelProp, label);
    return this;
  }

  /**
   * Gets the label text rendered in the header.
   *
   * @return the header label text
   */
  public String getLabel() {
    return get(labelProp);
  }

  /**
   * Sets the label text rendered in the header.
   *
   * <p>
   * Alias for {@link #setLabel(String)}.
   * </p>
   *
   * @param text the header label text
   * @return the component itself
   */
  @Override
  public AccordionPanel setText(String text) {
    return setLabel(text);
  }

  /**
   * Gets the label text rendered in the header.
   *
   * <p>
   * Alias for {@link #getLabel()}.
   * </p>
   *
   * @return the header label text
   */
  @Override
  public String getText() {
    return getLabel();
  }

  /**
   * Adds a listener for the toggle event, fired before the panel state changes.
   *
   * @param listener the listener
   * @return a registration object for removing the event listener
   */
  public ListenerRegistration<AccordionPanelToggleEvent> addToggleListener(
      EventListener<AccordionPanelToggleEvent> listener) {
    return addEventListener(AccordionPanelToggleEvent.class, listener);
  }

  /**
   * Alias for {@link #addToggleListener(EventListener)}.
   *
   * @param listener the listener
   * @return a registration object for removing the event listener
   */
  public ListenerRegistration<AccordionPanelToggleEvent> onToggle(
      EventListener<AccordionPanelToggleEvent> listener) {
    return addToggleListener(listener);
  }

  /**
   * Adds a listener for the open event, fired after the panel has fully opened.
   *
   * @param listener the listener
   * @return a registration object for removing the event listener
   */
  public ListenerRegistration<AccordionPanelOpenEvent> addOpenListener(
      EventListener<AccordionPanelOpenEvent> listener) {
    return addEventListener(AccordionPanelOpenEvent.class, listener);
  }

  /**
   * Alias for {@link #addOpenListener(EventListener)}.
   *
   * @param listener the listener
   * @return a registration object for removing the event listener
   */
  public ListenerRegistration<AccordionPanelOpenEvent> onOpen(
      EventListener<AccordionPanelOpenEvent> listener) {
    return addOpenListener(listener);
  }

  /**
   * Adds a listener for the close event, fired after the panel has fully closed.
   *
   * @param listener the listener
   * @return a registration object for removing the event listener
   */
  public ListenerRegistration<AccordionPanelCloseEvent> addCloseListener(
      EventListener<AccordionPanelCloseEvent> listener) {
    return addEventListener(AccordionPanelCloseEvent.class, listener);
  }

  /**
   * Alias for {@link #addCloseListener(EventListener)}.
   *
   * @param listener the listener
   * @return a registration object for removing the event listener
   */
  public ListenerRegistration<AccordionPanelCloseEvent> onClose(
      EventListener<AccordionPanelCloseEvent> listener) {
    return addCloseListener(listener);
  }

  Element getOriginalElement() {
    return getElement();
  }
}
