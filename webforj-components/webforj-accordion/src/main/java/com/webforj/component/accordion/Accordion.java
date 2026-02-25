package com.webforj.component.accordion;

import com.webforj.component.Component;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasEnablement;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasVisibility;

/**
 * The accordion groups {@link AccordionPanel} elements and coordinates their expand/collapse
 * behavior. When {@code multiple} is false (default), opening one panel automatically closes the
 * others.
 *
 * <p>
 * Panels can also be used standalone without this wrapper.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("dwc-accordion")
public class Accordion extends ElementCompositeContainer implements HasClassName<Accordion>,
    HasStyle<Accordion>, HasVisibility<Accordion>, HasEnablement<Accordion> {

  // Property descriptors
  private final PropertyDescriptor<Boolean> multipleProp =
      PropertyDescriptor.property("multiple", false);

  /**
   * Instantiates a new accordion.
   */
  public Accordion() {
    super();
  }

  /**
   * Instantiates a new accordion with panels.
   *
   * @param panels the panels to add
   */
  public Accordion(AccordionPanel... panels) {
    this();
    add(panels);
  }

  /**
   * Sets whether multiple panels can be open at the same time.
   *
   * @param multiple when true, multiple panels can be open simultaneously
   * @return the component itself
   */
  public Accordion setMultiple(boolean multiple) {
    set(multipleProp, multiple);
    return this;
  }

  /**
   * Returns whether multiple panels can be open at the same time.
   *
   * @return true if multiple panels can be open simultaneously
   */
  public boolean isMultiple() {
    return get(multipleProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled() {
    for (Component child : getComponents()) {
      if (child instanceof AccordionPanel panel && panel.isEnabled()) {
        return true;
      }
    }

    return getComponents().isEmpty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Accordion setEnabled(boolean enabled) {
    for (Component child : getComponents()) {
      if (child instanceof AccordionPanel panel) {
        panel.setEnabled(enabled);
      }
    }
    return this;
  }

  /**
   * Closes all panels.
   *
   * @return the component itself
   */
  public Accordion closeAll() {
    // A loop is better than calling the client API
    // this will keep the panel states in sync
    for (Component child : getComponents()) {
      if (child instanceof AccordionPanel panel) {
        panel.close();
      }
    }

    return this;
  }

  /**
   * Opens all panels. Only effective when {@code multiple} is true.
   *
   * @return the component itself
   */
  public Accordion openAll() {
    if (!isMultiple()) {
      return this;
    }

    // A loop is better than calling the client API
    // this will keep the panel states in sync
    for (Component child : getComponents()) {
      if (child instanceof AccordionPanel panel) {
        panel.open();
      }
    }

    return this;
  }

  Element getOriginalElement() {
    return getElement();
  }
}
