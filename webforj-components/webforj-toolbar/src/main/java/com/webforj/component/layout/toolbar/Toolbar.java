package com.webforj.component.layout.toolbar;

import com.webforj.component.Component;
import com.webforj.component.Theme;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasSize;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasVisibility;

/**
 * The Toolbar component provides a flexible container for grouping and aligning components in a app
 * layout.
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
@NodeName("dwc-toolbar")
public class Toolbar extends ElementCompositeContainer implements HasClassName<Toolbar>,
    HasStyle<Toolbar>, HasVisibility<Toolbar>, HasSize<Toolbar>, HasAttribute<Toolbar> {

  // Slots
  static final String START_SLOT = "start";
  static final String TITLE_SLOT = "title";
  static final String END_SLOT = "end";

  // Property descriptors
  private final PropertyDescriptor<Theme> themeProp =
      PropertyDescriptor.property("theme", Theme.DEFAULT);
  private final PropertyDescriptor<Boolean> compactProp =
      PropertyDescriptor.property("compact", false);

  /**
   * Instantiates a new app layout.
   */
  public Toolbar() {
    super();
  }

  /**
   * Adds the component to the start slot.
   *
   * @param components the component to be added
   * @return the component itself
   */
  public Toolbar addToStart(Component... components) {
    getElement().add(START_SLOT, components);
    return this;
  }

  /**
   * Adds the component to the title slot.
   *
   * @param components the component to be added
   * @return the component itself
   */
  public Toolbar addToTitle(Component... components) {
    getElement().add(TITLE_SLOT, components);
    return this;
  }

  /**
   * Alias for {@link #add(Component...)}.
   *
   * @param components the component to be added
   * @return the component itself
   */
  public Toolbar addToContent(Component... components) {
    getElement().add(components);
    return this;
  }

  /**
   * Adds the component to the end slot.
   *
   * @param components the component to be added
   * @return the component itself
   */
  public Toolbar addToEnd(Component... components) {
    getElement().add(END_SLOT, components);
    return this;
  }

  /**
   * Sets the toolbar theme.
   *
   * @param theme the theme
   * @return the component itself
   */
  public Toolbar setTheme(Theme theme) {
    set(themeProp, theme);
    return this;
  }

  /**
   * Gets the toolbar theme.
   *
   * @return the theme
   */
  public Theme getTheme() {
    return get(themeProp);
  }

  /**
   * Sets the toolbar compact mode.
   *
   * <p>
   * The compact mode is used to reduce the toolbar height by not using a padding.
   * </p>
   *
   * @param compact the compact mode
   * @return the component itself
   */
  public Toolbar setCompact(boolean compact) {
    set(compactProp, compact);
    return this;
  }

  /**
   * Checks if the toolbar is in compact mode.
   *
   * @return the compact mode
   */
  public boolean isCompact() {
    return get(compactProp);
  }

  Element getOriginalElement() {
    return getElement();
  }
}
