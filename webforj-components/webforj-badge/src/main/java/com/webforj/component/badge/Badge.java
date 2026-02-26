package com.webforj.component.badge;

import com.webforj.component.Component;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.concern.HasElementClickListener;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasExpanse;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.concern.HasTheme;
import com.webforj.concern.HasVisibility;

/**
 * A badge component that displays a small label or indicator.
 *
 * <p>
 * Badges are used for labeling content, displaying metadata, and highlighting information. They
 * support two visual styles through the theme property: filled (default) and outlined.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("dwc-badge")
public class Badge extends ElementCompositeContainer
    implements HasClassName<Badge>, HasStyle<Badge>, HasVisibility<Badge>, HasText<Badge>,
    HasTheme<Badge, BadgeTheme>, HasExpanse<Badge, BadgeExpanse>, HasElementClickListener<Badge> {

  // Properties
  private final PropertyDescriptor<BadgeTheme> themeProp =
      PropertyDescriptor.property("theme", BadgeTheme.DEFAULT);
  private final PropertyDescriptor<BadgeExpanse> expanseProp =
      PropertyDescriptor.property("expanse", BadgeExpanse.SMALL);
  private final PropertyDescriptor<String> labelProp = PropertyDescriptor.property("label", "");

  /**
   * Creates a new badge with the given text and theme.
   *
   * @param text the badge text
   * @param theme the badge theme
   */
  public Badge(String text, BadgeTheme theme) {
    super();
    setLabel(text);
    setTheme(theme);
  }

  /**
   * Creates a new badge with the given text.
   *
   * @param text the badge text
   */
  public Badge(String text) {
    this(text, BadgeTheme.DEFAULT);
  }

  /**
   * Creates a new badge with text and child components.
   *
   * <p>
   * Both the label text and the slotted components are rendered together. This is useful for
   * creating badges with an icon and text, e.g. {@code new Badge("5", TablerIcon.create("edit"))}.
   * </p>
   *
   * @param text the badge text
   * @param components the child components to add (e.g., icons)
   */
  public Badge(String text, Component... components) {
    super();
    setLabel(text);
    add(components);
  }

  /**
   * Creates a new badge with child components.
   *
   * @param components the child components to add (e.g., icons)
   */
  public Badge(Component... components) {
    super();
    add(components);
  }

  /**
   * Creates a new empty badge.
   */
  public Badge() {
    super();
  }

  /**
   * Sets the badge's label text.
   *
   * @param label the label text
   * @return the component itself
   */
  public Badge setLabel(String label) {
    set(labelProp, label);
    return this;
  }

  /**
   * Gets the badge's label text.
   *
   * @return the label text
   */
  public String getLabel() {
    return get(labelProp);
  }

  /**
   * Sets the badge's label text.
   *
   * <p>
   * Alias for {@link #setLabel(String)}.
   * </p>
   *
   * @param text the label text
   * @return the component itself
   */
  @Override
  public Badge setText(String text) {
    return setLabel(text);
  }

  /**
   * Gets the badge's label text.
   *
   * <p>
   * Alias for {@link #getLabel()}.
   * </p>
   *
   * @return the label text
   */
  @Override
  public String getText() {
    return getLabel();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Badge setTheme(BadgeTheme theme) {
    set(themeProp, theme);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BadgeTheme getTheme() {
    return get(themeProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Badge setExpanse(BadgeExpanse expanse) {
    set(expanseProp, expanse);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BadgeExpanse getExpanse() {
    return get(expanseProp);
  }
}
