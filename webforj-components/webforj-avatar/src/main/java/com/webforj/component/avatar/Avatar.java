package com.webforj.component.avatar;

import com.webforj.component.Component;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.concern.HasElementClickListener;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasExpanse;
import com.webforj.concern.HasLabel;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.concern.HasTheme;
import com.webforj.concern.HasTooltip;
import com.webforj.concern.HasVisibility;

/**
 * A customizable avatar component that displays an image, initials, or an icon.
 *
 * @author Hyyan Abo Fakher
 * @since 25.11
 */
@NodeName("dwc-avatar")
public class Avatar extends ElementCompositeContainer
    implements HasText<Avatar>, HasStyle<Avatar>, HasClassName<Avatar>, HasVisibility<Avatar>,
    HasLabel<Avatar>, HasAttribute<Avatar>, HasTooltip<Avatar>, HasTheme<Avatar, AvatarTheme>,
    HasExpanse<Avatar, AvatarExpanse>, HasElementClickListener<Avatar> {

  // Properties
  private final PropertyDescriptor<String> initialsProp =
      PropertyDescriptor.property("initials", "");
  private final PropertyDescriptor<String> labelProp =
      PropertyDescriptor.property("label", "avatar");
  private final PropertyDescriptor<AvatarExpanse> expanseProp =
      PropertyDescriptor.property("expanse", AvatarExpanse.MEDIUM);
  private final PropertyDescriptor<AvatarTheme> themeProp =
      PropertyDescriptor.property("theme", AvatarTheme.DEFAULT);
  private final PropertyDescriptor<AvatarShape> shapeProp =
      PropertyDescriptor.property("shape", AvatarShape.CIRCLE);

  /**
   * Creates a new heading with the given initials.
   *
   * @param initials the initials
   */

  public Avatar(String initials) {
    super();
    setInitials(initials);
  }

  /**
   * Creates a new avatar with the given child components.
   *
   * @param components the child components
   */
  public Avatar(Component... components) {
    super();
    add(components);
  }

  /**
   * Creates a new empty avatar.
   */
  public Avatar() {
    super();
  }

  /**
   * Sets the initials to display when no image is available.
   *
   * @param initials the initials to display
   * @return the component itself
   */
  public Avatar setInitials(String initials) {
    set(initialsProp, initials);
    return this;
  }

  /**
   * Gets the initials.
   *
   * @return the initials
   */
  public String getInitials() {
    return get(initialsProp);
  }

  /**
   * Alias for {@link #setInitials(String)}.
   *
   * {@inheritDoc}
   */
  @Override
  public Avatar setText(String text) {
    setInitials(text);
    return this;
  }

  /**
   * Alias for {@link #getInitials()}.
   *
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    return getInitials();
  }

  /**
   * Sets the accessible label for the avatar.
   *
   * @param label the accessible label
   * @return the component itself
   */
  @Override
  public Avatar setLabel(String label) {
    set(labelProp, label);
    return this;
  }

  /**
   * Gets the accessible label.
   *
   * @return the accessible label
   */
  @Override
  public String getLabel() {
    return get(labelProp);
  }

  /**
   * Sets the shape of the avatar.
   *
   * @param shape the shape
   * @return the component itself
   */
  public Avatar setShape(AvatarShape shape) {
    set(shapeProp, shape);
    return this;
  }

  /**
   * Gets the shape of the avatar.
   *
   * @return the shape
   */
  public AvatarShape getShape() {
    return get(shapeProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Avatar setExpanse(AvatarExpanse expanse) {
    set(expanseProp, expanse);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AvatarExpanse getExpanse() {
    return get(expanseProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Avatar setTheme(AvatarTheme theme) {
    set(themeProp, theme);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AvatarTheme getTheme() {
    return get(themeProp);
  }
}
