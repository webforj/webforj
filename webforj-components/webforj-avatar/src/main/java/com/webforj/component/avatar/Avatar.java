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

  static final String DEFAULT_LABEL = "Avatar";
  private String tooltipText;

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
   * Creates a new avatar with the given initials.
   *
   * @param label the accessible label
   * @param initials the initials
   */
  public Avatar(String label, String initials) {
    super();
    setText(label);
    setInitials(initials);
  }

  /**
   * Creates a new avatar with the given label. The initials are computed from the label.
   *
   * @param label the accessible label
   */
  public Avatar(String label) {
    this(label, getComputedInitials(label));
  }

  /**
   * Creates a new avatar with the given label and child components.
   *
   * @param label the accessible label
   * @param components the child components
   */
  public Avatar(String label, Component... components) {
    super();
    setText(label);
    add(components);
  }

  /**
   * Creates a new avatar with the given child components.
   *
   * @param components the child components
   */
  public Avatar(Component... components) {
    this(DEFAULT_LABEL, components);
  }

  /**
   * Creates a new empty avatar.
   */
  public Avatar() {
    this(DEFAULT_LABEL);
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
   * Sets the accessible label for the avatar.
   *
   * @param label the accessible label
   * @return the component itself
   */
  @Override
  public Avatar setLabel(String label) {
    set(labelProp, label);

    // auto generate the tooltip too, but not for the default label
    if ((this.tooltipText == null || this.tooltipText.isBlank()) && !DEFAULT_LABEL.equals(label)) {
      setTooltipText(label);
    }

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
   * Alias for {@link #setLabel(String)}.
   *
   * {@inheritDoc}
   */
  @Override
  public Avatar setText(String text) {
    setLabel(text);
    return this;
  }

  /**
   * Alias for {@link #getLabel()}.
   *
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    return getLabel();
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

  static String getComputedInitials(String label) {
    if (label == null || label.isBlank()) {
      return "";
    }

    String[] parts = label.trim().split(" ");
    StringBuilder initials = new StringBuilder();
    for (String part : parts) {
      if (!part.isEmpty()) {
        initials.append(part.charAt(0));
      }
    }

    return initials.toString();
  }
}
