package org.dwcj.component.layout.flexlayout;

import java.util.Optional;
import org.dwcj.component.Component;
import org.dwcj.component.window.Panel;
import org.dwcj.concern.HasStyle;
import org.dwcj.concern.legacy.LegacyHasStyle;

/**
 * A flex layout.
 *
 * @see <a href="https://css-tricks.com/snippets/css/a-guide-to-flexbox/">A Complete Guide to
 *      Flexbox</a>
 * @author Hyyan Abo Fakher
 */
public class FlexLayout extends Panel {
  /**
   * Create a new flex layout.
   */
  public FlexLayout() {
    super();
    setInline(false);
    setSpacing("1em");
  }

  /**
   * Creates a new flex layout with the given components.
   *
   * @param component the components to add to the layout
   */
  public FlexLayout(Component... component) {
    this();
    add(component);
  }

  /**
   * Creates a new flex layout with the given components.
   *
   * @param components the components to add to the layout
   * @param direction the layout direction
   */
  public FlexLayout(FlexDirection direction, Component... components) {
    this(components);
    setDirection(direction);
  }

  /**
   * Creates a new flex layout using the given builder.
   *
   * @param builder the builder
   */
  public FlexLayout(FlexLayoutBuilder builder, Component... components) {
    this(components);

    boolean isInline = builder.isInline();
    setInline(isInline);

    if (builder.getDirection() != null) {
      setDirection(builder.getDirection());
    }

    if (builder.getAlignment() != null) {
      setAlignment(builder.getAlignment());
    }

    if (builder.getContentAlignment() != null) {
      setAlignContent(builder.getContentAlignment());
    }

    if (builder.getJustifyContent() != null) {
      setJustifyContent(builder.getJustifyContent());
    }

    if (builder.getWrap() != null) {
      setWrap(builder.getWrap());
    }
  }

  /**
   * Creates a new flex layout builder.
   *
   * @return the builder
   */
  public static FlexLayoutBuilder create() {
    return new FlexLayoutBuilder();
  }

  /**
   * Creates a new flex layout builder.
   *
   * @param components the components to add to the layout
   * @return the builder
   */
  public static FlexLayoutBuilder create(Component... components) {
    return new FlexLayoutBuilder(components);
  }

  /**
   * When true, then the layout will be displayed inline, otherwise it will be display as block. The
   * default value is false (block).
   *
   * @param inline true to display the layout inline, otherwise false
   * @return this layout
   */
  public FlexLayout setInline(boolean inline) {
    setStyle("display", inline ? "inline-flex" : "flex");
    return this;
  }

  /**
   * Checks if the layout is inline or block displayed.
   *
   * @return true if the layout is inline, otherwise false
   */
  public boolean isInline() {
    return Optional.ofNullable(getStyle("display")).map(d -> d.equals("inline-flex")).orElse(false);
  }

  /**
   * Sets the layout direction.
   *
   * <p>
   * This establishes the main-axis, thus defining the direction items are placed in the layout. The
   * default value is {@link FlexDirection#ROW}.
   * </p>
   *
   * @param direction the direction
   * @return this layout
   */
  public FlexLayout setDirection(FlexDirection direction) {
    setStyle(FlexProperties.PROP_DIRECTION, direction.getValue());
    return this;
  }

  /**
   * Gets the layout direction.
   *
   * @return the direction
   */
  public FlexDirection getDirection() {
    String direction = Optional.ofNullable(getStyle(FlexProperties.PROP_DIRECTION))
        .orElse(FlexDirection.getDefault().getValue());

    return FlexDirection.fromValue(direction);
  }

  /**
   * Sets the layout wrap mode.
   *
   * <p>
   * By default, items will all try to fit onto one line. You can change that and allow the items to
   * wrap as needed with this option. The default value is {@link FlexWrap#NOWRAP}.
   * </p>
   *
   * @see <a href="https://css-tricks.com/snippets/css/a-guide-to-flexbox/#article-header-id-6">Flex
   *      Wrap</a>
   *
   * @param wrap the wrap
   * @return this layout
   */
  public FlexLayout setWrap(FlexWrap wrap) {
    setStyle(FlexProperties.PROP_WRAP, wrap.getValue());
    return this;
  }

  /**
   * Gets the layout wrap mode.
   *
   * @return the wrap
   */
  public FlexWrap getWrap() {
    String wrap = Optional.ofNullable(getStyle(FlexProperties.PROP_WRAP))
        .orElse(FlexWrap.getDefault().getValue());

    return FlexWrap.fromValue(wrap);
  }

  /**
   * Sets the layout flow.
   *
   * <p>
   * This is a shorthand for setting both {@link #setDirection(FlexDirection)} and
   * {@link #setWrap(FlexWrap)} at the same time. The default value is {@link FlexFlow#ROW_NOWRAP}.
   * </p>
   *
   * @param flow the flow
   * @return this layout
   *
   * @see #setDirection(FlexDirection)
   * @see #setWrap(FlexWrap)
   */
  public FlexLayout setFlow(FlexFlow flow) {
    setStyle(FlexProperties.PROP_FLOW, flow.getValue());
    return this;
  }

  /**
   * Gets the layout flow.
   *
   * <p>
   * This is a shorthand for getting both {@link #getDirection()} and {@link #getWrap()} at the same
   * time.
   * </p>
   *
   * @return the flow
   *
   * @see #getDirection()
   * @see #getWrap()
   */
  public FlexFlow getFlow() {
    String flow = Optional.ofNullable(getComputedStyle(FlexProperties.PROP_FLOW))
        .orElse(FlexFlow.getDefault().getValue());

    return FlexFlow.fromValue(flow);
  }

  /**
   * Sets the {@link FlexJustifyContent} use by the layout.
   *
   * <p>
   * This defines the alignment along the main axis. it helps distribute extra free space left over
   * when either all the items on a line are inflexible, or are flexible but have reached their
   * maximum size. It also exerts some control over the alignment of items when they overflow the
   * line. The default value is {@link FlexJustifyContent#START}
   * </p>
   *
   * @param justifyContent the justify content
   * @return this layout
   */
  public FlexLayout setJustifyContent(FlexJustifyContent justifyContent) {
    setStyle(FlexProperties.PROP_JUSTIFY_CONTENT, justifyContent.getValue());
    return this;
  }

  /**
   * Gets the {@link FlexJustifyContent} use by the layout.
   *
   * @return the justify content
   */
  public FlexJustifyContent getJustifyContent() {
    String justifyContent = Optional.ofNullable(getStyle(FlexProperties.PROP_JUSTIFY_CONTENT))
        .orElse(FlexJustifyContent.getDefault().getValue());

    return FlexJustifyContent.fromValue(justifyContent);
  }

  /**
   * Sets the {@link FlexAlignment} use by the layout.
   *
   * <p>
   * Defines the default behaviour for how items are laid out along the cross axis on the current
   * line. Think of it as the {@link #setJustifyContent(FlexJustifyContent)} version for the
   * cross-axis (perpendicular to the main-axis). The default value is {@link FlexAlignment#STRETCH}
   * </p>
   *
   * @param alignItems the align items
   * @return this layout
   */
  public FlexLayout setAlignment(FlexAlignment alignItems) {
    setStyle(FlexProperties.PROP_ALIGN_ITEMS, alignItems.getValue());
    return this;
  }

  /**
   * Gets the {@link FlexAlignment} use by the layout.
   *
   * @return the align items
   */
  public FlexAlignment getAlignment() {
    String alignItems = Optional.ofNullable(getStyle(FlexProperties.PROP_ALIGN_ITEMS))
        .orElse(FlexAlignment.getDefault().getValue());

    return FlexAlignment.fromValue(alignItems);
  }

  /**
   * Sets the {@link FlexContentAlignment} use by the layout.
   *
   * <p>
   * The aligns the layout's lines within when there is extra space in the cross-axis, similar to
   * how {@link #setJustifyContent(FlexJustifyContent)} aligns individual items within the
   * main-axis. The default value is {@link FlexContentAlignment#NORMAL}
   * </p>
   *
   * <p>
   * Note: This property has no effect when the layout has only one line of items. In this case, the
   * {@link #setAlignment(FlexAlignment)} property is used instead. This property also has no effect
   * when the {@link #setWrap(FlexWrap)} property is set to {@link FlexWrap#NOWRAP}.
   * </p>
   *
   * @param alignContent the align content
   * @return this layout
   */
  public FlexLayout setAlignContent(FlexContentAlignment alignContent) {
    setStyle(FlexProperties.PROP_ALIGN_CONTENT, alignContent.getValue());
    return this;
  }

  /**
   * Gets the {@link FlexContentAlignment} use by the layout.
   *
   * @return the align content
   */
  public FlexContentAlignment getAlignContent() {
    String alignContent = Optional.ofNullable(getStyle(FlexProperties.PROP_ALIGN_CONTENT))
        .orElse(FlexContentAlignment.getDefault().getValue());

    return FlexContentAlignment.fromValue(alignContent);
  }

  /**
   * Sets the gap between items.
   *
   * <p>
   * The gap property explicitly controls the space between items. It applies that spacing only
   * between items not on the outer edges.
   * </p>
   *
   * <p>
   * The behavior could be thought of as a minimum gutter, as if the gutter is bigger somehow
   * (because of something like <code>setAlignContent(FlexContentAlignment.SPACE_BETWEEN)</code>
   * then the gap will only take effect if that space would end up smaller.
   * </p>
   *
   * @param spacing the gap
   * @return this layout
   */
  public FlexLayout setSpacing(String spacing) {
    setStyle(FlexProperties.PROP_GAP, spacing);
    return this;
  }

  /**
   * Gets the gap between items.
   *
   * @return the gap
   */
  public String getSpacing() {
    return Optional.ofNullable(getStyle(FlexProperties.PROP_GAP)).orElse("");
  }

  /**
   * Sets the layout margin.
   *
   * @param margin the margin
   * @return this layout
   */
  public FlexLayout setMargin(String margin) {
    setStyle("margin", margin);
    return this;
  }

  /**
   * Sets the layout margin.
   *
   * @return this layout
   */
  public String getMargin() {
    return Optional.ofNullable(getStyle("margin")).orElse("");
  }

  /**
   * Sets the layout padding.
   *
   * @param padding the padding
   * @return this layout
   */
  public FlexLayout setPadding(String padding) {
    setStyle("padding", padding);
    return this;
  }

  /**
   * Sets the layout padding.
   *
   * @return the padding
   */
  public String getPadding() {
    return Optional.ofNullable(getStyle("gap")).orElse("");
  }

  /**
   * Sets the order of given control.
   *
   * <p>
   * By default, items will be laid out in the source order. However, the order property controls
   * the order in which they appear in the layout.
   * </p>
   *
   * @param order the order
   * @param component the component
   *
   * @return this layout
   *
   * @deprecated Use {@link #setItemOrder(int, HasStyle)} instead.
   */
  @Deprecated(since = "23.05", forRemoval = true)
  public FlexLayout setItemOrder(int order, LegacyHasStyle component) {
    if (order == 0) {
      component.setStyle(FlexProperties.PROP_ORDER, "");
    } else {
      component.setStyle(FlexProperties.PROP_ORDER, String.valueOf(order));
    }

    return this;
  }

  /**
   * Sets the order of given control.
   *
   * <p>
   * By default, items will be laid out in the source order. However, the order property controls
   * the order in which they appear in the layout.
   * </p>
   *
   * @param order the order
   * @param component the component
   *
   * @return this layout
   */
  public FlexLayout setItemOrder(int order, HasStyle<?> component) {
    if (order == 0) {
      component.setStyle(FlexProperties.PROP_ORDER, "");
    } else {
      component.setStyle(FlexProperties.PROP_ORDER, String.valueOf(order));
    }

    return this;
  }

  /**
   * Gets the order of given control.
   *
   * @param control the control
   * @return the order
   *
   * @deprecated Use {@link #getItemOrder(HasStyle)} instead.
   */
  @Deprecated(since = "23.05", forRemoval = true)
  public int getItemOrder(LegacyHasStyle control) {
    String order = Optional.ofNullable(control.getStyle(FlexProperties.PROP_ORDER)).orElse("0");

    return Integer.parseInt(order);
  }

  /**
   * Gets the order of given control.
   *
   * @param control the control
   * @return the order
   */
  public int getItemOrder(HasStyle<?> control) {
    String order = Optional.ofNullable(control.getStyle(FlexProperties.PROP_ORDER)).orElse("0");

    return Integer.parseInt(order);
  }

  /**
   * Sets the flex grow for the given items.
   *
   * <p>
   * The defines the ability for a control to grow if necessary. It accepts a numeric value that
   * serves as a proportion. It dictates what amount of the available space inside the layout the
   * control should take up.
   * </p>
   *
   * <p>
   * If all controls have flex-grow set to 1, the remaining space in the layout will be distributed
   * equally to all controls. If one of the items has a value of 2, the remaining space would take
   * up twice as much space as the others (or it will try to, at least).
   * </p>
   *
   * @param grow the grow to set. If {@code 0} the flex grow is removed from the control
   * @param items the items
   *
   * @return this layout
   * @throws IllegalArgumentException if the grow is negative
   *
   * @deprecated Use {@link #setItemGrow(double, HasStyle)} instead.
   */
  @Deprecated(since = "23.05", forRemoval = true)
  public FlexLayout setItemGrow(double grow, LegacyHasStyle... items) {
    if (grow < 0) {
      throw new IllegalArgumentException("Flex grow cannot be negative");
    }

    if (grow == 0) {
      for (LegacyHasStyle control : items) {
        control.setStyle(FlexProperties.PROP_GROW, "");
      }
    } else {
      for (LegacyHasStyle control : items) {
        control.setStyle(FlexProperties.PROP_GROW, String.valueOf(grow));
      }
    }

    return this;
  }

  /**
   * Sets the flex grow for the given items.
   *
   * <p>
   * The defines the ability for a control to grow if necessary. It accepts a numeric value that
   * serves as a proportion. It dictates what amount of the available space inside the layout the
   * control should take up.
   * </p>
   *
   * <p>
   * If all controls have flex-grow set to 1, the remaining space in the layout will be distributed
   * equally to all controls. If one of the items has a value of 2, the remaining space would take
   * up twice as much space as the others (or it will try to, at least).
   * </p>
   *
   * @param grow the grow to set. If {@code 0} the flex grow is removed from the control
   * @param items the items
   *
   * @return this layout
   * @throws IllegalArgumentException if the grow is negative
   */
  public FlexLayout setItemGrow(double grow, HasStyle<?>... items) {
    if (grow < 0) {
      throw new IllegalArgumentException("Flex grow cannot be negative");
    }

    if (grow == 0) {
      for (HasStyle<?> control : items) {
        control.setStyle(FlexProperties.PROP_GROW, "");
      }
    } else {
      for (HasStyle<?> control : items) {
        control.setStyle(FlexProperties.PROP_GROW, String.valueOf(grow));
      }
    }

    return this;
  }

  /**
   * Gets the flex grow for the given control.
   *
   * @param control the control
   * @return the flex grow
   *
   * @deprecated Use {@link #getItemGrow(HasStyle)} instead.
   */
  @Deprecated(since = "23.05", forRemoval = true)
  public double getItemGrow(LegacyHasStyle control) {
    String grow = Optional.ofNullable(control.getStyle(FlexProperties.PROP_GROW)).orElse("0");

    return Double.parseDouble(grow);
  }

  /**
   * Gets the flex grow for the given control.
   *
   * @param control the control
   * @return the flex grow
   */
  public double getItemGrow(HasStyle<?> control) {
    String grow = Optional.ofNullable(control.getStyle(FlexProperties.PROP_GROW)).orElse("0");

    return Double.parseDouble(grow);
  }

  /**
   * Sets the flex shrink for the given items.
   *
   * <p>
   * The defines the ability for a control to shrink if necessary.
   * </p>
   *
   * @param shrink the shrink to set.
   * @param items the items
   *
   * @return this layout
   *
   * @deprecated Use {@link #setItemShrink(double, HasStyle)} instead.
   */
  @Deprecated(since = "23.05", forRemoval = true)
  public FlexLayout setItemShrink(double shrink, LegacyHasStyle... items) {
    if (shrink < 0) {
      throw new IllegalArgumentException("Flex shrink cannot be negative");
    }

    for (LegacyHasStyle control : items) {
      control.setStyle(FlexProperties.PROP_SHRINK, String.valueOf(shrink));
    }

    return this;
  }

  /**
   * Sets the flex shrink for the given items.
   *
   * <p>
   * The defines the ability for a control to shrink if necessary.
   * </p>
   *
   * @param shrink the shrink to set.
   * @param items the items
   *
   * @return this layout
   */
  public FlexLayout setItemShrink(double shrink, HasStyle<?>... items) {
    if (shrink < 0) {
      throw new IllegalArgumentException("Flex shrink cannot be negative");
    }

    for (HasStyle<?> control : items) {
      control.setStyle(FlexProperties.PROP_SHRINK, String.valueOf(shrink));
    }

    return this;
  }

  /**
   * Gets the flex shrink for the given component.
   *
   * @param component the component
   * @return the flex shrink
   *
   * @deprecated Use {@link #getItemShrink(HasStyle)} instead.
   */
  @Deprecated(since = "23.05", forRemoval = true)
  public double getItemShrink(LegacyHasStyle component) {
    String shrink = Optional.ofNullable(component.getStyle(FlexProperties.PROP_SHRINK)).orElse("1");

    return Double.parseDouble(shrink);
  }

  /**
   * Gets the flex shrink for the given component.
   *
   * @param component the component
   * @return the flex shrink
   */
  public double getItemShrink(HasStyle<?> component) {
    String shrink = Optional.ofNullable(component.getStyle(FlexProperties.PROP_SHRINK)).orElse("1");

    return Double.parseDouble(shrink);
  }

  /**
   * Sets the flex basis for the given items.
   *
   * <p>
   * The defines the default size of an item before the remaining space is distributed. It can be a
   * length (e.g. 20%, 5rem, etc.) or a keyword. for instance, the "auto" keyword means "look at my
   * width or height property".
   * </p>
   *
   * @param width the width to set. If {@code null} is passed then the flex-basis will be removed.
   * @param items the items
   *
   * @return this layout
   *
   * @deprecated Use {@link #setItemBasis(String, HasStyle)} instead.
   */
  @Deprecated(since = "23.05", forRemoval = true)
  public FlexLayout setItemBasis(String width, LegacyHasStyle... items) {
    if (width == null) {
      for (LegacyHasStyle control : items) {
        control.setStyle(FlexProperties.PROP_BASIS, "");
      }
    } else {
      for (LegacyHasStyle control : items) {
        control.setStyle(FlexProperties.PROP_BASIS, width);
      }
    }
    return this;
  }

  /**
   * Sets the flex basis for the given items.
   *
   * <p>
   * The defines the default size of an item before the remaining space is distributed. It can be a
   * length (e.g. 20%, 5rem, etc.) or a keyword. for instance, the "auto" keyword means "look at my
   * width or height property".
   * </p>
   *
   * @param width the width to set. If {@code null} is passed then the flex-basis will be removed.
   * @param items the items
   *
   * @return this layout
   */
  public FlexLayout setItemBasis(String width, HasStyle<?>... items) {
    if (width == null) {
      for (HasStyle<?> control : items) {
        control.setStyle(FlexProperties.PROP_BASIS, "");
      }
    } else {
      for (HasStyle<?> control : items) {
        control.setStyle(FlexProperties.PROP_BASIS, width);
      }
    }
    return this;
  }

  /**
   * Gets the flex basis for the given component.
   *
   * @param component the component
   * @return the flex basis
   *
   * @deprecated Use {@link #getItemBasis(HasStyle)} instead.
   */
  @Deprecated(since = "23.05", forRemoval = true)
  public String getItemBasis(LegacyHasStyle component) {
    return Optional.ofNullable(component.getStyle(FlexProperties.PROP_BASIS)).orElse("auto");
  }

  /**
   * Gets the flex basis for the given component.
   *
   * @param component the component
   * @return the flex basis
   */
  public String getItemBasis(HasStyle<?> component) {
    return Optional.ofNullable(component.getStyle(FlexProperties.PROP_BASIS)).orElse("auto");
  }

  /**
   * Sets the alignment of given items.
   *
   * <p>
   * This allows the default alignment (or the one specified by
   * {@link #setAlignment(FlexAlignment)}) to be overridden for individual items.
   * </p>
   *
   * @param alignSelf the alignment
   * @param items the items
   *
   * @return this layout
   *
   * @see #setAlignment(FlexAlignment)
   *
   * @deprecated Use {@link #setItemAlignment(FlexAlignment, HasStyle)} instead.
   */
  @Deprecated(since = "23.05", forRemoval = true)
  public FlexLayout setItemAlignment(FlexAlignment alignSelf, LegacyHasStyle... items) {
    if (alignSelf == null) {
      for (LegacyHasStyle component : items) {
        component.setStyle(FlexProperties.PROP_ALIGN_SELF, "");
      }
    } else {
      for (LegacyHasStyle component : items) {
        component.setStyle(FlexProperties.PROP_ALIGN_SELF, alignSelf.getValue());
      }
    }

    return this;
  }

  /**
   * Sets the alignment of given items.
   *
   * <p>
   * This allows the default alignment (or the one specified by
   * {@link #setAlignment(FlexAlignment)}) to be overridden for individual items.
   * </p>
   *
   * @param alignSelf the alignment
   * @param items the items
   *
   * @return this layout
   *
   * @see #setAlignment(FlexAlignment)
   */
  public FlexLayout setItemAlignment(FlexAlignment alignSelf, HasStyle<?>... items) {
    if (alignSelf == null) {
      for (HasStyle<?> component : items) {
        component.setStyle(FlexProperties.PROP_ALIGN_SELF, "");
      }
    } else {
      for (HasStyle<?> component : items) {
        component.setStyle(FlexProperties.PROP_ALIGN_SELF, alignSelf.getValue());
      }
    }

    return this;
  }

  /**
   * Gets the alignment of given component.
   *
   * @param component the component
   * @return the alignment
   *
   * @deprecated Use {@link #getItemAlignment(HasStyle)} instead.
   */
  @Deprecated(since = "23.05", forRemoval = true)
  public FlexAlignment getItemAlignment(LegacyHasStyle component) {
    String alignSelf = Optional.ofNullable(component.getStyle(FlexProperties.PROP_ALIGN_SELF))
        .orElse(FlexAlignment.getDefault().getValue());

    return FlexAlignment.fromValue(alignSelf);
  }

  /**
   * Gets the alignment of given component.
   *
   * @param component the component
   * @return the alignment
   */
  public FlexAlignment getItemAlignment(HasStyle<?> component) {
    String alignSelf = Optional.ofNullable(component.getStyle(FlexProperties.PROP_ALIGN_SELF))
        .orElse(FlexAlignment.getDefault().getValue());

    return FlexAlignment.fromValue(alignSelf);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FlexLayout setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FlexLayout addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public FlexLayout removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }
}
