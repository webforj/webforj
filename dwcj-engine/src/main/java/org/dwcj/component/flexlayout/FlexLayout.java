package org.dwcj.component.flexlayout;

import java.util.Optional;

import org.dwcj.component.AbstractComponent;
import org.dwcj.component.HasStyle;
import org.dwcj.component.window.Div;

/**
 * A flex layout.
 * 
 * @see <a href="https://css-tricks.com/snippets/css/a-guide-to-flexbox/">A
 *      Complete Guide to Flexbox</a>
 * @author Hyyan Abo Fakher
 */
public class FlexLayout extends Div {
  /**
   * Create a new flex layout.
   */
  public FlexLayout() {
    super();
    setInline(false);
    setSpacing("1em");
  }

  /**
   * Create a new flex layout with the given controls.
   * 
   * @param control the controls to add to the layout
   */
  public FlexLayout(AbstractComponent... control) {
    this();
    add(control);
  }

  /**
   * Create a new flex layout with the given controls.
   * 
   * @param controls  the controls to add to the layout
   * @param direction the layout direction
   */
  public FlexLayout(FlexDirection direction, AbstractComponent... controls) {
    this(controls);
    setDirection(direction);
  }

  /**
   * Create a new flex layout using the given builder.
   * 
   * @param builder the builder
   */
  public FlexLayout(FlexLayoutBuilder builder, AbstractComponent... controls) {
    this(controls);

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
   * Create a new flex layout builder.
   * 
   * @return the builder
   */
  public static FlexLayoutBuilder create() {
    return new FlexLayoutBuilder();
  }

  /**
   * Create a new flex layout builder.
   * 
   * @param controls the controls to add to the layout
   * @return the builder
   */
  public static FlexLayoutBuilder create(AbstractComponent... controls) {
    return new FlexLayoutBuilder(controls);
  }

  /**
   * When true, then the layout will be displayed inline , otherwise it will be
   * display as block. The default value is false (block).
   * 
   * @param inline true to display the layout inline, otherwise false
   * @return this layout
   */
  public FlexLayout setInline(boolean inline) {
    setStyle("display", inline ? "inline-flex" : "flex");
    return this;
  }

  /**
   * Check if the layout is inline or block displayed.
   * 
   * @return true if the layout is inline, otherwise false
   */
  public boolean isInline() {
    Boolean display = Optional.ofNullable(getStyle("display"))
        .map(d -> d.equals("inline-flex"))
        .orElse(false);

    return display;
  }

  /**
   * Set the layout direction.
   * 
   * This establishes the main-axis, thus defining the direction items are
   * placed in the layout. The default value is {@link FlexDirection#ROW}.
   * 
   * @param direction the direction
   * @return this layout
   */
  public FlexLayout setDirection(FlexDirection direction) {
    setStyle(FlexProperties.PROP_DIRECTION, direction.getValue());
    return this;
  }

  /**
   * Get the layout direction.
   * 
   * @return the direction
   */
  public FlexDirection getDirection() {
    String direction = Optional.ofNullable(getStyle(FlexProperties.PROP_DIRECTION))
        .orElse(FlexDirection.getDefault().getValue());

    return FlexDirection.fromValue(direction);
  }

  /**
   * Set the layout wrap mode.
   * 
   * By default, items will all try to fit onto one line. You can change
   * that and allow the items to wrap as needed with this option. The
   * default value is {@link FlexWrap#NOWRAP}.
   * 
   * @see <a href="https://css-tricks.com/almanac/properties/f/flex-wrap/>visual
   *      demos of flex-wrap</a>
   * 
   * @param wrap the wrap
   * @return this layout
   */
  public FlexLayout setWrap(FlexWrap wrap) {
    setStyle(FlexProperties.PROP_WRAP, wrap.getValue());
    return this;
  }

  /**
   * Get the layout wrap mode.
   * 
   * @return the wrap
   */
  public FlexWrap getWrap() {
    String wrap = Optional.ofNullable(getStyle(FlexProperties.PROP_WRAP))
        .orElse(FlexWrap.getDefault().getValue());

    return FlexWrap.fromValue(wrap);
  }

  /**
   * Set the layout flow.
   * 
   * This is a shorthand for setting both {@link #setDirection(FlexDirection)}
   * and {@link #setWrap(FlexWrap)} at the same time. The default value is
   * {@link FlexFlow#ROW_NOWRAP}.
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
   * Get the layout flow.
   * 
   * This is a shorthand for getting both {@link #getDirection()} and
   * {@link #getWrap()} at the same time.
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
   * Set the {@link FlexJustifyContent} use by the layout.
   * 
   * This defines the alignment along the main axis. it helps distribute extra
   * free space left over when either all the items on a line are inflexible, or
   * are flexible but have reached their maximum size. It also exerts some
   * control over the alignment of items when they overflow the line. The
   * default value is {@link FlexJustifyContent#START}
   * 
   * @param justifyContent the justify content
   * @return this layout
   */
  public FlexLayout setJustifyContent(FlexJustifyContent justifyContent) {
    setStyle(FlexProperties.PROP_JUSTIFY_CONTENT, justifyContent.getValue());
    return this;
  }

  /**
   * Get the {@link FlexJustifyContent} use by the layout.
   * 
   * @return the justify content
   */
  public FlexJustifyContent getJustifyContent() {
    String justifyContent = Optional.ofNullable(getStyle(FlexProperties.PROP_JUSTIFY_CONTENT))
        .orElse(FlexJustifyContent.getDefault().getValue());

    return FlexJustifyContent.fromValue(justifyContent);
  }

  /**
   * Set the {@link FlexAlignment} use by the layout.
   * 
   * Defines the default behaviour for how items are laid out along the
   * cross axis on the current line. Think of it as the
   * {@link #setJustifyContent(FlexJustifyContent)} version for the cross-axis
   * (perpendicular to the main-axis). The default value is
   * {@link FlexAlignment#STRETCH}
   * 
   * @param alignItems the align items
   * @return this layout
   */
  public FlexLayout setAlignment(FlexAlignment alignItems) {
    setStyle(FlexProperties.PROP_ALIGN_ITEMS, alignItems.getValue());
    return this;
  }

  /**
   * Get the {@link FlexAlignment} use by the layout.
   * 
   * @return the align items
   */
  public FlexAlignment getAlignment() {
    String alignItems = Optional.ofNullable(getStyle(FlexProperties.PROP_ALIGN_ITEMS))
        .orElse(FlexAlignment.getDefault().getValue());

    return FlexAlignment.fromValue(alignItems);
  }

  /**
   * Set the {@link FlexContentAlignment} use by the layout.
   * 
   * The aligns the layout's lines within when there is extra space in the
   * cross-axis, similar to how {@link #setJustifyContent(FlexJustifyContent)}
   * aligns
   * individual items within the main-axis. The default value is
   * {@link FlexContentAlignment#NORMAL}
   * 
   * <p>
   * Note: This property has no effect when the layout has only one
   * line of items. In this case, the {@link #setAlignment(FlexAlignment)}
   * property is used instead. This property also has no effect when the
   * {@link #setWrap(FlexWrap)} property is set to {@link FlexWrap#NOWRAP}.
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
   * Get the {@link FlexContentAlignment} use by the layout.
   * 
   * @return the align content
   */
  public FlexContentAlignment getAlignContent() {
    String alignContent = Optional.ofNullable(getStyle(FlexProperties.PROP_ALIGN_CONTENT))
        .orElse(FlexContentAlignment.getDefault().getValue());

    return FlexContentAlignment.fromValue(alignContent);
  }

  /**
   * Set the gap between items.
   * 
   * The gap property explicitly controls the space between items. It applies
   * that spacing only between items not on the outer edges.
   * 
   * <p>
   * The behavior could be thought of as a minimum gutter, as if the gutter is
   * bigger somehow (because of something like
   * <code>setAlignContent(FlexContentAlignment.SPACE_BETWEEN)</code>
   * then the gap will only take effect if that space would end up smaller.
   * </p>
   * 
   * @param spacing
   * @return
   */
  public FlexLayout setSpacing(String spacing) {
    setStyle(FlexProperties.PROP_GAP, spacing);
    return this;
  }

  /**
   * Get the gap between items.
   * 
   * @return the gap
   */
  public String getSpacing() {
    String gap = Optional.ofNullable(getStyle(FlexProperties.PROP_GAP))
        .orElse("");

    return gap;
  }

  /**
   * Set the layout margin
   * 
   * @param margin the margin
   * @return this layout
   */
  public FlexLayout setMargin(String margin) {
    setStyle("margin", margin);
    return this;
  }

  /**
   * Set the layout margin
   * 
   * @param margin the margin
   * @return this layout
   */
  public String getMargin() {
    String margin = Optional.ofNullable(getStyle("margin"))
        .orElse("");

    return margin;
  }

  /**
   * Set the layout padding
   * 
   * @param padding the padding
   * @return this layout
   */
  public FlexLayout setPadding(String padding) {
    setStyle("padding", padding);
    return this;
  }

  /**
   * Set the layout padding
   * 
   * @param padding the padding
   * @return this layout
   */
  public String getPadding() {
    String padding = Optional.ofNullable(getStyle("gap"))
        .orElse("");

    return padding;
  }

  /**
   * Set the order of given control
   * 
   * By default, items will be laid out in the source order. However, the
   * order property controls the order in which they appear in the layout.
   * 
   * @param order   the order
   * @param control the control
   * 
   * @return this layout
   */
  public FlexLayout setItemOrder(int order, HasStyle control) {
    if (order == 0) {
      control.setStyle(FlexProperties.PROP_ORDER, "");
    } else {
      control.setStyle(FlexProperties.PROP_ORDER, String.valueOf(order));
    }

    return this;
  }

  /**
   * Get the order of given control
   * 
   * @param control the control
   * @return the order
   */
  public int getItemOrder(HasStyle control) {
    String order = Optional.ofNullable(control.getStyle(FlexProperties.PROP_ORDER))
        .orElse("0");

    return Integer.parseInt(order);
  }

  /**
   * Set the flex grow for the given items.
   * 
   * The defines the ability for a control to grow if necessary. It accepts a
   * numeric value that serves as a proportion. It dictates what amount of the
   * available space inside the layout the control should take up.
   * 
   * <p>
   * If all controls have flex-grow set to 1, the remaining space in the
   * layout will be distributed equally to all controls. If one of the
   * items has a value of 2, the remaining space would take up twice as
   * much space as the others (or it will try to, at least).
   * <p>
   * 
   * @param grow  the grow to set. If {@code 0} the flex grow is removed
   *              from the control
   * @param items the items
   * 
   * @return this layout
   * @throws IllegalArgumentException if the grow is negative
   */
  public FlexLayout setItemGrow(double grow, HasStyle... items) {
    if (grow < 0) {
      throw new IllegalArgumentException("Flex grow cannot be negative");
    }

    if (grow == 0) {
      for (HasStyle control : items) {
        control.setStyle(FlexProperties.PROP_GROW, "");
      }
    } else {
      for (HasStyle control : items) {
        control.setStyle(FlexProperties.PROP_GROW, String.valueOf(grow));
      }
    }

    return this;
  }

  /**
   * Get the flex grow for the given control.
   * 
   * @param control the control
   * @return the flex grow
   */
  public double getItemGrow(HasStyle control) {
    String grow = Optional.ofNullable(control.getStyle(FlexProperties.PROP_GROW))
        .orElse("0");

    return Double.parseDouble(grow);
  }

  /**
   * Set the flex shrink for the given items.
   * 
   * The defines the ability for a control to shrink if necessary.
   * 
   * @param shrink the shrink to set.
   * @param items  the items
   * 
   * @return this layout
   */
  public FlexLayout setItemShrink(double shrink, HasStyle... items) {
    if (shrink < 0) {
      throw new IllegalArgumentException("Flex shrink cannot be negative");
    }

    for (HasStyle control : items) {
      control.setStyle(FlexProperties.PROP_SHRINK, String.valueOf(shrink));
    }

    return this;
  }

  /**
   * Get the flex shrink for the given control.
   * 
   * @param control the control
   * @return the flex shrink
   */
  public double getItemShrink(HasStyle control) {
    String shrink = Optional.ofNullable(control.getStyle(FlexProperties.PROP_SHRINK))
        .orElse("1");

    return Double.parseDouble(shrink);
  }

  /**
   * Set the flex basis for the given items.
   * 
   * The defines the default size of an item before the remaining space is
   * distributed. It can be a length (e.g. 20%, 5rem, etc.) or a keyword.
   * for instance, the "auto" keyword means "look at my width or height property".
   * 
   * @param width the width to set. If {@code null} is passed then the
   *              flex-basis will be removed.
   * @param items the items
   * 
   * @return this layout
   */
  public FlexLayout setItemBasis(String width, HasStyle... items) {
    if (width == null) {
      for (HasStyle control : items) {
        control.setStyle(FlexProperties.PROP_BASIS, "");
      }
    } else {
      for (HasStyle control : items) {
        control.setStyle(FlexProperties.PROP_BASIS, width);
      }
    }
    return this;
  }

  /**
   * Get the flex basis for the given control.
   * 
   * @param control the control
   * @return the flex basis
   */
  public String getItemBasis(HasStyle control) {
    String basis = Optional.ofNullable(control.getStyle(FlexProperties.PROP_BASIS))
        .orElse("auto");

    return basis;
  }

  /**
   * Set the alignment of given items
   * 
   * This allows the default alignment (or the one specified by
   * {@link #setAlignment(FlexAlignment)}) to be overridden for individual items.
   * 
   * @param alignSelf the alignment
   * @param items     the items
   * 
   * @return this layout
   * 
   * @see #setAlignment(FlexAlignment)
   */
  public FlexLayout setItemAlignment(FlexAlignment alignSelf, HasStyle... items) {
    if (alignSelf == null) {
      for (HasStyle control : items) {
        control.setStyle(FlexProperties.PROP_ALIGN_SELF, "");
      }
    } else {
      for (HasStyle control : items) {
        control.setStyle(FlexProperties.PROP_ALIGN_SELF, alignSelf.getValue());
      }
    }

    return this;
  }

  /**
   * Get the alignment of given control
   * 
   * @param control the control
   * @return the alignment
   */
  public FlexAlignment getItemAlignment(HasStyle control) {
    String alignSelf = Optional.ofNullable(control.getStyle(FlexProperties.PROP_ALIGN_SELF))
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
