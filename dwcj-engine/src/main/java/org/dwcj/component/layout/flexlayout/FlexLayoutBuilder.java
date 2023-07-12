package org.dwcj.component.layout.flexlayout;

import org.dwcj.component.AbstractComponent;

/**
 * A builder for {@link FlexLayout}.
 *
 * @author Hyyan Abo Fakher
 */
public final class FlexLayoutBuilder {
  private boolean isInline = false;
  private FlexAlignment alignment = null;
  private FlexContentAlignment contentAlignment = null;
  private FlexDirection direction = null;
  private FlexJustifyContent justifyContent = null;
  private FlexWrap wrap = null;
  private AbstractComponent[] children = null;

  @FunctionalInterface
  interface Builder {
    FlexLayoutBuilder getInstance();
  }

  /**
   * Set the alignment of the controls inside the layout.
   *
   * @see <a href= "https://developer.mozilla.org/en-US/docs/Web/CSS/align-items">align-items
   *      property</a>
   * @author Hyyan Abo Fakher
   */
  public interface FlexAlignmentBuilder extends Builder {

    /**
     * The items are placed at the start of the cross axis.
     *
     * @return the builder
     */
    default FlexLayoutBuilder start() {
      getInstance().alignment = FlexAlignment.START;
      return getInstance();
    }

    /**
     * The items are centered along the cross axis.
     *
     * @return the builder
     */
    default FlexLayoutBuilder center() {
      getInstance().alignment = FlexAlignment.CENTER;
      return getInstance();
    }

    /**
     * The items are placed at the end of the cross axis.
     *
     * @return the builder
     */
    default FlexLayoutBuilder end() {
      getInstance().alignment = FlexAlignment.END;
      return getInstance();
    }

    /**
     * The items are stretched to fit the container.
     *
     * @return the builder
     */
    default FlexLayoutBuilder stretch() {
      getInstance().alignment = FlexAlignment.STRETCH;
      return getInstance();
    }

    /**
     * The items are aligned such as their baselines align.
     *
     * @return the builder
     */
    default FlexLayoutBuilder baseline() {
      getInstance().alignment = FlexAlignment.BASELINE;
      return getInstance();
    }

    /**
     * The items are evenly distributed in the container.
     *
     * @return the builder
     */
    default FlexLayoutBuilder auto() {
      getInstance().alignment = FlexAlignment.AUTO;
      return getInstance();
    }
  }

  /**
   * Set the distribution of empty space between and around items along the cross-axis.
   *
   * @see <a href= "https://developer.mozilla.org/en-US/docs/Web/CSS/align-content"> align-content
   *      property</a>
   * @author Hyyan Abo Fakher
   */
  public interface FlexContentAlignmentBuilder extends Builder {

    /**
     * The items are packed toward the start line.
     *
     * @return the builder
     */
    default FlexLayoutBuilder start() {
      getInstance().contentAlignment = FlexContentAlignment.START;
      return getInstance();
    }

    /**
     * The items are packed toward the center of the line.
     *
     * @return the builder
     */
    default FlexLayoutBuilder center() {
      getInstance().contentAlignment = FlexContentAlignment.CENTER;
      return getInstance();
    }

    /**
     * The items are packed toward the end line.
     *
     * @return the builder
     */
    default FlexLayoutBuilder end() {
      getInstance().contentAlignment = FlexContentAlignment.END;
      return getInstance();
    }

    /**
     * The items are evenly distributed in the line; first item is on the start line, last item on
     * the end line.
     *
     * @return the builder
     */
    default FlexLayoutBuilder between() {
      getInstance().contentAlignment = FlexContentAlignment.BETWEEN;
      return getInstance();
    }

    /**
     * The items are evenly distributed in the line with equal space around them.
     *
     * @return the builder
     */
    default FlexLayoutBuilder around() {
      getInstance().contentAlignment = FlexContentAlignment.AROUND;
      return getInstance();
    }

    /**
     * The lines stretch to take up the remaining space.
     *
     * @return the builder
     */
    default FlexLayoutBuilder stretch() {
      getInstance().contentAlignment = FlexContentAlignment.STRETCH;
      return getInstance();
    }

    /**
     * The items are packed in their default position as if no value was set.
     *
     * @return the builder
     */
    default FlexLayoutBuilder normal() {
      getInstance().contentAlignment = FlexContentAlignment.NORMAL;
      return getInstance();
    }
  }

  /**
   * Set the way the space inside the layout is distributed between and around the controls.
   *
   * @author Hyyan Abo Fakher
   */
  public interface FlexJustifyContentBuilder extends Builder {

    /**
     * Controls are packed toward the start line.
     *
     * @return the builder
     */
    default FlexLayoutBuilder start() {
      getInstance().justifyContent = FlexJustifyContent.START;
      return getInstance();
    }

    /**
     * Controls are packed toward the center of the line.
     *
     * @return the builder
     */
    default FlexLayoutBuilder center() {
      getInstance().justifyContent = FlexJustifyContent.CENTER;
      return getInstance();
    }

    /**
     * Controls are packed toward the end line.
     *
     * @return the builder
     */
    default FlexLayoutBuilder end() {
      getInstance().justifyContent = FlexJustifyContent.END;
      return getInstance();
    }

    /**
     * Controls are evenly distributed in the line; first item is on the start line, last item on
     * the end line.
     *
     * @return the builder
     */
    default FlexLayoutBuilder between() {
      getInstance().justifyContent = FlexJustifyContent.BETWEEN;
      return getInstance();
    }

    /**
     * Controls are evenly distributed in the line with equal space around them.
     *
     * @return the builder
     */
    default FlexLayoutBuilder around() {
      getInstance().justifyContent = FlexJustifyContent.AROUND;
      return getInstance();
    }

    /**
     * Controls are distributed with equal space around them. The first line is flushed to the start
     * of the container while the last one is flushed to the end.
     *
     * @return the builder
     */
    default FlexLayoutBuilder evenly() {
      getInstance().justifyContent = FlexJustifyContent.EVENLY;
      return getInstance();
    }
  }

  /**
   * Create a new FlexLayoutBuilder with the given controls.
   *
   * @param control the controls
   */
  public FlexLayoutBuilder(AbstractComponent... control) {
    children = control;
  }

  /**
   * Set the layout direction to be horizontal. Left to right for LTR, right to left for RTL.
   *
   * @param inline if true, the layout will be inline (default is false)
   * @return the builder
   */
  public FlexLayoutBuilder horizontal(boolean inline) {
    direction = FlexDirection.ROW;
    isInline = inline;
    return this;
  }

  /**
   * Set the layout direction to be horizontal. Left to right for LTR, right to left for RTL.
   *
   * @return the builder
   */
  public FlexLayoutBuilder horizontal() {
    return horizontal(false);
  }

  /**
   * Set the layout direction to be horizontal. Right to left for LTR, left to right for RTL.
   *
   * @param inline if true, the layout will be inline (default is false)
   * @return the builder
   */
  public FlexLayoutBuilder horizontalReverse(boolean inline) {
    direction = FlexDirection.ROW_REVERSE;
    isInline = inline;
    return this;
  }

  /**
   * Set the layout direction to be horizontal. Right to left for LTR, left to right for RTL.
   *
   * @return the builder
   */
  public FlexLayoutBuilder horizontalReverse() {
    return horizontalReverse(false);
  }

  /**
   * Set the layout direction to be vertical. Top to bottom.
   *
   * @param inline if true, the layout will be inline (default is false)
   * @return the builder
   */
  public FlexLayoutBuilder vertical(boolean inline) {
    direction = FlexDirection.COLUMN;
    isInline = inline;
    return this;
  }

  /**
   * Set the layout direction to be vertical. Top to bottom.
   *
   * @return the builder
   */
  public FlexLayoutBuilder vertical() {
    return vertical(false);
  }

  /**
   * Set the layout direction to be vertical. Bottom to top.
   *
   * @param inline if true, the layout will be inline (default is false)
   * @return the builder
   */
  public FlexLayoutBuilder verticalReverse(boolean inline) {
    direction = FlexDirection.COLUMN_REVERSE;
    isInline = inline;
    return this;
  }

  /**
   * Set the layout direction to be vertical. Bottom to top.
   *
   * @return the builder
   */
  public FlexLayoutBuilder verticalReverse() {
    return verticalReverse(false);
  }

  /**
   * Set the alignment of items on the cross-axis.
   *
   * @return the builder
   */
  public FlexAlignmentBuilder align() {
    return () -> this;
  }

  /**
   * Set the distribution of empty space between and around items along the cross-axis.
   *
   * @return the builder
   */
  public FlexContentAlignmentBuilder contentAlign() {
    return () -> this;
  }

  /**
   * Set the way the space inside the layout is distributed between and around the controls.
   *
   * @return the builder
   */
  public FlexJustifyContentBuilder justify() {
    return () -> this;
  }

  /**
   * The items will wrap onto multiple lines, from top to bottom.
   *
   * @return the builder
   */
  public FlexLayoutBuilder wrap() {
    wrap = FlexWrap.WRAP;
    return this;
  }

  /**
   * The items will be on one line.
   *
   * @return the builder
   */
  public FlexLayoutBuilder nowrap() {
    wrap = FlexWrap.NOWRAP;
    return this;
  }

  /**
   * The items will wrap onto multiple lines from bottom to top.
   *
   * @return the builder
   */
  public FlexLayoutBuilder wrapReverse() {
    wrap = FlexWrap.WRAP_REVERSE;
    return this;
  }

  /**
   * Check if the layout is inline.
   *
   * @return true if the layout is inline
   */
  public boolean isInline() {
    return isInline;
  }

  /**
   * Get the layout alignment.
   *
   * @return the layout alignment
   */
  public FlexAlignment getAlignment() {
    return alignment;
  }

  /**
   * Get the layout content alignment.
   *
   * @return the layout content alignment
   */
  public FlexContentAlignment getContentAlignment() {
    return contentAlignment;
  }

  /**
   * Get the layout direction.
   *
   * @return the layout direction
   */
  public FlexDirection getDirection() {
    return direction;
  }

  /**
   * Get the layout justify content.
   *
   * @return the layout justify content
   */
  public FlexJustifyContent getJustifyContent() {
    return justifyContent;
  }

  /**
   * Get the layout wrap.
   *
   * @return the layout wrap
   */
  public FlexWrap getWrap() {
    return wrap;
  }

  /**
   * Build the FlexLayout.
   *
   * @return the FlexLayout
   */
  public FlexLayout build() {
    return new FlexLayout(this, children);
  }
}
