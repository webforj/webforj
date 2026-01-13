package com.webforj.concern;

import com.webforj.annotation.Experimental;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for handling CSS styles on a component.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasStyle<T extends Component> {

  /**
   * Gets the value of a CSS property.
   *
   * <p>
   * This method is intended to be used to retrieve the value of a CSS property of a component.
   * </p>
   *
   * @see #getComputedStyle(java.lang.String)
   *
   * @param property The CSS property to be retrieved
   * @return String containing the value of the CSS property
   */
  public default String getStyle(String property) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasStyle) {
      return ((HasStyle<?>) component).getStyle(property);
    }

    throw new UnsupportedOperationException("The component does not support styles");
  }

  /**
   * Gets the computed value of a CSS property.
   *
   * <p>
   * This method is used to obtain the computed value of a CSS property for the component. The
   * computed value represents the final value that is applied to the element after considering all
   * styles applied to it, including styles inherited from parent elements and user-agent defaults.
   * </p>
   *
   * @see #getStyle(java.lang.String)
   *
   * @param property The CSS property to be retrieved
   * @return String containing all computed styles
   */
  public default String getComputedStyle(String property) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasStyle) {
      return ((HasStyle<?>) component).getComputedStyle(property);
    }

    throw new UnsupportedOperationException("The component does not support styles");
  }

  /**
   * Sets a CSS property to a specific value.
   *
   * <p>
   * This method is intended to be used to modify a single CSS property of a component.
   * </p>
   *
   * @param property The CSS property to be changed
   * @param value The value to be assigned to the CSS property
   *
   * @return @return The component itself.
   */
  public default T setStyle(String property, String value) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasStyle) {
      ((HasStyle<?>) component).setStyle(property, value);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support styles");
  }

  /**
   * Removes a CSS property to a specific value.
   *
   * @param property The CSS property to be changed
   *
   * @return @return The component itself.
   */
  public default T removeStyle(String property) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasStyle) {
      ((HasStyle<?>) component).removeStyle(property);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support styles");
  }

  /**
   * Sets the view transition name for this component.
   *
   * <p>
   * The view transition name is used to identify elements that should morph between views during
   * animated transitions. Elements with matching names in the old and new views will be smoothly
   * animated from one position/size to another.
   * </p>
   *
   * <p>
   * ⚠️ WARNING: This method is experimental since 25.11 and may change in future releases.
   * </p>
   *
   * @param name the transition name, or {@code null} to remove
   * @return the component itself
   *
   * @see #getViewTransitionName()
   *
   * @since 25.11
   */
  @Experimental(since = "25.11")
  public default T setViewTransitionName(String name) {
    if (name == null || name.isEmpty()) {
      return removeStyle("view-transition-name");
    }
    return setStyle("view-transition-name", name);
  }

  /**
   * Gets the view transition name for this component.
   *
   * <p>
   * ⚠️ WARNING: This method is experimental since 25.11 and may change in future releases.
   * </p>
   *
   * @return the transition name, or {@code null} if not set
   *
   * @see #setViewTransitionName(String)
   *
   * @since 25.11
   */
  @Experimental(since = "25.11")
  public default String getViewTransitionName() {
    return getStyle("view-transition-name");
  }

}
