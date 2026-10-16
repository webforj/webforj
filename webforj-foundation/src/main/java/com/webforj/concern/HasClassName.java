package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import java.util.List;

/**
 * An interface that enables adding or removing CSS classes to a component.
 *
 * <p>
 * This interface provides methods for adding or removing CSS classes to/from a component's list of
 * CSS classes.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasClassName<T extends Component> {

  /**
   * Adds a CSS class to the list of CSS classes for the component.
   *
   * @param classNames the name of the CSS class to be added
   * @return the component itself after adding the class
   */
  public default T addClassName(String... classNames) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasClassName) {
      ((HasClassName<?>) component).addClassName(classNames);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support CSS classes");
  }

  /**
   * Removes a CSS class from the list of CSS classes for the component.
   *
   * @param classNames the name of the CSS class to be removed
   * @return the component itself after removing the class
   */
  public default T removeClassName(String... classNames) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasClassName) {
      ((HasClassName<?>) component).removeClassName(classNames);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support CSS classes");
  }

  /**
   * Gets the list of CSS classes for the component.
   *
   * @return the names of the CSS classes of the component
   * @throws UnsupportedOperationException if the component does not support CSS classes
   * @throws IllegalArgumentException if the component is not a composite and does not implement
   *         this method
   */
  public default List<String> getClassNameList() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasClassName) {
      return ((HasClassName<?>) component).getClassNameList();
    }

    throw new UnsupportedOperationException("The component does not support CSS classes");
  }
}
