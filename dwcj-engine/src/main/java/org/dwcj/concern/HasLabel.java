package org.dwcj.concern;

import org.dwcj.component.Component;

/**
 * This interface defines a contract for components that have a label.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasLabel<T extends Component> {

  /**
   * Gets the label associated with the component.
   *
   * @return A string representing the label of the component.
   */
  public String getLabel();

  /**
   * Sets the component's label.
   *
   * <p>
   * A label is a descriptive text or title that is associated with the component. It provides a
   * brief explanation or prompt to help users understand the purpose or expected input for that
   * particular component. Labels are not only important for usability but also play a crucial role
   * in accessibility, as they enable screen readers and assistive technologies to provide accurate
   * information and facilitate keyboard navigation.
   * </p>
   *
   * @param label The label to set for the component.
   * @return The component itself.
   */
  public T setLabel(String label);
}
