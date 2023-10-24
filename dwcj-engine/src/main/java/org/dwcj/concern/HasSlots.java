package org.dwcj.concern;

import java.util.ArrayList;
import java.util.List;
import org.dwcj.component.Component;

/**
 * Interface defining methods to be implemented by components that have multiple slots Slots can be
 * filled with components.
 *
 * @param <T> the components implementing this interface.
 * @see HasDynamicSlots for components that support dynamic slot creation and removal at runtime
 */
public interface HasSlots<T extends Component> {

  /**
   * This method returns the list of defined slots. An empty string in the list represents the
   * default slot (if such exists). If a component supports dynamic slots, the list is dynamic.
   *
   * @return a list of the defined slots.
   */
  default List<String> getSlots() {
    // in the trivial case, we have one default slot
    List<String> al = new ArrayList<>();
    al.add("");
    return al;
  }

  /**
   * Adds a component into a named slot.
   *
   * @param slot The slot to which the component should be added.
   * @param c The component to add into the slow
   * @return the component itself
   */
  T add(String slot, Component c);

  /**
   * Adds a component into the default slot. This method coincides with the add method of Panel and
   * Frame on purpose.
   *
   * @param c the component to add
   * @return this component itself
   */
  default T add(Component c) {
    return add("", c);
  }

}
