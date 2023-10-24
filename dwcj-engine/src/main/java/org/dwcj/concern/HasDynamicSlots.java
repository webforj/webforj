package org.dwcj.concern;

import org.dwcj.component.Component;

/**
 * This interface defines methods that shall be implemented by components that offer dynamic slots.
 * Such UI components could be Tab Control, Carousel, Accordion and similar, where the developer can
 * add (and remove) slots at runtime.
 *
 * @param <T> the component it implements
 */
public interface HasDynamicSlots<T extends Component> extends HasSlots {


  /**
   * Adds a slot to the component.
   *
   * @param slotName a name representing the dynamic slot to add
   * @return the component itself
   */
  T createSlot(String slotName);

  /**
   * Deletes one of the slots that had been created dynamically.
   *
   * @param slotName - the slot to be deleted
   * @return - the component itself
   */
  T removeSlot(String slotName);

  /**
   * Method that returns the current slot count. (TBD The count can also be determined outside by
   * retrieving the slot list and counting the items. This method proposes developer convenience)
   *
   * @return the number of slots the component supports
   */
  int getNumSlots();

}
