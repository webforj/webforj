package com.webforj.component.list.event;

import com.webforj.component.list.DwcSelectDropdown;
import java.util.Map;

/**
 * This event is triggered when the user clicks an item from a List-based component.
 *
 * <p>
 * It provides essential information about the selected item and allows developers to implement
 * custom actions or responses when an item is chosen.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class ListClickEvent extends ListEvent {

  /**
   * Creates a new event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  public ListClickEvent(DwcSelectDropdown<?> component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DwcSelectDropdown<?> getComponent() {
    return (DwcSelectDropdown<?>) super.getComponent();
  }
}
