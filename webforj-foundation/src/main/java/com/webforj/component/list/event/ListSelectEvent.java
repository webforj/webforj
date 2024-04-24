package com.webforj.component.list.event;

import com.webforj.component.list.DwcList;
import java.util.Map;

/**
 * An event which is fired when the user selects an item from a list.
 *
 * <p>
 * This event can be fired when an item is clicked on, or navigated to via keyboard interactions.
 * </p>
 *
 * @param <V> the type of the list items
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class ListSelectEvent<V> extends ListEvent<V> {

  /**
   * Creates a new event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  public ListSelectEvent(DwcList<?, V> component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DwcList<?, V> getComponent() {
    return (DwcList<?, V>) super.getComponent();
  }
}
