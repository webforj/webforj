package org.dwcj.component.tabbedpane.event;

import java.util.Map;
import org.dwcj.component.event.ComponentEvent;
import org.dwcj.component.tabbedpane.Tab;
import org.dwcj.component.tabbedpane.TabbedPane;

/**
 * The base class for all tab events.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
class TabEvent extends ComponentEvent<TabbedPane> {

  /**
   * Constructs a new TabEvent.
   *
   * @param component the tabbed pane component
   * @param eventMap the event map
   */
  TabEvent(TabbedPane component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }

  /**
   * Gets the tab which caused the event.
   *
   * @return the tab instance
   */
  public Tab getTab() {
    int index = getTabIndex();
    return getComponent().getTab(index);
  }

  /**
   * Gets the tab index which caused the event.
   *
   * @return the tab index
   */
  public int getTabIndex() {
    Object index = getEventMap().get("index");
    if (index == null) {
      return -1;
    }

    return Integer.parseInt(String.valueOf(index));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TabbedPane getComponent() {
    return (TabbedPane) super.getComponent();
  }
}
