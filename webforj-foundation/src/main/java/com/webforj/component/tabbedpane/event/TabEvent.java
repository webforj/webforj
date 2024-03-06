package com.webforj.component.tabbedpane.event;

import com.webforj.component.event.ComponentEvent;
import com.webforj.component.tabbedpane.Tab;
import com.webforj.component.tabbedpane.TabbedPane;
import java.util.Map;

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
