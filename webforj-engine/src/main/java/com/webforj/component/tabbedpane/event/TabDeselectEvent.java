package com.webforj.component.tabbedpane.event;

import com.webforj.component.tabbedpane.TabbedPane;
import java.util.Map;

/**
 * An event that is fired when a tab is deselected.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public class TabDeselectEvent extends TabEvent {

  /**
   * Constructs a new TabbedPaneDeselectEvent.
   *
   * @param component the tabbed pane component
   * @param eventMap the event map
   */
  public TabDeselectEvent(TabbedPane component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
