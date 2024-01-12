package org.dwcj.component.tabbedpane.event;

import java.util.Map;
import org.dwcj.component.tabbedpane.TabbedPane;

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
