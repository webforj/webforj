package org.dwcj.component.tabbedpane.event;

import java.util.Map;
import org.dwcj.component.tabbedpane.TabbedPane;

/**
 * An event that is fired when a tab is closed.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public class TabCloseEvent extends TabEvent {

  /**
   * Constructs a new TabbedPaneCloseEvent.
   *
   * @param component the tabbed pane component
   * @param eventMap the event map
   */
  public TabCloseEvent(TabbedPane component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
