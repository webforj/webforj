package org.dwcj.component.tabbedpane.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.tabbedpane.TabbedPane;

public final class TabSelectEvent implements ControlEvent {
  private final TabbedPane control;
  private final int index;
  private final String title;

  public TabSelectEvent(TabbedPane tabControl, int index, String title) {
    this.control = tabControl;
    this.index = index;
    this.title = title;
  }

  @Override
  public TabbedPane getControl() {
    return control;
  }

  public int getIndex() {
    return index;
  }

  public String getTitle() {
    return title;
  }

  public String toString() {
    return "Event: Tab " + index + " Selected";
  }

}
