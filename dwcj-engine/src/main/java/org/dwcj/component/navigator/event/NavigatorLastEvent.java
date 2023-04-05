package org.dwcj.component.navigator.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.navigator.Navigator;

public final class NavigatorLastEvent implements ComponentEvent {

  private final Navigator control;

  public NavigatorLastEvent(Navigator navigator) {
    this.control = navigator;
  }

  @Override
  public Navigator getControl() {
    return control;
  }
}
