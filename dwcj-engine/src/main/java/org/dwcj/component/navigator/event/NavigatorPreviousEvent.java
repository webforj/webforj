package org.dwcj.component.navigator.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.navigator.Navigator;

public final class NavigatorPreviousEvent implements ControlEvent {

  private final Navigator control;

  public NavigatorPreviousEvent(Navigator navigator) {
    this.control = navigator;
  }

  @Override
  public Navigator getControl() {
    return control;
  }
}
