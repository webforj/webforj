package org.dwcj.component.navigator.event;

import org.dwcj.component.ControlEvent;
import org.dwcj.component.navigator.Navigator;

public final class NavigatorNextEvent implements ControlEvent {

  private final Navigator control;

  public NavigatorNextEvent(Navigator navigator) {
    this.control = navigator;
  }

  @Override
  public Navigator getControl() {
    return control;
  }
}
