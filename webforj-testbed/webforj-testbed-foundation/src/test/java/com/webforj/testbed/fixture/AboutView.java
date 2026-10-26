package com.webforj.testbed.fixture;

import com.webforj.component.Composite;
import com.webforj.component.html.elements.Div;
import com.webforj.router.annotation.Route;
import com.webforj.router.event.DidEnterEvent;
import com.webforj.router.event.DidLeaveEvent;
import com.webforj.router.history.ParametersBag;
import com.webforj.router.observer.DidEnterObserver;
import com.webforj.router.observer.DidLeaveObserver;

/** Tracks route entry and exit for navigation tests. */
@Route("about")
public class AboutView extends Composite<Div> implements DidEnterObserver, DidLeaveObserver {
  private int enterCount = 0;
  private int leaveCount = 0;

  @Override
  public void onDidEnter(DidEnterEvent event, ParametersBag parameters) {
    enterCount++;
  }

  @Override
  public void onDidLeave(DidLeaveEvent event, ParametersBag parameters) {
    leaveCount++;
  }

  public int getEnterCount() {
    return enterCount;
  }

  public int getLeaveCount() {
    return leaveCount;
  }
}
