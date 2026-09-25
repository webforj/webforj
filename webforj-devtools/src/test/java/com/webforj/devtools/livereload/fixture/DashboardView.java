package com.webforj.devtools.livereload.fixture;

import com.webforj.component.Component;
import com.webforj.component.window.Window;

/**
 * A route that reaches a card through a panel.
 */
public class DashboardView extends Component {
  private final DashboardPanel panel = new DashboardPanel();

  @Override
  protected void onCreate(Window window) {
    // Do nothing
  }

  @Override
  protected void onDestroy() {
    // Do nothing
  }
}
