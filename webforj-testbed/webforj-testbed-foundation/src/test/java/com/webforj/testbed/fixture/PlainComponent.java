package com.webforj.testbed.fixture;

import com.webforj.component.Component;
import com.webforj.component.window.Window;

/** Provides a component without a client control for tests. */
public class PlainComponent extends Component {

  @Override
  protected void onCreate(Window window) {
    // no control needed for this fixture
  }

  @Override
  protected void onDestroy() {
    // nothing to release
  }
}
