package com.webforj.testbed.runtime;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.bridge.AppAccessor;
import com.webforj.router.history.MemoryHistory;

/**
 * Configures application history before a testbed application initializes.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class TestbedAppLifecycleListener implements AppLifecycleListener {
  /**
   * {@inheritDoc}
   */
  @Override
  public void onDidCreate(App app) {
    if (TestbedRuntime.isRunning()) {
      AppAccessor.getDefault().setRouterHistory(app, new MemoryHistory());
    }
  }
}
