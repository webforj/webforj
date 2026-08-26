package com.webforj.push;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.Page;

/**
 * Registers the push worker in the browser when the deployment configures push.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class PushLifecycleListener implements AppLifecycleListener {

  /**
   * {@inheritDoc}
   */
  @Override
  public void onDidRun(App app) {
    if (!Environment.isPresent() || !Page.isPresent() || Page.getCurrent().isEmbedded()) {
      return;
    }

    PushConfiguration.fromConfig(Environment.getCurrent().getConfig())
        .ifPresent(configuration -> Push.getCurrent().registerServiceWorker());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillTerminate(App app) {
    if (Push.isPresent()) {
      Push.getCurrent().destroy();
    }
  }
}
