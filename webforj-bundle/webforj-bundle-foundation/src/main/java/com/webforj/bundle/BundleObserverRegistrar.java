package com.webforj.bundle;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.annotation.AppListenerPriority;

/**
 * Loads the bundle outputs bound to the application class when the app is created.
 *
 * <p>
 * The outputs bound to a component load through {@link BundleComponentObserver} as each component
 * is created, so this listener only covers the entry the application class itself declares.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
@AppListenerPriority(1)
public class BundleObserverRegistrar implements AppLifecycleListener {

  private final BundleAssetLoader loader = new BundleAssetLoader();

  /**
   * {@inheritDoc}
   */
  @Override
  public void onDidCreate(App app) {
    loader.loadEager();
    loader.loadGlobal();
    loader.loadFor(app.getClass());
  }
}
