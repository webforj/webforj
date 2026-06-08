package com.webforj.bundle;

import com.webforj.component.Component;
import com.webforj.component.ComponentLifecycleObserver;

/**
 * Loads the bundle outputs bound to a component the moment it is created, wherever it is used and
 * however deeply it is nested.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class BundleComponentObserver implements ComponentLifecycleObserver {

  private final BundleAssetLoader loader = new BundleAssetLoader();

  /**
   * {@inheritDoc}
   */
  @Override
  public void onComponentLifecycleEvent(Component component, LifecycleEvent event) {
    if (event == LifecycleEvent.CREATE) {
      loader.loadFor(component.getClass());
    }
  }
}
