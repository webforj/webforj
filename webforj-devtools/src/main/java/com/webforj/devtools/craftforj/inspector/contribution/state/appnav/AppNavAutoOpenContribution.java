package com.webforj.devtools.craftforj.inspector.contribution.state.appnav;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for AppNav auto open mode.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AppNavAutoOpenContribution extends ConcernContribution<AppNav> {

  /**
   * Creates a new AppNavAutoOpenContribution.
   */
  public AppNavAutoOpenContribution() {
    super(AppNav.class, "AutoOpen", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(AppNav::isAutoOpen);
    setSetter((c, v) -> c.setAutoOpen(Boolean.TRUE.equals(v)));
  }
}
