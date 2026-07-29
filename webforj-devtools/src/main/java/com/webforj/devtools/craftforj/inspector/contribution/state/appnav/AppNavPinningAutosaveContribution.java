package com.webforj.devtools.craftforj.inspector.contribution.state.appnav;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for AppNav pinning autosave mode.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AppNavPinningAutosaveContribution extends ConcernContribution<AppNav> {

  /**
   * Creates a new AppNavPinningAutosaveContribution.
   */
  public AppNavPinningAutosaveContribution() {
    super(AppNav.class, "PinningAutosave", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(c -> c.getPinning().isAutosave());
    setSetter((c, v) -> c.getPinning().setAutosave(Boolean.TRUE.equals(v)));
  }

  @Override
  public String getSourceMethodName(String propertyName) {
    return "setAutosave";
  }

  @Override
  public String getSourceAccessor() {
    return "getPinning";
  }
}
