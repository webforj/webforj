package com.webforj.devtools.craftforj.inspector.contribution.state.appnav;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for AppNav pinning toggle.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AppNavPinningEnabledContribution extends ConcernContribution<AppNav> {

  /**
   * Creates a new AppNavPinningEnabledContribution.
   */
  public AppNavPinningEnabledContribution() {
    super(AppNav.class, "PinningEnabled", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(c -> c.getPinning().isEnabled());
    setSetter((c, v) -> c.getPinning().setEnabled(Boolean.TRUE.equals(v)));
  }

  @Override
  public String getSourceMethodName(String propertyName) {
    return "setEnabled";
  }

  @Override
  public String getSourceAccessor() {
    return "getPinning";
  }
}
