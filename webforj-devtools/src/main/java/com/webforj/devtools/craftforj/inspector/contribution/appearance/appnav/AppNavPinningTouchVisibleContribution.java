package com.webforj.devtools.craftforj.inspector.contribution.appearance.appnav;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for AppNav pin control visibility on touch devices.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AppNavPinningTouchVisibleContribution extends ConcernContribution<AppNav> {

  /**
   * Creates a new AppNavPinningTouchVisibleContribution.
   */
  public AppNavPinningTouchVisibleContribution() {
    super(AppNav.class, "PinningTouchVisible", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(c -> c.getPinning().isTouchVisible());
    setSetter((c, v) -> c.getPinning().setTouchVisible(Boolean.TRUE.equals(v)));
  }

  @Override
  public String getSourceMethodName(String propertyName) {
    return "setTouchVisible";
  }

  @Override
  public String getSourceAccessor() {
    return "getPinning";
  }
}
