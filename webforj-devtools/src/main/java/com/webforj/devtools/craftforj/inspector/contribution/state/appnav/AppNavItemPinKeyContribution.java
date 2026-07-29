package com.webforj.devtools.craftforj.inspector.contribution.state.appnav;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.appnav.AppNavItem;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for AppNavItem pin key.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AppNavItemPinKeyContribution extends ConcernContribution<AppNavItem> {

  /**
   * Creates a new AppNavItemPinKeyContribution.
   */
  public AppNavItemPinKeyContribution() {
    super(AppNavItem.class, "PinKey", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(AppNavItem::getPinKey);
    setSetter((c, v) -> c.setPinKey(String.valueOf(v)));
  }
}
