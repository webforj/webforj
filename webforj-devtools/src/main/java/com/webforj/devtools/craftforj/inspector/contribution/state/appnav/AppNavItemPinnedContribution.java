package com.webforj.devtools.craftforj.inspector.contribution.state.appnav;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.appnav.AppNavItem;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for AppNavItem pinned state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AppNavItemPinnedContribution extends ConcernContribution<AppNavItem> {

  /**
   * Creates a new AppNavItemPinnedContribution.
   */
  public AppNavItemPinnedContribution() {
    super(AppNavItem.class, "Pinned", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(AppNavItem::isPinned);
    setSetter((c, v) -> c.setPinned(Boolean.TRUE.equals(v)));
  }
}
