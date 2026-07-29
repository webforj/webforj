package com.webforj.devtools.craftforj.inspector.contribution.layout.applayout;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.layout.applayout.AppLayout;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for AppLayout drawerPlacement property.
 *
 * @author webforJ craftforJ
 * @since 26.02
 *
 */
@AutoService(FeatureHandler.class)
public class AppLayoutDrawerPlacementContribution extends EnumConcernContribution<AppLayout> {

  /**
   * Creates a new contribution.
   */
  public AppLayoutDrawerPlacementContribution() {
    super(AppLayout.class, "DrawerPlacement", FeatureCategory.LAYOUT);
    setGetter(AppLayout::getDrawerPlacement);
    setSetter((c, v) -> c.setDrawerPlacement((AppLayout.DrawerPlacement) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return AppLayout.DrawerPlacement.class;
  }

}
