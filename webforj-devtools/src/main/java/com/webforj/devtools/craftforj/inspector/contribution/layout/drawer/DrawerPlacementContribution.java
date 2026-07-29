package com.webforj.devtools.craftforj.inspector.contribution.layout.drawer;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.drawer.Drawer;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for Drawer placement property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class DrawerPlacementContribution extends EnumConcernContribution<Drawer> {

  /**
   * Creates a new DrawerPlacementContribution.
   */
  public DrawerPlacementContribution() {
    super(Drawer.class, "Placement", FeatureCategory.LAYOUT);
    setGetter(Drawer::getPlacement);
    setSetter((c, v) -> c.setPlacement((Drawer.Placement) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Drawer.Placement.class;
  }

}
