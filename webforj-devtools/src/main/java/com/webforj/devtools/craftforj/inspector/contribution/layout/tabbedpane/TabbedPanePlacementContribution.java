package com.webforj.devtools.craftforj.inspector.contribution.layout.tabbedpane;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for TabbedPane placement.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TabbedPanePlacementContribution extends EnumConcernContribution<TabbedPane> {

  /**
   * Creates a new TabbedPanePlacementContribution.
   */
  public TabbedPanePlacementContribution() {
    super(TabbedPane.class, "Placement", FeatureCategory.LAYOUT);
    setGetter(TabbedPane::getPlacement);
    setSetter((c, v) -> c.setPlacement((TabbedPane.Placement) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return TabbedPane.Placement.class;
  }

}
