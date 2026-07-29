package com.webforj.devtools.craftforj.inspector.contribution.appearance.tabbedpane;

import com.google.auto.service.AutoService;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for TabbedPane hide active indicator mode.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TabbedPaneHideActiveIndicatorContribution extends ConcernContribution<TabbedPane> {

  /**
   * Creates a new TabbedPaneHideActiveIndicatorContribution.
   */
  public TabbedPaneHideActiveIndicatorContribution() {
    super(TabbedPane.class, "HideActiveIndicator", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(TabbedPane::isHideActiveIndicator);
    setSetter((c, v) -> c.setHideActiveIndicator(Boolean.TRUE.equals(v)));
  }

}
