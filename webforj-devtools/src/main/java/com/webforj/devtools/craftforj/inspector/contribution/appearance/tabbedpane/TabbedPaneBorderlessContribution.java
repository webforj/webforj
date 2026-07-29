package com.webforj.devtools.craftforj.inspector.contribution.appearance.tabbedpane;

import com.google.auto.service.AutoService;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for TabbedPane borderless mode.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TabbedPaneBorderlessContribution extends ConcernContribution<TabbedPane> {

  /**
   * Creates a new TabbedPaneBorderlessContribution.
   */
  public TabbedPaneBorderlessContribution() {
    super(TabbedPane.class, "Borderless", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(TabbedPane::isBorderless);
    setSetter((c, v) -> c.setBorderless(Boolean.TRUE.equals(v)));
  }

}
