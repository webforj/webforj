package com.webforj.devtools.craftforj.inspector.contribution.appearance.tabbedpane;

import com.google.auto.service.AutoService;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for TabbedPane body hidden mode.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TabbedPaneBodyHiddenContribution extends ConcernContribution<TabbedPane> {

  /**
   * Creates a new TabbedPaneBodyHiddenContribution.
   */
  public TabbedPaneBodyHiddenContribution() {
    super(TabbedPane.class, "BodyHidden", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(TabbedPane::isBodyHidden);
    setSetter((c, v) -> c.setBodyHidden(Boolean.TRUE.equals(v)));
  }

}
