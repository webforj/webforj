package com.webforj.devtools.craftforj.inspector.contribution.state.tabbedpane;

import com.google.auto.service.AutoService;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for TabbedPane swipe with mouse mode.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TabbedPaneSwipeWithMouseContribution extends ConcernContribution<TabbedPane> {

  /**
   * Creates a new TabbedPaneSwipeWithMouseContribution.
   */
  public TabbedPaneSwipeWithMouseContribution() {
    super(TabbedPane.class, "SwipeWithMouse", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(TabbedPane::isSwipeWithMouse);
    setSetter((c, v) -> c.setSwipeWithMouse(Boolean.TRUE.equals(v)));
  }

}
