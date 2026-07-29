package com.webforj.devtools.craftforj.inspector.contribution.state.tabbedpane;

import com.google.auto.service.AutoService;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for TabbedPane swipeable mode.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TabbedPaneSwipeableContribution extends ConcernContribution<TabbedPane> {

  /**
   * Creates a new TabbedPaneSwipeableContribution.
   */
  public TabbedPaneSwipeableContribution() {
    super(TabbedPane.class, "Swipeable", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(TabbedPane::isSwipeable);
    setSetter((c, v) -> c.setSwipeable(Boolean.TRUE.equals(v)));
  }

}
