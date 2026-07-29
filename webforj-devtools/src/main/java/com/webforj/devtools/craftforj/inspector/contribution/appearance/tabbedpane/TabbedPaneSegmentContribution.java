package com.webforj.devtools.craftforj.inspector.contribution.appearance.tabbedpane;

import com.google.auto.service.AutoService;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for TabbedPane segment style.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TabbedPaneSegmentContribution extends ConcernContribution<TabbedPane> {

  /**
   * Creates a new TabbedPaneSegmentContribution.
   */
  public TabbedPaneSegmentContribution() {
    super(TabbedPane.class, "Segment", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(TabbedPane::isSegment);
    setSetter((c, v) -> c.setSegment(Boolean.TRUE.equals(v)));
  }
}
