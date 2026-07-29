package com.webforj.devtools.craftforj.inspector.contribution.layout.tabbedpane;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for TabbedPane alignment.
 *
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TabbedPaneAlignmentContribution extends EnumConcernContribution<TabbedPane> {

  /**
   * Creates a new TabbedPaneAlignmentContribution.
   */
  public TabbedPaneAlignmentContribution() {
    super(TabbedPane.class, "Alignment", FeatureCategory.LAYOUT);
    setGetter(TabbedPane::getAlignment);
    setSetter((c, v) -> c.setAlignment((TabbedPane.Alignment) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return TabbedPane.Alignment.class;
  }

}
