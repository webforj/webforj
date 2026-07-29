package com.webforj.devtools.craftforj.inspector.contribution.appearance.applayout;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.applayout.AppLayout;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for AppLayout headerReveal property.
 *
 * @author webforJ craftforJ
 * @since 26.02
 *
 */
@AutoService(FeatureHandler.class)
public class AppLayoutHeaderRevealContribution extends ConcernContribution<AppLayout> {

  /**
   * Creates a new contribution.
   */
  public AppLayoutHeaderRevealContribution() {
    super(AppLayout.class, "HeaderReveal", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(AppLayout::isHeaderReveal);
    setSetter((c, v) -> c.setHeaderReveal(Boolean.TRUE.equals(v)));
  }

}
