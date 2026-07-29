package com.webforj.devtools.craftforj.inspector.contribution.layout.applayout;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.applayout.AppLayout;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for AppLayout headerOffscreen property.
 *
 * @author webforJ craftforJ
 * @since 26.02
 *
 */
@AutoService(FeatureHandler.class)
public class AppLayoutHeaderOffscreenContribution extends ConcernContribution<AppLayout> {

  /**
   * Creates a new contribution.
   */
  public AppLayoutHeaderOffscreenContribution() {
    super(AppLayout.class, "HeaderOffscreen", FeatureCategory.LAYOUT);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(AppLayout::isHeaderOffscreen);
    setSetter((c, v) -> c.setHeaderOffscreen(Boolean.TRUE.equals(v)));
  }

}
