package com.webforj.devtools.craftforj.inspector.contribution.layout.applayout;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.applayout.AppLayout;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for AppLayout footerOffscreen property.
 *
 * @author webforJ craftforJ
 * @since 26.02
 *
 */
@AutoService(FeatureHandler.class)
public class AppLayoutFooterOffscreenContribution extends ConcernContribution<AppLayout> {

  /**
   * Creates a new contribution.
   */
  public AppLayoutFooterOffscreenContribution() {
    super(AppLayout.class, "FooterOffscreen", FeatureCategory.LAYOUT);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(AppLayout::isFooterOffscreen);
    setSetter((c, v) -> c.setFooterOffscreen(Boolean.TRUE.equals(v)));
  }

}
