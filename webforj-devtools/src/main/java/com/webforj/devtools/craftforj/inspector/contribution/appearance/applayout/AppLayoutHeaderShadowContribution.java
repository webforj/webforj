package com.webforj.devtools.craftforj.inspector.contribution.appearance.applayout;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.layout.applayout.AppLayout;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for AppLayout headerShadow property.
 *
 * @author webforJ craftforJ
 * @since 26.02
 *
 */
@AutoService(FeatureHandler.class)
public class AppLayoutHeaderShadowContribution extends EnumConcernContribution<AppLayout> {

  /**
   * Creates a new contribution.
   */
  public AppLayoutHeaderShadowContribution() {
    super(AppLayout.class, "HeaderShadow", FeatureCategory.APPEARANCE);
    setGetter(AppLayout::getHeaderShadow);
    setSetter((c, v) -> c.setHeaderShadow((AppLayout.Shadow) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return AppLayout.Shadow.class;
  }

}
