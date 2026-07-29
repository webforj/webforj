package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.layout.flexlayout.FlexJustifyContent;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for FlexLayout justify-content property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class FlexLayoutJustifyContentContribution extends EnumConcernContribution<FlexLayout> {

  /** Creates the FlexLayout justify-content contribution. */
  public FlexLayoutJustifyContentContribution() {
    super(FlexLayout.class, "JustifyContent", FeatureCategory.LAYOUT);
    setGetter(flex -> {
      try {
        return flex.getJustifyContent();
      } catch (IllegalArgumentException e) {
        return FlexJustifyContent.getDefault();
      }
    });
    setSetter((c, v) -> c.setJustifyContent((FlexJustifyContent) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return FlexJustifyContent.class;
  }
}
