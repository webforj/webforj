package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.layout.flexlayout.FlexContentAlignment;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for FlexLayout align-content property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class FlexLayoutAlignContentContribution extends EnumConcernContribution<FlexLayout> {

  /** Creates the FlexLayout align-content contribution. */
  public FlexLayoutAlignContentContribution() {
    super(FlexLayout.class, "AlignContent", FeatureCategory.LAYOUT);
    setGetter(flex -> {
      try {
        return flex.getAlignContent();
      } catch (IllegalArgumentException e) {
        return FlexContentAlignment.getDefault();
      }
    });
    setSetter((c, v) -> c.setAlignContent((FlexContentAlignment) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return FlexContentAlignment.class;
  }
}
