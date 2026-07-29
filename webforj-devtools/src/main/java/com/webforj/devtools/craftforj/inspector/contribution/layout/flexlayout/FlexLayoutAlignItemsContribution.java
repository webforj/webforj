package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.layout.flexlayout.FlexAlignment;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for FlexLayout align-items property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class FlexLayoutAlignItemsContribution extends EnumConcernContribution<FlexLayout> {

  /** Creates the FlexLayout align-items contribution. */
  public FlexLayoutAlignItemsContribution() {
    super(FlexLayout.class, "Alignment", FeatureCategory.LAYOUT);
    setGetter(flex -> {
      try {
        return flex.getAlignment();
      } catch (IllegalArgumentException e) {
        return FlexAlignment.getDefault();
      }
    });
    setSetter((c, v) -> c.setAlignment((FlexAlignment) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return FlexAlignment.class;
  }
}
