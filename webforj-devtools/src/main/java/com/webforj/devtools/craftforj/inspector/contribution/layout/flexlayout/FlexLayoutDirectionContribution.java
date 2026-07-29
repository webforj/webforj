package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.layout.flexlayout.FlexDirection;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for FlexLayout direction property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class FlexLayoutDirectionContribution extends EnumConcernContribution<FlexLayout> {

  /** Creates the FlexLayout direction contribution. */
  public FlexLayoutDirectionContribution() {
    super(FlexLayout.class, "Direction", FeatureCategory.LAYOUT);
    setGetter(flex -> {
      try {
        return flex.getDirection();
      } catch (IllegalArgumentException e) {
        return FlexDirection.getDefault();
      }
    });
    setSetter((c, v) -> c.setDirection((FlexDirection) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return FlexDirection.class;
  }
}
