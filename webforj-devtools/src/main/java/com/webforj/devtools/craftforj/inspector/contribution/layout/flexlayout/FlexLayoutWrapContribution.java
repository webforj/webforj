package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.layout.flexlayout.FlexWrap;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for FlexLayout wrap property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class FlexLayoutWrapContribution extends EnumConcernContribution<FlexLayout> {

  /** Creates the FlexLayout wrap contribution. */
  public FlexLayoutWrapContribution() {
    super(FlexLayout.class, "Wrap", FeatureCategory.LAYOUT);
    setGetter(flex -> {
      try {
        return flex.getWrap();
      } catch (IllegalArgumentException e) {
        return FlexWrap.getDefault();
      }
    });
    setSetter((c, v) -> c.setWrap((FlexWrap) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return FlexWrap.class;
  }
}
