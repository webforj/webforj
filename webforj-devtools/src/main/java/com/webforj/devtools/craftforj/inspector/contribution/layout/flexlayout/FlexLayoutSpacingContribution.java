package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for FlexLayout spacing (gap) property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class FlexLayoutSpacingContribution extends ConcernContribution<FlexLayout> {

  /** Creates the FlexLayout spacing contribution. */
  public FlexLayoutSpacingContribution() {
    super(FlexLayout.class, "Spacing", FeatureCategory.LAYOUT);
    setBuilderConfig(FeatureProperty.Builder::size);
    setGetter(FlexLayout::getSpacing);
    setSetter((c, v) -> c.setSpacing(v == null ? null : String.valueOf(v)));
  }
}
