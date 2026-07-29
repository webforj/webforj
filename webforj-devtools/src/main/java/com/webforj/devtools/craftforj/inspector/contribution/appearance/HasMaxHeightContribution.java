package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasMaxHeight;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.utilities.DimensionSetter;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasMaxHeight concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasMaxHeightContribution extends ConcernContribution<HasMaxHeight<?>> {

  /**
   * Creates the HasMaxHeight contribution.
   */
  public HasMaxHeightContribution() {
    super(HasMaxHeight.class, "MaxHeight", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::size);
    setGetter(HasMaxHeight::getMaxHeight);
    setSetter((c, v) -> DimensionSetter.set(c, v, (comp, d) -> comp.setMaxHeight((float) d),
        c::setMaxHeight));
  }
}
