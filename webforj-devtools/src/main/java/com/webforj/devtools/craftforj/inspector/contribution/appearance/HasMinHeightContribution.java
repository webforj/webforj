package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasMinHeight;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.utilities.DimensionSetter;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasMinHeight concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasMinHeightContribution extends ConcernContribution<HasMinHeight<?>> {

  /**
   * Creates the HasMinHeight contribution.
   */
  public HasMinHeightContribution() {
    super(HasMinHeight.class, "MinHeight", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::size);
    setGetter(HasMinHeight::getMinHeight);
    setSetter((c, v) -> DimensionSetter.set(c, v, (comp, d) -> comp.setMinHeight((float) d),
        c::setMinHeight));
  }
}
