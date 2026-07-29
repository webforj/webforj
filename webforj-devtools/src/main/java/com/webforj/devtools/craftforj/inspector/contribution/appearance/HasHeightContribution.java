package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasHeight;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.utilities.DimensionSetter;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasHeight concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasHeightContribution extends ConcernContribution<HasHeight<?>> {

  /**
   * Creates the HasHeight contribution.
   */
  public HasHeightContribution() {
    super(HasHeight.class, "Height", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::size);
    setGetter(HasHeight::getHeight);
    setSetter(
        (c, v) -> DimensionSetter.set(c, v, (comp, d) -> comp.setHeight((float) d), c::setHeight));
  }
}
