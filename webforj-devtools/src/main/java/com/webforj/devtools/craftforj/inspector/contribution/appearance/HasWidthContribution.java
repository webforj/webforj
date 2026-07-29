package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasWidth;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.utilities.DimensionSetter;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasWidth concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasWidthContribution extends ConcernContribution<HasWidth<?>> {

  /**
   * Creates the HasWidth contribution.
   */
  public HasWidthContribution() {
    super(HasWidth.class, "Width", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::size);
    setGetter(HasWidth::getWidth);
    setSetter(
        (c, v) -> DimensionSetter.set(c, v, (comp, d) -> comp.setWidth((float) d), c::setWidth));
  }
}
