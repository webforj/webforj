package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasMaxWidth;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.utilities.DimensionSetter;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasMaxWidth concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasMaxWidthContribution extends ConcernContribution<HasMaxWidth<?>> {

  /**
   * Creates the HasMaxWidth contribution.
   */
  public HasMaxWidthContribution() {
    super(HasMaxWidth.class, "MaxWidth", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::size);
    setGetter(HasMaxWidth::getMaxWidth);
    setSetter((c, v) -> DimensionSetter.set(c, v, (comp, d) -> comp.setMaxWidth((float) d),
        c::setMaxWidth));
  }
}
