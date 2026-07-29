package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasMinWidth;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.utilities.DimensionSetter;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasMinWidth concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasMinWidthContribution extends ConcernContribution<HasMinWidth<?>> {

  /**
   * Creates the HasMinWidth contribution.
   */
  public HasMinWidthContribution() {
    super(HasMinWidth.class, "MinWidth", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::size);
    setGetter(HasMinWidth::getMinWidth);
    setSetter((c, v) -> DimensionSetter.set(c, v, (comp, d) -> comp.setMinWidth((float) d),
        c::setMinWidth));
  }
}
