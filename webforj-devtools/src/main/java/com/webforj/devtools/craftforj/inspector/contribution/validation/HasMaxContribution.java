package com.webforj.devtools.craftforj.inspector.contribution.validation;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasMax;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.utilities.NumberConverter;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasMax concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasMaxContribution extends ConcernContribution<HasMax<?, ?>> {

  /**
   * Creates the HasMax contribution.
   */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public HasMaxContribution() {
    super(HasMax.class, "Max", FeatureCategory.VALIDATION);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(c -> {
      Object val = c.getMax();
      return val != null ? String.valueOf(val) : "";
    });
    setSetter((c, v) -> ((HasMax) c).setMax(NumberConverter.convert(v, c.getMax())));
  }
}
