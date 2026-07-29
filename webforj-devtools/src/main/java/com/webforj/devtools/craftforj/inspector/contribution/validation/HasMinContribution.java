package com.webforj.devtools.craftforj.inspector.contribution.validation;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasMin;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.utilities.NumberConverter;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasMin concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasMinContribution extends ConcernContribution<HasMin<?, ?>> {

  /**
   * Creates the HasMin contribution.
   */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public HasMinContribution() {
    super(HasMin.class, "Min", FeatureCategory.VALIDATION);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(c -> {
      Object val = c.getMin();
      return val != null ? String.valueOf(val) : "";
    });
    setSetter((c, v) -> ((HasMin) c).setMin(NumberConverter.convert(v, c.getMin())));
  }
}
