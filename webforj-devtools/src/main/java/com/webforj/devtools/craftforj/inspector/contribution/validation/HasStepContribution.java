package com.webforj.devtools.craftforj.inspector.contribution.validation;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasStep;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.utilities.NumberConverter;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasStep concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasStepContribution extends ConcernContribution<HasStep<?, ?>> {

  /**
   * Creates the HasStep contribution.
   */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public HasStepContribution() {
    super(HasStep.class, "Step", FeatureCategory.VALIDATION);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(c -> {
      Object val = c.getStep();
      return val != null ? String.valueOf(val) : "";
    });
    setSetter((c, v) -> ((HasStep) c).setStep(NumberConverter.convert(v, c.getStep())));
  }
}
