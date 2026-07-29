package com.webforj.devtools.craftforj.inspector.contribution.state.accordion;

import com.google.auto.service.AutoService;
import com.webforj.component.accordion.Accordion;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Accordion multiple open panels mode.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class AccordionMultipleContribution extends ConcernContribution<Accordion> {

  /**
   * Creates a new AccordionMultipleContribution.
   */
  public AccordionMultipleContribution() {
    super(Accordion.class, "Multiple", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Accordion::isMultiple);
    setSetter((c, v) -> c.setMultiple(Boolean.TRUE.equals(v)));
  }
}
