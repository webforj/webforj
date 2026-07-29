package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasExpanse;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for the HasExpanse concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasExpanseContribution extends EnumConcernContribution<HasExpanse<?, ?>> {

  /**
   * Creates the HasExpanse contribution.
   */
  @SuppressWarnings("rawtypes")
  public HasExpanseContribution() {
    super(HasExpanse.class, "Expanse", FeatureCategory.APPEARANCE);
    setGetter(HasExpanse::getExpanse);
    setSetter((c, v) -> ((HasExpanse) c).setExpanse(v));
  }
}
