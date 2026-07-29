package com.webforj.devtools.craftforj.inspector.contribution.content;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasHelperText;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasHelperText concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasHelperTextContribution extends ConcernContribution<HasHelperText<?>> {

  /**
   * Creates the HasHelperText contribution.
   */
  public HasHelperTextContribution() {
    super(HasHelperText.class, "HelperText", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(HasHelperText::getHelperText);
    setSetter((c, v) -> c.setHelperText(String.valueOf(v)));
  }
}
