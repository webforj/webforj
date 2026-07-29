package com.webforj.devtools.craftforj.inspector.contribution.content;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasPlaceholder;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasPlaceholder concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasPlaceholderContribution extends ConcernContribution<HasPlaceholder<?>> {

  /**
   * Creates the HasPlaceholder contribution.
   */
  public HasPlaceholderContribution() {
    super(HasPlaceholder.class, "Placeholder", FeatureCategory.CONTENT);
    setBuilderConfig(FeatureProperty.Builder::text);
    setGetter(HasPlaceholder::getPlaceholder);
    setSetter((c, v) -> c.setPlaceholder(String.valueOf(v)));
  }
}
