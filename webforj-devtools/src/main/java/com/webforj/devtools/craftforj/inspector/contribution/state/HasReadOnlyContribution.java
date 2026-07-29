package com.webforj.devtools.craftforj.inspector.contribution.state;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasReadOnly;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasReadOnly concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasReadOnlyContribution extends ConcernContribution<HasReadOnly<?>> {

  /**
   * Creates the HasReadOnly contribution.
   */
  public HasReadOnlyContribution() {
    super(HasReadOnly.class, "ReadOnly", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(HasReadOnly::isReadOnly);
    setSetter((c, v) -> c.setReadOnly(Boolean.TRUE.equals(v)));
  }
}
