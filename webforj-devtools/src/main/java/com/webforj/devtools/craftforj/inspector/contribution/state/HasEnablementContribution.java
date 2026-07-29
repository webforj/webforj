package com.webforj.devtools.craftforj.inspector.contribution.state;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasEnablement;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasEnablement concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasEnablementContribution extends ConcernContribution<HasEnablement<?>> {

  /**
   * Creates the HasEnablement contribution.
   */
  public HasEnablementContribution() {
    super(HasEnablement.class, "Enabled", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(HasEnablement::isEnabled);
    setSetter((c, v) -> c.setEnabled(Boolean.TRUE.equals(v)));
  }
}
