package com.webforj.devtools.craftforj.inspector.contribution.state;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasVisibility;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasVisibility concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasVisibilityContribution extends ConcernContribution<HasVisibility<?>> {

  /**
   * Creates the HasVisibility contribution.
   */
  public HasVisibilityContribution() {
    super(HasVisibility.class, "Visible", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(HasVisibility::isVisible);
    setSetter((c, v) -> c.setVisible(Boolean.TRUE.equals(v)));
  }
}
