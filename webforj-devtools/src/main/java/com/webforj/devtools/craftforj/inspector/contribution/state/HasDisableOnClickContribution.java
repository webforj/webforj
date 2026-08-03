package com.webforj.devtools.craftforj.inspector.contribution.state;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasDisableOnClick;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasDisableOnClick concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasDisableOnClickContribution extends ConcernContribution<HasDisableOnClick<?>> {

  /**
   * Creates the HasDisableOnClick contribution.
   */
  public HasDisableOnClickContribution() {
    super(HasDisableOnClick.class, "DisableOnClick", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(HasDisableOnClick::isDisableOnClick);
    setSetter((c, v) -> c.setDisableOnClick(Boolean.TRUE.equals(v)));
  }
}
