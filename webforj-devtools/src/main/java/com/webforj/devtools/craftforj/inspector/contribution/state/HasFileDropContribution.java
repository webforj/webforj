package com.webforj.devtools.craftforj.inspector.contribution.state;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasFileDrop;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasFileDrop concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasFileDropContribution extends ConcernContribution<HasFileDrop<?>> {

  /**
   * Creates the HasFileDrop contribution.
   */
  public HasFileDropContribution() {
    super(HasFileDrop.class, "Drop", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(HasFileDrop::isDrop);
    setSetter((c, v) -> c.setDrop(Boolean.TRUE.equals(v)));
  }
}
