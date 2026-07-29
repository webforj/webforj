package com.webforj.devtools.craftforj.inspector.contribution.appearance;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasFileChooserFiltersVisible;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasFileChooserFiltersVisible concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasFileChooserFiltersVisibleContribution
    extends ConcernContribution<HasFileChooserFiltersVisible<?>> {

  /**
   * Creates the HasFileChooserFiltersVisible contribution.
   */
  public HasFileChooserFiltersVisibleContribution() {
    super(HasFileChooserFiltersVisible.class, "FiltersVisible", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(HasFileChooserFiltersVisible::isFiltersVisible);
    setSetter((c, v) -> c.setFiltersVisible(Boolean.TRUE.equals(v)));
  }
}
