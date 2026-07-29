package com.webforj.devtools.craftforj.inspector.contribution.state;

import com.google.auto.service.AutoService;
import com.webforj.concern.HasFileChooserMultiFilterSelection;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the HasFileChooserMultiFilterSelection concern.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class HasFileChooserMultiFilterSelectionContribution
    extends ConcernContribution<HasFileChooserMultiFilterSelection<?>> {

  /**
   * Creates the HasFileChooserMultiFilterSelection contribution.
   */
  public HasFileChooserMultiFilterSelectionContribution() {
    super(HasFileChooserMultiFilterSelection.class, "MultiFilterSelection", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(HasFileChooserMultiFilterSelection::isMultiFilterSelection);
    setSetter((c, v) -> c.setMultiFilterSelection(Boolean.TRUE.equals(v)));
  }
}
