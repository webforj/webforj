package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout;

import com.google.auto.service.AutoService;
import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for ColumnsLayout horizontal spacing property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ColumnsLayoutHorizontalSpacingContribution extends ConcernContribution<ColumnsLayout> {

  /** Creates the ColumnsLayout horizontal spacing contribution. */
  public ColumnsLayoutHorizontalSpacingContribution() {
    super(ColumnsLayout.class, "HorizontalSpacing", FeatureCategory.LAYOUT);
    setBuilderConfig(FeatureProperty.Builder::size);
    setGetter(ColumnsLayout::getHorizontalSpacing);
    setSetter((c, v) -> c.setHorizontalSpacing(v == null ? null : String.valueOf(v)));
  }
}
