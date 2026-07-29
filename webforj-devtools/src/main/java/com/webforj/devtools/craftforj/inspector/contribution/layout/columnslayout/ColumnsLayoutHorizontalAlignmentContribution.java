package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.component.layout.columnslayout.ColumnsLayout.Alignment;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for ColumnsLayout horizontal alignment property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class ColumnsLayoutHorizontalAlignmentContribution
    extends EnumConcernContribution<ColumnsLayout> {

  /** Creates the ColumnsLayout horizontal alignment contribution. */
  public ColumnsLayoutHorizontalAlignmentContribution() {
    super(ColumnsLayout.class, "HorizontalAlignment", FeatureCategory.LAYOUT);
    setGetter(ColumnsLayout::getHorizontalAlignment);
    setSetter((c, v) -> c.setHorizontalAlignment((Alignment) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return Alignment.class;
  }
}
