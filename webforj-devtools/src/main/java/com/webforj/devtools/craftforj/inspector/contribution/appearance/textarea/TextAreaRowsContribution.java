package com.webforj.devtools.craftforj.inspector.contribution.appearance.textarea;

import com.google.auto.service.AutoService;
import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the TextArea rows property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TextAreaRowsContribution extends ConcernContribution<TextArea> {

  /**
   * Creates the TextArea rows contribution.
   */
  public TextAreaRowsContribution() {
    super(TextArea.class, "Rows", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(TextArea::getRows);
    setSetter((c, v) -> c.setRows(v instanceof Number n ? n.intValue() : 0));
  }

}
