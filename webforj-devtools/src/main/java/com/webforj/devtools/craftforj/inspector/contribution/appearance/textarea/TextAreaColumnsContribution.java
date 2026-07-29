package com.webforj.devtools.craftforj.inspector.contribution.appearance.textarea;

import com.google.auto.service.AutoService;
import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the TextArea columns property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TextAreaColumnsContribution extends ConcernContribution<TextArea> {

  /**
   * Creates the TextArea columns contribution.
   */
  public TextAreaColumnsContribution() {
    super(TextArea.class, "Columns", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(TextArea::getColumns);
    setSetter((c, v) -> c.setColumns(v instanceof Number n ? n.intValue() : 0));
  }

}
