package com.webforj.devtools.craftforj.inspector.contribution.validation.textarea;

import com.google.auto.service.AutoService;
import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the TextArea lineCountLimit property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TextAreaLineCountLimitContribution extends ConcernContribution<TextArea> {

  /**
   * Creates the TextArea lineCountLimit contribution.
   */
  public TextAreaLineCountLimitContribution() {
    super(TextArea.class, "LineCountLimit", FeatureCategory.VALIDATION);
    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(TextArea::getLineCountLimit);
    setSetter((c, v) -> c.setLineCountLimit(v instanceof Number n ? n.intValue() : 0));
  }

}
