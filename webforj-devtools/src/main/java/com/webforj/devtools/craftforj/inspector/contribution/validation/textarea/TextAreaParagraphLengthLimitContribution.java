package com.webforj.devtools.craftforj.inspector.contribution.validation.textarea;

import com.google.auto.service.AutoService;
import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the TextArea paragraphLengthLimit property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TextAreaParagraphLengthLimitContribution extends ConcernContribution<TextArea> {

  /**
   * Creates the TextArea paragraphLengthLimit contribution.
   */
  public TextAreaParagraphLengthLimitContribution() {
    super(TextArea.class, "ParagraphLengthLimit", FeatureCategory.VALIDATION);
    setBuilderConfig(FeatureProperty.Builder::integer);
    setGetter(TextArea::getParagraphLengthLimit);
    setSetter((c, v) -> c.setParagraphLengthLimit(v instanceof Number n ? n.intValue() : 0));
  }

}
