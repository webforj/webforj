package com.webforj.devtools.craftforj.inspector.contribution.appearance.textarea;

import com.google.auto.service.AutoService;
import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the TextArea lineWrap property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TextAreaLineWrapContribution extends ConcernContribution<TextArea> {

  /**
   * Creates the TextArea lineWrap contribution.
   */
  public TextAreaLineWrapContribution() {
    super(TextArea.class, "LineWrap", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(TextArea::isLineWrap);
    setSetter((c, v) -> c.setLineWrap(Boolean.TRUE.equals(v)));
  }

}
