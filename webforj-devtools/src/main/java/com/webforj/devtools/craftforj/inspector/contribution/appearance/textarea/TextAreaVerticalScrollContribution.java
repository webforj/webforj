package com.webforj.devtools.craftforj.inspector.contribution.appearance.textarea;

import com.google.auto.service.AutoService;
import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the TextArea verticalScroll property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TextAreaVerticalScrollContribution extends ConcernContribution<TextArea> {

  /**
   * Creates the TextArea verticalScroll contribution.
   */
  public TextAreaVerticalScrollContribution() {
    super(TextArea.class, "VerticalScroll", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(TextArea::isVerticalScroll);
    setSetter((c, v) -> c.setVerticalScroll(Boolean.TRUE.equals(v)));
  }

}
