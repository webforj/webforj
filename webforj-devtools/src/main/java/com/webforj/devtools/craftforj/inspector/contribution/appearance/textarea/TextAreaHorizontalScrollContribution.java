package com.webforj.devtools.craftforj.inspector.contribution.appearance.textarea;

import com.google.auto.service.AutoService;
import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the TextArea horizontalScroll property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TextAreaHorizontalScrollContribution extends ConcernContribution<TextArea> {

  /**
   * Creates the TextArea horizontalScroll contribution.
   */
  public TextAreaHorizontalScrollContribution() {
    super(TextArea.class, "HorizontalScroll", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(TextArea::isHorizontalScroll);
    setSetter((c, v) -> c.setHorizontalScroll(Boolean.TRUE.equals(v)));
  }

}
