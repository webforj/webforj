package com.webforj.devtools.craftforj.inspector.contribution.appearance.textarea;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.field.TextArea;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for the TextArea wrapStyle property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TextAreaWrapStyleContribution extends EnumConcernContribution<TextArea> {

  /**
   * Creates the TextArea wrapStyle contribution.
   */
  public TextAreaWrapStyleContribution() {
    super(TextArea.class, "WrapStyle", FeatureCategory.APPEARANCE);
    setGetter(TextArea::getWrapStyle);
    setSetter((c, v) -> c.setWrapStyle((TextArea.WrapStyle) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return TextArea.WrapStyle.class;
  }

}
