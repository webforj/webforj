package com.webforj.devtools.craftforj.inspector.contribution.content.textfield;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.component.field.TextField;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;

/**
 * Contribution for the TextField type property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TextFieldTypeContribution extends EnumConcernContribution<TextField> {

  /**
   * Creates the TextField type contribution.
   */
  public TextFieldTypeContribution() {
    super(TextField.class, "Type", FeatureCategory.CONTENT);
    setGetter(TextField::getType);
    setSetter((c, v) -> c.setType((TextField.Type) v));
  }

  @Override
  public Class<?> findEnumClass(Component component) {
    return TextField.Type.class;
  }

}
