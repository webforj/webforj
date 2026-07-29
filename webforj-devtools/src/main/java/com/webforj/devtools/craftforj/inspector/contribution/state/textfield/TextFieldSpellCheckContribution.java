package com.webforj.devtools.craftforj.inspector.contribution.state.textfield;

import com.google.auto.service.AutoService;
import com.webforj.component.field.DwcField;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the TextField spellCheck property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class TextFieldSpellCheckContribution extends ConcernContribution<DwcField<?, ?>> {

  /**
   * Creates the TextField spellCheck contribution.
   */
  public TextFieldSpellCheckContribution() {
    super(DwcField.class, "SpellCheck", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(DwcField::isSpellCheck);
    setSetter((c, v) -> c.setSpellCheck(Boolean.TRUE.equals(v)));
  }

}
