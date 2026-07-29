package com.webforj.devtools.craftforj.inspector.contribution.appearance.passwordfield;

import com.google.auto.service.AutoService;
import com.webforj.component.field.PasswordField;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the PasswordField passwordReveal property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class PasswordFieldPasswordRevealContribution extends ConcernContribution<PasswordField> {

  /**
   * Creates the PasswordField passwordReveal contribution.
   */
  public PasswordFieldPasswordRevealContribution() {
    super(PasswordField.class, "PasswordReveal", FeatureCategory.APPEARANCE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(PasswordField::isPasswordReveal);
    setSetter((c, v) -> c.setPasswordReveal(Boolean.TRUE.equals(v)));
  }

}
