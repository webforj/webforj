package com.webforj.devtools.craftforj.inspector.contribution.validation.login;

import com.google.auto.service.AutoService;
import com.webforj.component.login.Login;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Login empty password property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class LoginEmptyPasswordContribution extends ConcernContribution<Login> {

  /**
   * Creates a new LoginEmptyPasswordContribution.
   */
  public LoginEmptyPasswordContribution() {
    super(Login.class, "EmptyPassword", FeatureCategory.VALIDATION);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Login::isEmptyPassword);
    setSetter((c, v) -> c.setEmptyPassword(Boolean.TRUE.equals(v)));
  }

}
