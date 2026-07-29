package com.webforj.devtools.craftforj.inspector.contribution.state.login;

import com.google.auto.service.AutoService;
import com.webforj.component.login.Login;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Login remember me property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class LoginRemembermeContribution extends ConcernContribution<Login> {

  /**
   * Creates a new LoginRemembermeContribution.
   */
  public LoginRemembermeContribution() {
    super(Login.class, "Rememberme", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Login::isRememberme);
    setSetter((c, v) -> c.setRememberme(Boolean.TRUE.equals(v)));
  }

}
