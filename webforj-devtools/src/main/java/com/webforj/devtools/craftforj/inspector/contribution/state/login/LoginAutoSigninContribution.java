package com.webforj.devtools.craftforj.inspector.contribution.state.login;

import com.google.auto.service.AutoService;
import com.webforj.component.login.Login;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Login auto signin property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class LoginAutoSigninContribution extends ConcernContribution<Login> {

  /**
   * Creates a new LoginAutoSigninContribution.
   */
  public LoginAutoSigninContribution() {
    super(Login.class, "AutoSignin", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Login::isAutoSignin);
    setSetter((c, v) -> c.setAutoSignin(Boolean.TRUE.equals(v)));
  }

}
