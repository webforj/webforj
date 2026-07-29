package com.webforj.devtools.craftforj.inspector.contribution.state.login;

import com.google.auto.service.AutoService;
import com.webforj.component.login.Login;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Login error property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class LoginErrorContribution extends ConcernContribution<Login> {

  /**
   * Creates a new LoginErrorContribution.
   */
  public LoginErrorContribution() {
    super(Login.class, "Error", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Login::isError);
    setSetter((c, v) -> c.setError(Boolean.TRUE.equals(v)));
  }

}
