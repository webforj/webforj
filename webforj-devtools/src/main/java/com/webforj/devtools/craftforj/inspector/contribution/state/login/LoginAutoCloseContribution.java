package com.webforj.devtools.craftforj.inspector.contribution.state.login;

import com.google.auto.service.AutoService;
import com.webforj.component.login.Login;
import com.webforj.devtools.craftforj.inspector.contribution.ConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for Login auto close property.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class LoginAutoCloseContribution extends ConcernContribution<Login> {

  /**
   * Creates a new LoginAutoCloseContribution.
   */
  public LoginAutoCloseContribution() {
    super(Login.class, "AutoClose", FeatureCategory.STATE);
    setBuilderConfig(FeatureProperty.Builder::bool);
    setGetter(Login::isAutoClose);
    setSetter((c, v) -> c.setAutoClose(Boolean.TRUE.equals(v)));
  }

}
