package com.webforj.automation.see.component.login;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.login.Login;
import com.webforj.concern.HasEnablement;
import java.util.Set;

/**
 * Serializes a login with its opened state when the snapshot includes the browser state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class LoginSnapshotSerializer implements SnapshotSerializer<Login> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Login> getContract() {
    return Login.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(Login target, SnapshotNode node) {
    if (node.isBrowserState() && target.isOpened()) {
      node.addFlag("opened");
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getClientHeldContracts() {
    return Set.of(HasEnablement.class);
  }
}
