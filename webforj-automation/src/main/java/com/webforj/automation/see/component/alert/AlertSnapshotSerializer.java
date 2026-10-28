package com.webforj.automation.see.component.alert;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.alert.Alert;
import com.webforj.concern.HasText;
import com.webforj.concern.HasVisibility;
import java.util.Set;

/**
 * Serializes an alert with its text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class AlertSnapshotSerializer implements SnapshotSerializer<Alert> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Alert> getContract() {
    return Alert.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getServerHeldContracts() {
    return Set.of(HasText.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getClientHeldContracts() {
    return Set.of(HasVisibility.class);
  }
}
