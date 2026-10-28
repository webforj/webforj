package com.webforj.automation.see.component.toast;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.toast.Toast;
import com.webforj.concern.HasText;
import java.util.Set;

/**
 * Serializes a toast with its text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class ToastSnapshotSerializer implements SnapshotSerializer<Toast> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Toast> getContract() {
    return Toast.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getServerHeldContracts() {
    return Set.of(HasText.class);
  }
}
