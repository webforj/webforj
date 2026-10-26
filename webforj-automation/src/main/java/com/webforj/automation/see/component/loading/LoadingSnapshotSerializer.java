package com.webforj.automation.see.component.loading;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.loading.Loading;
import com.webforj.concern.HasText;
import java.util.Set;

/**
 * Serializes a loading indicator with its text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class LoadingSnapshotSerializer implements SnapshotSerializer<Loading> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Loading> getContract() {
    return Loading.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getServerHeldContracts() {
    return Set.of(HasText.class);
  }
}
