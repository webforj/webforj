package com.webforj.automation.see.component.optioninput;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.optioninput.DwcOptionInput;
import com.webforj.concern.HasText;
import java.util.Set;

/**
 * Serializes a check box or a radio button with its text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class OptionInputSnapshotSerializer implements SnapshotSerializer<DwcOptionInput<?>> {

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings("unchecked")
  public Class<DwcOptionInput<?>> getContract() {
    return (Class<DwcOptionInput<?>>) (Class<?>) DwcOptionInput.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getServerHeldContracts() {
    return Set.of(HasText.class);
  }
}
