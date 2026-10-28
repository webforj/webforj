package com.webforj.automation.see.component.text;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.text.Label;
import com.webforj.concern.HasText;
import java.util.Set;

/**
 * Serializes a label with its text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class LabelSnapshotSerializer implements SnapshotSerializer<Label> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Label> getContract() {
    return Label.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getServerHeldContracts() {
    return Set.of(HasText.class);
  }
}
