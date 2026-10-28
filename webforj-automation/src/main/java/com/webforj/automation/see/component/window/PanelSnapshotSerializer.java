package com.webforj.automation.see.component.window;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.window.Panel;
import com.webforj.concern.HasText;
import java.util.Set;

/**
 * Serializes a panel with its text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class PanelSnapshotSerializer implements SnapshotSerializer<Panel> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Panel> getContract() {
    return Panel.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getServerHeldContracts() {
    return Set.of(HasText.class);
  }
}
