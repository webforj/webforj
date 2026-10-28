package com.webforj.automation.see.component.accordion;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.accordion.AccordionPanel;
import com.webforj.concern.HasText;
import java.util.Set;

/**
 * Serializes an accordion panel with its text, and its expanded state when the snapshot includes
 * the browser state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class AccordionPanelSnapshotSerializer implements SnapshotSerializer<AccordionPanel> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<AccordionPanel> getContract() {
    return AccordionPanel.class;
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
  public void describe(AccordionPanel target, SnapshotNode node) {
    if (node.isBrowserState() && target.isOpened()) {
      node.addFlag("expanded");
    }
  }
}
