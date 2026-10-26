package com.webforj.automation.see.component.drawer;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.drawer.Drawer;

/**
 * Serializes a drawer with its label, and its opened state when the snapshot includes the browser
 * state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class DrawerSnapshotSerializer implements SnapshotSerializer<Drawer> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Drawer> getContract() {
    return Drawer.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(Drawer target, SnapshotNode node) {
    node.setAttribute("label", target.getLabel());
    if (node.isBrowserState() && target.isOpened()) {
      node.addFlag("opened");
    }
  }
}
