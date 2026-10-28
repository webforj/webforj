package com.webforj.automation.see.component.dialog;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.dialog.Dialog;

/**
 * Serializes a dialog with its opened state when the snapshot includes the browser state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class DialogSnapshotSerializer implements SnapshotSerializer<Dialog> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Dialog> getContract() {
    return Dialog.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(Dialog target, SnapshotNode node) {
    if (node.isBrowserState() && target.isOpened()) {
      node.addFlag("opened");
    }
  }
}
