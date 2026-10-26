package com.webforj.automation.see.component.progressbar;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.progressbar.ProgressBar;

/**
 * Serializes a progress bar with its indeterminate state and its vertical orientation.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class ProgressBarSnapshotSerializer implements SnapshotSerializer<ProgressBar> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<ProgressBar> getContract() {
    return ProgressBar.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(ProgressBar target, SnapshotNode node) {
    if (target.isIndeterminate()) {
      node.addFlag("indeterminate");
    }

    if (target.getOrientation() == ProgressBar.Orientation.VERTICAL) {
      node.addFlag("vertical");
    }
  }
}
