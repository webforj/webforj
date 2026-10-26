package com.webforj.automation.see.component.optioninput;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.optioninput.CheckBox;

/**
 * Serializes a check box with its indeterminate state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class CheckBoxSnapshotSerializer implements SnapshotSerializer<CheckBox> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<CheckBox> getContract() {
    return CheckBox.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(CheckBox target, SnapshotNode node) {
    if (target.isIndeterminate()) {
      node.addFlag("indeterminate");
    }
  }
}
