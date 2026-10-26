package com.webforj.automation.see.component.upload;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.upload.Upload;

/**
 * Serializes an upload with its multiple selection state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class UploadSnapshotSerializer implements SnapshotSerializer<Upload> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Upload> getContract() {
    return Upload.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(Upload target, SnapshotNode node) {
    if (target.getSelectionMode() == Upload.SelectionMode.MULTIPLE) {
      node.addFlag("multiple");
    }
  }
}
