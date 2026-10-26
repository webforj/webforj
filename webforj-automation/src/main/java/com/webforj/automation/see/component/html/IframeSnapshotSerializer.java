package com.webforj.automation.see.component.html;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.html.elements.Iframe;

/**
 * Serializes an inline frame with its source.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class IframeSnapshotSerializer implements SnapshotSerializer<Iframe> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Iframe> getContract() {
    return Iframe.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(Iframe target, SnapshotNode node) {
    node.setAttribute("src", target.getSrc());
  }
}
