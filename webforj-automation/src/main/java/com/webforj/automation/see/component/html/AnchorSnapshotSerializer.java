package com.webforj.automation.see.component.html;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.html.elements.Anchor;

/**
 * Serializes an anchor with its link target.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class AnchorSnapshotSerializer implements SnapshotSerializer<Anchor> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Anchor> getContract() {
    return Anchor.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(Anchor target, SnapshotNode node) {
    node.setAttribute("href", target.getHref());
  }
}
