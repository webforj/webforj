package com.webforj.automation.see.component.html;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.html.elements.Img;

/**
 * Serializes an image with its source and its alternative text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class ImgSnapshotSerializer implements SnapshotSerializer<Img> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Img> getContract() {
    return Img.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(Img target, SnapshotNode node) {
    node.setAttribute("src", target.getSrc()).setAttribute("alt", target.getAlt());
  }
}
