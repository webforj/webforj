package com.webforj.automation.see;

/**
 * Serializes a {@link Gauge} as an application serializer.
 */
public final class GaugeSerializer implements SnapshotSerializer<Gauge> {

  @Override
  public Class<Gauge> getContract() {
    return Gauge.class;
  }

  @Override
  public void describe(Gauge target, SnapshotNode node) {
    node.setAttribute("zone", target.getZone());
    node.setAttribute("level", target.getLevel());
    if (node.isBrowserState()) {
      node.setAttribute("needle", target.getNeedle());
    }

    node.addChild("Scale").setText(target.getUnit());
    for (String mark : target.getMarks()) {
      node.addItem("Mark").setText(mark);
    }

    if (target.getMarkCount() != null) {
      node.setItemCount(target.getMarkCount());
    }
  }
}
