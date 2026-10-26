package com.webforj.automation.see.component.googlecharts;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.googlecharts.GoogleChart;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Serializes a chart with its type, its title and the rows of its data.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class GoogleChartSnapshotSerializer implements SnapshotSerializer<GoogleChart> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<GoogleChart> getContract() {
    return GoogleChart.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(GoogleChart target, SnapshotNode node) {
    GoogleChart.Type type = target.getType();
    if (type != null) {
      node.setAttribute("type", type.name().toLowerCase(Locale.ROOT).replace('_', '-'));
    }

    Map<String, Object> options = target.getOptions();
    if (options != null && options.get("title") instanceof String title) {
      node.setText(title);
    }

    describeData(target.getData(), node);
  }

  private static void describeData(List<Object> data, SnapshotNode node) {
    if (data == null || data.isEmpty() || !(data.get(0) instanceof List<?> columns)) {
      return;
    }

    for (Object entry : data.subList(1, data.size())) {
      if (entry instanceof List<?> values) {
        SnapshotNode row = node.addItem("Row");
        for (int index = 0; index < Math.min(columns.size(), values.size()); index++) {
          row.setAttribute(resolveColumnName(columns.get(index), index), values.get(index));
        }
      }
    }
  }

  private static String resolveColumnName(Object column, int index) {
    if (!(column instanceof Map<?, ?> declaration)) {
      return String.valueOf(column);
    }

    if (declaration.get("label") instanceof String label) {
      return label;
    }

    if (declaration.get("id") instanceof String id) {
      return id;
    }

    return String.valueOf(index);
  }
}
