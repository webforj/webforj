package com.webforj.automation.see.component.table;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.table.Column;
import com.webforj.component.table.Table;
import com.webforj.data.HasEntityKey;
import java.util.List;
import java.util.Locale;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * Serializes a table with its columns, its item type and its first rows with their rendered values
 * and keys, and the selected rows when the snapshot includes the browser state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class TableSnapshotSerializer implements SnapshotSerializer<Table<?>> {

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings("unchecked")
  public Class<Table<?>> getContract() {
    return (Class<Table<?>>) (Class<?>) Table.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(Table<?> table, SnapshotNode node) {
    describeTyped(table, node);
  }

  private static <T> void describeTyped(Table<T> table, SnapshotNode node) {
    if (table.getSelectionMode() == Table.SelectionMode.MULTIPLE) {
      node.addFlag("multiple");
    }

    List<Column<T, ?>> columns = table.getColumns();
    for (Column<T, ?> column : columns) {
      describeColumn(column, node);
    }

    try (Stream<T> rows = table.getRepository().findAll().limit(node.getMaxWidth() + 1L)) {
      List<T> items = rows.toList();
      if (items.isEmpty()) {
        return;
      }

      node.setAttribute("itemType", items.get(0).getClass().getSimpleName());
      List<T> selected = node.isBrowserState() ? table.getSelectedItems() : List.of();
      for (T item : items) {
        describeRow(table, columns, item, selected.contains(item), node);
      }
    }
  }

  private static <T> void describeColumn(Column<T, ?> column, SnapshotNode node) {
    SnapshotNode line = node.addChild("Column").setAttribute("id", column.getId());
    if (!column.getLabel().equalsIgnoreCase(column.getId())) {
      line.setAttribute("label", column.getLabel());
    }
    if (column.getSortDirection() != Column.SortDirection.NONE) {
      line.setAttribute("sort", column.getSortDirection().name().toLowerCase(Locale.ROOT));
    }
    if (column.isSortable()) {
      line.addFlag("sortable");
    }
    if (column.isHidden()) {
      line.addFlag("hidden");
    }
  }

  private static <T> void describeRow(Table<T> table, List<Column<T, ?>> columns, T item,
      boolean selected, SnapshotNode node) {
    SnapshotNode row = node.addItem("Row");
    if (item instanceof HasEntityKey || table.getRepository().getKey(item) != item) {
      row.setAttribute("key", table.getItemKeysRegistry().getKey(item));
    }

    for (Column<T, ?> column : columns) {
      Function<T, ?> provider = column.getValueProvider();
      if (provider != null) {
        row.setAttribute(column.getId(), provider.apply(item));
      }
    }

    if (selected) {
      row.addFlag("selected");
    }
  }
}
