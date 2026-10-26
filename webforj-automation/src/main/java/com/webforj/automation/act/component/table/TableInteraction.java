package com.webforj.automation.act.component.table;

import com.google.auto.service.AutoService;
import com.google.gson.Gson;
import com.webforj.automation.act.AbstractComponentInteraction;
import com.webforj.automation.act.ComponentInteraction;
import com.webforj.automation.act.DeselectInteraction;
import com.webforj.automation.act.DoubleClickInteraction;
import com.webforj.automation.act.ItemClickInteraction;
import com.webforj.automation.act.MouseOptions;
import com.webforj.automation.act.SelectInteraction;
import com.webforj.automation.act.SelectKeyInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.table.Column;
import com.webforj.component.table.Table;
import com.webforj.component.table.event.TableSortChangeEvent;
import com.webforj.component.table.event.cell.TableCellClickEvent;
import com.webforj.component.table.event.cell.TableCellDoubleClickEvent;
import com.webforj.component.table.event.column.TableColumnMoveEvent;
import com.webforj.component.table.event.column.TableColumnResizeEvent;
import com.webforj.component.table.event.item.TableItemClickEvent;
import com.webforj.component.table.event.item.TableItemDoubleClickEvent;
import com.webforj.component.table.event.selection.TableItemDeselectEvent;
import com.webforj.component.table.event.selection.TableItemSelectEvent;
import com.webforj.component.table.event.selection.TableItemSelectionChange;
import com.webforj.data.repository.OrderCriteria;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Selects, clicks, sorts, moves and resizes the rows and columns of tables, following the selection
 * mode of the table and leaving the selection as it is on a click.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(ComponentInteraction.class)
public final class TableInteraction extends AbstractComponentInteraction<Table<Object>>
    implements SelectInteraction<Table<Object>, Object>, SelectKeyInteraction<Table<Object>>,
    DeselectInteraction<Table<Object>, Object>, ItemClickInteraction<Table<Object>, Object>,
    DoubleClickInteraction<Table<Object>, Object> {
  private static final String ID = "id";
  private static final String HOLDS_COLUMN = "holds the column '";

  /**
   * Creates the interaction.
   */
  public TableInteraction() {
    super(Table.class);
  }

  /**
   * Returns the interaction that sorts a table by the given columns, the first one sorting first.
   *
   * <pre>{@code
   * UserInteraction.perform(table, TableInteraction.sort(Map.of(name, Direction.ASC)))
   * }</pre>
   *
   * @param <T> the type of the rows of the table
   * @param criteria the direction of every sorted column, in the order they sort
   *
   * @return the interaction to perform on the table
   * @throws InteractionRejectedException when performed, if the table does not hold a column or a
   *         column is not sortable
   */
  public static <T> Consumer<Table<T>> sort(Map<Column<T, ?>, OrderCriteria.Direction> criteria) {
    return table -> {
      List<Map<String, Object>> clientCriteria = new ArrayList<>();
      int sortIndex = 1;
      for (Map.Entry<Column<T, ?>, OrderCriteria.Direction> entry : criteria.entrySet()) {
        Column<T, ?> column = entry.getKey();
        String id = resolveColumnId(table, column);
        if (!column.isSortable()) {
          throw reject(table, HOLDS_COLUMN + id + "' not sortable and a user cannot sort by it");
        }

        Map<String, Object> criterion = new HashMap<>();
        criterion.put(ID, id);
        criterion.put("sort", entry.getValue() == OrderCriteria.Direction.ASC ? "asc" : "desc");
        criterion.put("sortIndex", sortIndex++);
        clientCriteria.add(criterion);
      }

      Map<String, Object> data = new HashMap<>();
      data.put("criteria", new Gson().toJson(clientCriteria));
      fire(new TableSortChangeEvent<>(table, data));
    };
  }

  /**
   * Returns the interaction that moves the given column of a table to the given index.
   *
   * <pre>{@code
   * UserInteraction.perform(table, TableInteraction.moveColumn(name, 1))
   * }</pre>
   *
   * @param <T> the type of the rows of the table
   * @param column the column to move
   * @param index the index the user drops the column at
   *
   * @return the interaction to perform on the table
   * @throws InteractionRejectedException when performed, if the table does not hold the column or
   *         the column is fixed
   */
  public static <T> Consumer<Table<T>> moveColumn(Column<T, ?> column, int index) {
    return table -> {
      String id = resolveColumnId(table, column);
      if (!column.isMovable()) {
        throw reject(table, HOLDS_COLUMN + id + "' fixed and a user cannot move it");
      }

      Map<String, Object> data = new HashMap<>();
      data.put(ID, id);
      data.put("oldIndex", table.getColumns().indexOf(column));
      data.put("newIndex", index);
      fire(new TableColumnMoveEvent(toUntyped(table), data));
    };
  }

  /**
   * Returns the interaction that resizes the given column of a table to the given width.
   *
   * <pre>{@code
   * UserInteraction.perform(table, TableInteraction.resizeColumn(name, 80f))
   * }</pre>
   *
   * @param <T> the type of the rows of the table
   * @param column the column to resize
   * @param width the width the user lets go at
   *
   * @return the interaction to perform on the table
   * @throws InteractionRejectedException when performed, if the table does not hold the column or
   *         the column is fixed
   */
  public static <T> Consumer<Table<T>> resizeColumn(Column<T, ?> column, float width) {
    return table -> {
      String id = resolveColumnId(table, column);
      if (!column.isResizable()) {
        throw reject(table, HOLDS_COLUMN + id + "' fixed and a user cannot resize it");
      }

      Map<String, Object> data = new HashMap<>();
      data.put(ID, id);
      data.put("oldWidth", column.getWidth());
      data.put("newWidth", width);
      column.setWidth(width);
      fire(new TableColumnResizeEvent(toUntyped(table), data));
    };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void select(Table<Object> table, Object item) {
    final Map<String, Object> row = createRowData(table, item);
    if (table.getSelectionMode() == Table.SelectionMode.NONE) {
      throw reject(table, "does not select rows");
    }

    List<Object> previous = new ArrayList<>(table.getSelectedItems());
    if (previous.contains(item)) {
      return;
    }

    List<Object> deselected = new ArrayList<>();
    if (table.getSelectionMode() == Table.SelectionMode.SINGLE) {
      deselected.addAll(previous);
      table.deselectAll();
    }

    table.selectKey(table.getRepository().getKey(item));
    for (Object other : deselected) {
      fire(new TableItemDeselectEvent<>(table, createRowData(table, other)));
    }

    fire(new TableItemSelectEvent<>(table, row));
    fire(createSelectionChange(table));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void selectKey(Table<Object> table, Object key) {
    Object item = table.getRepository().find(key)
        .orElseThrow(() -> reject(table, "has no item with the key " + key));

    select(table, item);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void deselect(Table<Object> table, Object item) {
    final Map<String, Object> row = createRowData(table, item);
    if (!table.getSelectedItems().contains(item)) {
      return;
    }

    if (!table.isDeselection()) {
      throw reject(table, "does not deselect rows");
    }

    table.deselectKey(table.getRepository().getKey(item));
    fire(new TableItemDeselectEvent<>(table, row));
    fire(createSelectionChange(table));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void click(Table<Object> table, Object item, MouseOptions options) {
    Map<String, Object> row = createRowData(table, item);
    if (!table.getColumns().isEmpty()) {
      fire(
          new TableCellClickEvent<>(table, createCellData(table, item, table.getColumns().get(0))));
    }

    fire(new TableItemClickEvent<>(table, row));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void doubleClick(Table<Object> table, Object item, MouseOptions options) {
    Map<String, Object> row = createRowData(table, item);
    if (!table.getColumns().isEmpty()) {
      fire(new TableCellDoubleClickEvent<>(table,
          createCellData(table, item, table.getColumns().get(0))));
    }

    fire(new TableItemDoubleClickEvent<>(table, row));
  }

  private static TableItemSelectionChange<Object> createSelectionChange(Table<Object> table) {
    List<String> keys = new ArrayList<>();
    for (Object item : table.getSelectedItems()) {
      keys.add(table.getItemKeysRegistry().getKey(item));
    }

    Map<String, Object> data = new HashMap<>();
    data.put("keys", keys);

    return new TableItemSelectionChange<>(table, data);
  }

  private static Map<String, Object> createRowData(Table<Object> table, Object item) {
    Map<String, Object> data = new HashMap<>();
    data.put("key", resolveItemKey(table, item));
    data.put("index", new ArrayList<>(table.getItems()).indexOf(item));

    return data;
  }

  private static Map<String, Object> createCellData(Table<Object> table, Object item,
      Column<Object, ?> column) {
    Map<String, Object> data = new HashMap<>();
    data.put("itemKey", resolveItemKey(table, item));
    data.put("columnKey", resolveColumnId(table, column));

    return data;
  }

  private static String resolveItemKey(Table<Object> table, Object item) {
    if (!table.getRepository().has(item)) {
      throw reject(table, "does not contain the item '" + item + "'");
    }

    return table.getItemKeysRegistry().getKey(item);
  }

  private static <T> String resolveColumnId(Table<T> table, Column<T, ?> column) {
    if (!table.getColumns().contains(column)) {
      throw reject(table, "does not contain the column '" + column.getId() + "'");
    }

    return column.getId();
  }

  @SuppressWarnings("unchecked")
  private static Table<Object> toUntyped(Table<?> table) {
    return (Table<Object>) table;
  }
}
