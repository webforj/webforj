package org.dwcj.addons.table;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.annotations.SerializedName;
import com.google.gson.reflect.TypeToken;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.dwcj.addons.table.event.TableSortChangeEvent;
import org.dwcj.addons.table.event.cell.TableCellClickEvent;
import org.dwcj.addons.table.event.cell.TableCellDoubleClickEvent;
import org.dwcj.addons.table.event.item.TableItemClickEvent;
import org.dwcj.addons.table.event.item.TableItemDoubleClickEvent;
import org.dwcj.addons.table.event.selection.TableItemDeselectEvent;
import org.dwcj.addons.table.event.selection.TableItemSelectEvent;
import org.dwcj.addons.table.event.selection.TableItemSelectionChange;
import org.dwcj.addons.table.renderer.Renderer;
import org.dwcj.component.element.Element;
import org.dwcj.component.element.PropertyDescriptor;
import org.dwcj.component.element.annotation.NodeName;
import org.dwcj.component.html.HtmlComponent;
import org.dwcj.data.EntityKeysRegistry;
import org.dwcj.data.repository.CollectionRepository;
import org.dwcj.data.repository.HasRepository;
import org.dwcj.data.repository.OrderCriteriaList;
import org.dwcj.data.repository.Repository;
import org.dwcj.data.repository.event.RepositoryCommitEvent;
import org.dwcj.data.selection.repository.MultipleSelectableRepository;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;

/**
 * A component designed for the presentation of tabular information in a structured, easily
 * understandable manner, optimized for handling large datasets with high performance. The Table
 * component not only facilitates the display of data in rows and columns but also enriches data
 * interaction through advanced visualization and a comprehensive suite of events for dynamic user
 * engagement. It supports a variety of functionalities aimed at enhancing data analysis,
 * navigation, and interaction:
 *
 * <ul>
 * <li><b>Selection:</b> Out of the box, selection features are disabled, but the Table can be
 * configured for both single and multiple selection modes, enabling users to select rows with a
 * simple click or through a checkbox column for multiple selections.</li>
 *
 * <li><b>Columns:</b> Offers extensive configuration options for columns including alignment, fixed
 * positioning, header inclusion, visibility control, and minimum width settings.</li>
 *
 * <li><b>Sorting:</b> Supports sortable columns, allowing users to order data by different criteria
 * (alphabetical, numerical, chronological). While server-side sorting is the default for efficiency
 * with large datasets, client-side sorting can also be enabled for immediate, in-browser data
 * organization.</li>
 *
 * <li><b>Cell Focus:</b> Enhances interaction within the Table through mouse clicks or keyboard
 * navigation to focus on specific cells.</li>
 * </ul>
 *
 * <p>
 * Designed to efficiently process and visualize large volumes of data, the Table component employs
 * virtualization techniques to maintain performance. Its event-driven architecture supports a wide
 * array of interactions including selections, clicks, double clicks, and sorting changes, making it
 * an ideal choice for creating responsive, data-intensive interfaces.
 * </p>
 *
 * @param <T> the data type the table represents
 *
 * @author Hyyan Abo Fakher
 * @since version 24.00
 */
@NodeName("bbj-table")
public final class Table<T> extends HtmlComponent<Table<T>>
    implements HasRepository<T>, MultipleSelectableRepository<Table<T>, T> {

  /**
   * The selection mode for the table.
   */
  public enum SelectionMode {
    /**
     * One row at a time can be selected.
     */
    @SerializedName("single")
    SINGLE,

    /**
     * Multiple rows can be selected.
     */
    @SerializedName("multiple")
    MULTIPLE,

    /**
     * No rows can be selected.
     */
    @SerializedName("none")
    NONE
  }

  private final EntityKeysRegistry keyRegistry = new EntityKeysRegistry();
  private Repository<T> repository = new CollectionRepository<>(Collections.emptyList());
  private List<Column<T, ?>> columns = new ArrayList<>();
  private Set<String> selectedKeys = new HashSet<>();
  private OrderCriteriaList<T> orderCriteriaList = new OrderCriteriaList<>();
  private EventListener<TableSortChangeEvent<T>> handleSortChangedListener =
      this::handleSortChanged;
  private ListenerRegistration<TableSortChangeEvent<T>> handleSortChangedListenerRegistration =
      null;

  // Internal properties
  private final PropertyDescriptor<List<Column<T, ?>>> columnDefinitionsProp =
      PropertyDescriptor.property("columnDefinitions", Collections.emptyList());
  private final PropertyDescriptor<JsonArray> dataProp =
      PropertyDescriptor.property("data", new JsonArray());
  private final PropertyDescriptor<String> getRowIdProp =
      PropertyDescriptor.property("getRowId", "row.data.__APPID__");
  private final PropertyDescriptor<Set<String>> selectedProp =
      PropertyDescriptor.property("selected", Collections.emptySet());

  // public properties though getters and setters
  private final PropertyDescriptor<Boolean> headerCheckboxSelectionProp =
      PropertyDescriptor.property("headerCheckboxSelection", true);
  private final PropertyDescriptor<Boolean> checkboxSelectionProp =
      PropertyDescriptor.property("checkboxSelection", true);
  private final PropertyDescriptor<Boolean> deselectionProp =
      PropertyDescriptor.property("deselection", true);
  private final PropertyDescriptor<SelectionMode> selectionModeProp =
      PropertyDescriptor.property("selectionMode", SelectionMode.NONE);
  private final PropertyDescriptor<Boolean> multiSelectWithClickProp =
      PropertyDescriptor.property("multiSelectWithClick", false);
  private final PropertyDescriptor<Double> headerHeight =
      PropertyDescriptor.property("headerHeight", 48d);
  private final PropertyDescriptor<Double> rowHeight =
      PropertyDescriptor.property("rowHeight", 35d);
  private final PropertyDescriptor<Double> overscan = PropertyDescriptor.property("overscan", 35d);
  private final PropertyDescriptor<Boolean> clientSorting =
      PropertyDescriptor.property("clientSorting", false);

  /**
   * Construct a new Table.
   */
  public Table() {
    super();
    set(getRowIdProp, getRowIdProp.getDefaultValue());
    getElement().whenDefined().thenAccept(this::onInit).exceptionally(this::onInitFailed);
  }

  /**
   * Get the key entity registry.
   *
   * @return the key entity registry instance
   */
  public EntityKeysRegistry getItemKeysRegistry() {
    return keyRegistry;
  }

  /**
   * Adds a new column to the table with the specified id, and the value provider.
   *
   * @see Column#setValueProvider(Function)
   *
   * @param <V> the type of the value provided by the provider
   * @param id the column id
   * @param provider the value provider for the column
   *
   * @return the new column
   */
  public <V> Column<T, V> addColumn(String id, Function<T, V> provider) {
    Column<T, V> column = new Column<>(this, id);
    column.setValueProvider(provider);
    column.setLabel(capitalizeFirstLetter(id));
    columns.add(column);

    return column;
  }

  /**
   * Adds a new column to the table with the specified value provider.
   *
   * @see #addColumn(String, Function)
   *
   * @param provider the value provider for the column
   * @return the new column
   */
  public <V> Column<T, V> addColumn(Function<T, V> provider) {
    return addColumn(null, provider);
  }

  /**
   * Adds a new column to the table with the specified id, and the renderer.
   *
   * @see Column#setRenderer(Renderer)
   *
   * @param <V> the type of the value provided by the renderer
   * @param id the column id
   * @param renderer the renderer for the column
   *
   * @return the new column
   */
  public <V> Column<T, V> addColumn(String id, Renderer<T> renderer) {
    Column<T, V> column = new Column<>(this, id);
    column.setValueProvider(i -> null);
    column.setRenderer(renderer);
    column.setLabel("");
    columns.add(column);

    return column;
  }

  /**
   * Adds a new column to the table with the specified renderer.
   *
   * @see #addColumn(String, Renderer)
   *
   * @param renderer the renderer for the column
   * @return the new column
   */
  public <V> Column<T, V> addColumn(Renderer<T> renderer) {
    return addColumn(null, renderer);
  }

  /**
   * Gets the columns of the table by id.
   *
   * @param id the id of the column
   *
   * @return the column with the given id
   */
  public Column<T, ?> getColumnById(String id) {
    return columns.stream().filter(column -> column.getId().equals(id)).findFirst().orElse(null);
  }

  /**
   * Check if a given column exists in the table.
   *
   * @param id the id of the column
   * @return true if the column exists, false otherwise
   */
  public boolean hasColumn(String id) {
    return columns.stream().anyMatch(column -> column.getId().equals(id));
  }

  /**
   * Removes the column with the given id.
   *
   * @param id the id of the column
   *
   * @return the table itself.
   */
  public Table<T> removeColumn(String id) {
    Column<T, ?> column = getColumnById(id);
    columns.remove(column);

    return this;
  }

  /**
   * Removes the given column.
   *
   * @param column the column
   *
   * @return the table itself.
   */
  public Table<T> removeColumn(Column<T, ?> column) {
    return removeColumn(column.getId());
  }

  /**
   * Gets the columns of the table.
   *
   * @return the columns
   */
  public List<Column<T, ?>> getColumns() {
    return Collections.unmodifiableList(columns);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Table<T> setRepository(Repository<T> repository) {
    Objects.requireNonNull(repository, "The repository cannot be null");

    // set the repository if not the same as the current repository

    if (this.repository != null && !this.repository.equals(repository)) {
      this.repository = repository;
      this.repository.onCommit(this::handleRepositoryCommit);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Repository<T> getRepository() {
    return repository;
  }

  /**
   * Sets the rows of the table.
   *
   * @param rows the items
   *
   * @return the component itself.
   */
  public Table<T> setItems(Collection<T> rows) {
    this.setRepository(new CollectionRepository<>(rows));
    if (getElement().isDefined()) {
      refershItems();
    }

    return this;
  }

  /**
   * Gets the rows of the table.
   *
   * <p>
   * Not that the returned collection is not necessarily the same as the one passed to
   * {@link #setItems(Collection)}.
   * </p>
   *
   * @return the items
   */
  public Collection<T> getItems() {
    return getRepository().findAll().toList();
  }

  /**
   * Gets the cell value for the given row and column.
   *
   * @param item the row
   * @param column the column
   *
   * @return the cell value
   */
  public <V> Object getCellValue(T item, Column<T, V> column) {
    Function<T, V> valueProvider = column.getValueProvider();
    return valueProvider != null ? valueProvider.apply(item) : null;
  }

  /**
   * Refresh the columns of the table.
   *
   * @return the component itself
   */
  public Table<T> refershColumns() {
    if (getElement().isDefined()) {
      set(columnDefinitionsProp, columns);
    }

    return this;
  }

  /**
   * Refresh the items of the table.
   *
   * @return the component itself
   */
  public Table<T> refershItems() {
    if (getElement().isDefined()) {
      set(dataProp, buildData());
    }

    return this;
  }

  /**
   * Refresh everything in the table to reflect the current state of the table on the server.
   *
   * @return the table
   */
  public Table<T> refersh() {
    this.refershItems();
    this.refershColumns();

    return this;
  }

  /**
   * Selects all items in the table.
   *
   * @return the component itself
   */
  public Table<T> selectAll() {
    if (getElement().isDefined()) {
      getElement().callJsFunction("selectAll");
    } else {
      Set<String> appKeys = getRepository().findAll().map(x -> getItemKeysRegistry().getKey(x))
          .collect(Collectors.toSet());

      selectedKeys.addAll(appKeys);
      set(selectedProp, selectedKeys);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Table<T> selectKey(Object... keys) {
    selectedKeys.addAll(new HashSet<>(Arrays.asList(mapKeys(keys))));

    if (getElement().isDefined()) {
      getElement().callJsFunction("select", selectedKeys);
    } else {
      set(selectedProp, selectedKeys);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SafeVarargs
  public final Table<T> select(T... items) {
    Object[] keys = new Object[items.length];
    for (int i = 0; i < items.length; i++) {
      keys[i] = getRepository().getKey(items[i]);
    }

    return selectKey(keys);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Table<T> selectIndex(int... indices) {
    Object[] keys = new Object[indices.length];
    for (int i = 0; i < indices.length; i++) {
      T entity = getRepository().findByIndex(indices[i]).orElse(null);
      if (entity != null) {
        keys[i] = getRepository().getKey(entity);
      }
    }

    return selectKey(keys);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Table<T> deselectAll() {
    selectedKeys.clear();

    if (getElement().isDefined()) {
      getElement().callJsFunction("deselectAll");
    } else {
      set(selectedProp, selectedKeys);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Table<T> deselectKey(Object... key) {
    Set<Object> idsSet = new HashSet<>(Arrays.asList(mapKeys(key)));
    selectedKeys.removeAll(idsSet);

    if (getElement().isDefined()) {
      getElement().callJsFunction("deselect", idsSet);
    } else {
      set(selectedProp, selectedKeys);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Table<T> deselectIndex(int... index) {
    Object[] keys = new Object[index.length];
    for (int i = 0; i < index.length; i++) {
      T entity = getRepository().findByIndex(index[i]).orElse(null);
      if (entity != null) {
        keys[i] = getRepository().getKey(entity);
      }
    }

    return deselectKey(keys);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<T> getSelectedItems() {
    TypeToken<Set<String>> typeToken = new TypeToken<>() {};
    Set<String> keys = get(selectedProp, isAttached(), typeToken.getType());

    return keys.stream().map(key -> (T) getItemKeysRegistry().getEntity(key)).toList();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<Integer> getSelectedIndices() {
    List<T> items = getSelectedItems();
    return items.stream().map(item -> getRepository().getIndex(item)).toList();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getSelectedIndex() {
    return getSelectedIndices().stream().findFirst().orElse(-1);
  }

  /**
   * Set the table's header height in pixels.
   *
   * @return the component itself
   */
  public Table<T> setHeaderHeight(double value) {
    set(headerHeight, value);
    return this;
  }

  /**
   * Get the table's header height in pixels.
   *
   * @return the table's header height in pixels
   */
  public double getHeaderHeight() {
    return get(headerHeight);
  }

  /**
   * Sets the height of each row in the table, in pixels.
   *
   * <p>
   * Setting a fixed row height improves the performance of the table when dealing with large data
   * sets. By knowing the height of each row in advance, the table can more efficiently calculate
   * the layout and scrolling position.
   * </p>
   *
   * <p>
   * Additionally, setting a specific row height can be useful for achieving a consistent visual
   * appearance, especially when the table rows contain varying amounts of content or different
   * types of content.
   * </p>
   *
   * @param value the height of each row in pixels
   * @return the component itself
   */
  public Table<T> setRowHeight(double value) {
    set(rowHeight, value);
    return this;
  }

  /**
   * Get the table's row height in pixels.
   *
   * @return the table's row height in pixels
   */
  public double getRowHeight() {
    return get(rowHeight);
  }

  /**
   * Sets the number of rows to pre-render beyond the visible scrolling area.
   *
   * <p>
   * This method is part of the table's row virtualization feature. Row virtualization is an
   * optimization technique that improves performance for large data sets. Instead of rendering all
   * rows at once, the table only renders the rows that are currently visible in the viewport, plus
   * a certain number of rows outside the viewport defined by the overscan value.
   * </p>
   *
   * <p>
   * The overscan value determines how many rows to render outside of the visible area. A higher
   * overscan value can reduce the frequency of rendering when scrolling, but at the cost of
   * rendering more rows than are visible at any one time. This can be a trade-off between rendering
   * performance and scroll smoothness.
   * </p>
   *
   * @param value the number of rows to pre-render beyond the visible scrolling area
   * @return the component itself
   */
  public Table<T> setOverscan(double value) {
    set(overscan, value);
    return this;
  }

  /**
   * Gets the number of rows to pre-render beyond the visible scrolling area.
   *
   * @return the number of rows to pre-render beyond the visible scrolling area
   * @see #setOverscan(double)
   */
  public double getOverscan() {
    return get(overscan);
  }

  /**
   * Enables or disables client-side sorting of the table.
   *
   * <p>
   * By default, the table sorts data on the server side. However, enabling client-side sorting
   * allows the table to sort data on the client side. This can enhance performance or be useful
   * when the server cannot sort data. When client-side sorting is active, the table sorts the data
   * based on the inferred client column type. This inference happens on the server at runtime based
   * on the value returned by the column's value provider. The supported types are: {@code Text},
   * {@code Number}, {@code Boolean}, {@code Date}, {@code Time}, and {@code DateTime}. If the type
   * is not supported, the table defaults to sorting as {@code Text}.
   * </p>
   *
   * @param enabled {@code false} to disable client-side sorting, {@code true} to enable
   * @return the component itself
   */
  public Table<T> setClientSorting(boolean enabled) {
    // enable or disable server sorting listener
    if (enabled) {
      if (handleSortChangedListenerRegistration != null) {
        handleSortChangedListenerRegistration.remove();
        handleSortChangedListenerRegistration = null;
      }
    } else {
      if (handleSortChangedListenerRegistration == null) {
        handleSortChangedListenerRegistration = addSortChangeListener(handleSortChangedListener);
      }
    }

    set(clientSorting, enabled);
    return this;
  }

  /**
   * Checks if client-side sorting is enabled.
   *
   * @return {@code false} if client-side sorting is disabled, {@code true} otherwise
   */
  public boolean isClientSorting() {
    return get(clientSorting);
  }

  /**
   * Enables or disables the rendering of selection checkbox in the header.
   *
   * @param value {@code false} to suppress rendering of selection checkbox in the header,
   *        {@code true} to enable
   * @return the component itself
   */
  public Table<T> setHeaderCheckboxSelection(boolean value) {
    set(headerCheckboxSelectionProp, value);
    return this;
  }

  /**
   * Checks if the rendering of selection checkbox in the header is suppressed.
   *
   * @return {@code false} if the rendering of selection checkbox in the header is suppressed,
   *         {@code true} otherwise
   */
  public boolean isHeaderCheckboxSelection() {
    return get(headerCheckboxSelectionProp);
  }

  /**
   * Enables or disables the rendering of selection checkboxes for each row.
   *
   * @param value {@code false} to suppress rendering of selection checkboxes, {@code true} to
   *        enable
   * @return the component itself
   */
  public Table<T> setCheckboxSelection(boolean value) {
    set(checkboxSelectionProp, value);
    return this;
  }

  /**
   * Checks if the rendering of selection checkboxes for each row is suppressed.
   *
   * @return {@code false} if the rendering of selection checkboxes is suppressed, {@code true}
   *         otherwise
   */
  public boolean isCheckboxSelection() {
    return get(checkboxSelectionProp);
  }

  /**
   * Set to false to prevent rows from being deselected when holding down <code>Ctrl</code> and
   * clicking the row.
   *
   * <p>
   * Once a row is selected, it remains selected until another row is selected in its place. By
   * default, the table allows deselection of rows.
   * </p>
   *
   * @param value {@code false} to prevent row deselection, {@code true} to allow
   * @return the component itself
   */
  public Table<T> setDeselection(boolean value) {
    set(deselectionProp, value);
    return this;
  }

  /**
   * Checks if row deselection is allowed.
   *
   * @return {@code false} if row deselection is prevented, {@code true} otherwise
   */
  public boolean isDeselection() {
    return get(deselectionProp);
  }

  /**
   * Sets the selection mode for the table.
   *
   * @param mode the selection mode
   * @return the component itself
   */
  public Table<T> setSelectionMode(SelectionMode mode) {
    set(selectionModeProp, mode);
    return this;
  }

  /**
   * Gets the selection mode for the table.
   *
   * @return the selection mode
   */
  public SelectionMode getSelectionMode() {
    return get(selectionModeProp);
  }

  /**
   * Set to true to allow multiple rows to be selected with clicks.
   *
   * <p>
   * For example, if you click to select one row and then click to select another row, the first row
   * will stay selected as well. Clicking a selected row in this mode will deselect the row. This is
   * useful for touch devices where Ctrl and Shift clicking is not an option.
   * </p>
   *
   * @param value {@code true} to enable multi selection with clicks, {@code false} to suppress
   * @return the component itself
   */
  public Table<T> setMultiSelectWithClick(boolean value) {
    set(multiSelectWithClickProp, value);
    return this;
  }

  /**
   * Checks if multi selection with click is suppressed.
   *
   * @return {@code true} if multi selection with click is suppressed, {@code false} otherwise
   */
  public boolean isMultiSelectWithClick() {
    return get(multiSelectWithClickProp);
  }

  /**
   * Adds a listener for the row click event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableItemClickEvent<T>> addItemClickListener(
      EventListener<TableItemClickEvent<T>> listener) {
    return addEventListener(TableItemClickEvent.class, listener);
  }

  /**
   * Alias for {@link #addItemClickListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableItemClickEvent<T>> onItemClick(
      EventListener<TableItemClickEvent<T>> listener) {
    return addItemClickListener(listener);
  }

  /**
   * Adds a listener for the row double click event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableItemDoubleClickEvent<T>> addItemDoubleClickListener(
      EventListener<TableItemDoubleClickEvent<T>> listener) {
    return addEventListener(TableItemDoubleClickEvent.class, listener);
  }

  /**
   * Alias for {@link #addItemDoubleClickListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableItemDoubleClickEvent<T>> onItemDoubleClick(
      EventListener<TableItemDoubleClickEvent<T>> listener) {
    return addItemDoubleClickListener(listener);
  }

  /**
   * Adds a listener for the cell click event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableCellClickEvent<T>> addCellClickListener(
      EventListener<TableCellClickEvent<T>> listener) {
    return addEventListener(TableCellClickEvent.class, listener);
  }

  /**
   * Alias for {@link #addCellClickListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableCellClickEvent<T>> onCellClick(
      EventListener<TableCellClickEvent<T>> listener) {
    return addCellClickListener(listener);
  }

  /**
   * Adds a listener for the cell double click event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableCellDoubleClickEvent<T>> addCellDoubleClickListener(
      EventListener<TableCellDoubleClickEvent<T>> listener) {
    return addEventListener(TableCellDoubleClickEvent.class, listener);
  }

  /**
   * Alias for {@link #addCellDoubleClickListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableCellDoubleClickEvent<T>> onCellDoubleClick(
      EventListener<TableCellDoubleClickEvent<T>> listener) {
    return addCellDoubleClickListener(listener);
  }

  /**
   * Adds a listener for the row selection event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableItemSelectEvent<T>> addItemSelectListener(
      EventListener<TableItemSelectEvent<T>> listener) {
    return addEventListener(TableItemSelectEvent.class, listener);
  }

  /**
   * Alias for {@link #addItemSelectListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableItemSelectEvent<T>> onItemSelect(
      EventListener<TableItemSelectEvent<T>> listener) {
    return addItemSelectListener(listener);
  }

  /**
   * Adds a listener for the row deselection event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableItemDeselectEvent<T>> addItemDeselectListener(
      EventListener<TableItemDeselectEvent<T>> listener) {
    return addEventListener(TableItemDeselectEvent.class, listener);
  }

  /**
   * Alias for {@link #addItemDeselectListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableItemDeselectEvent<T>> onItemDeselect(
      EventListener<TableItemDeselectEvent<T>> listener) {
    return addItemDeselectListener(listener);
  }

  /**
   * Adds a listener for the selection change event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableItemSelectionChange<T>> addItemSelectionChangeListener(
      EventListener<TableItemSelectionChange<T>> listener) {
    return addEventListener(TableItemSelectionChange.class, listener);
  }

  /**
   * Alias for {@link #addItemSelectionChangeListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableItemSelectionChange<T>> onItemSelectionChange(
      EventListener<TableItemSelectionChange<T>> listener) {
    return addItemSelectionChangeListener(listener);
  }

  /**
   * Adds a listener for the sort change event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableSortChangeEvent<T>> addSortChangeListener(
      EventListener<TableSortChangeEvent<T>> listener) {
    return addEventListener(TableSortChangeEvent.class, listener);
  }

  /**
   * Alias for {@link #addSortChangeListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<TableSortChangeEvent<T>> onSortChange(
      EventListener<TableSortChangeEvent<T>> listener) {
    return addSortChangeListener(listener);
  }

  void onInit(Element el) {
    this.refersh();
    set(selectedProp, selectedKeys);

    if (!isClientSorting()) {
      handleSortChangedListenerRegistration = addSortChangeListener(handleSortChangedListener);
    }
  }

  Void onInitFailed(Throwable e) {
    System.err.println(e.getMessage()); // NOSONAR
    e.printStackTrace(); // NOSONAR
    return null;
  }

  JsonArray buildData() {
    Stream<T> data = repository.findAll();
    JsonArray populated = new JsonArray();

    data.forEach(row -> {
      JsonObject rowJson = buildItem(row);
      populated.add(rowJson);
    });

    return populated;
  }

  JsonObject buildItem(T item) {
    JsonObject rowJson = new JsonObject();

    for (Column<T, ?> column : columns) {
      String id = column.getId();
      Object value = column.getValue(item);

      if (column.getClientType() == null && value != null) {
        // try to determine the type of the column
        Class<?> type = column.getValue(item).getClass();
        column.figureClientType(type);
      }

      rowJson.addProperty(id, String.valueOf(value));
    }

    // add __APPID__ to the row
    rowJson.addProperty("__APPID__", getItemKeysRegistry().getKey(item));

    return rowJson;
  }

  void handleRepositoryCommit(RepositoryCommitEvent<T> e) {
    List<T> commits = e.getCommits();

    if (commits.size() == 1) {
      // update a single item
      Element el = getElement();
      if (getElement().isDefined()) {
        T commit = commits.get(0);
        Object key = getRepository().getKey(commit);

        el.callJsFunctionAsync("updateRow", key, buildItem(commit));
      }
    } else {
      this.refersh();
    }
  }

  void handleSortChanged(TableSortChangeEvent<T> e) {
    Map<String, String> clientCriterion = e.getClientCriterion();

    // update the columns with the new sort directions
    for (Column<T, ?> column : getColumns()) {
      String id = column.getId();
      String direction = clientCriterion.get(id);

      if (direction != null) {
        column.setSortDirection("asc".equalsIgnoreCase(direction) ? Column.SortDirection.ASC
            : Column.SortDirection.DESC);
      } else {
        column.setSortDirection(Column.SortDirection.NONE);
      }
    }

    orderCriteriaList.set(e.getOrderCriteriaList());

    Repository<T> repo = getRepository();
    repo.getOrderCriteriaList().set(e.getOrderCriteriaList());

    repo.commit();
  }

  String[] mapKeys(Object... keys) {
    String[] appKeys = new String[keys.length];
    for (int i = 0; i < keys.length; i++) {
      T entity = getRepository().findByKey(keys[i]).orElse(null);
      if (entity != null) {
        appKeys[i] = getItemKeysRegistry().getKey(entity);
      }
    }

    return appKeys;
  }

  private static final String capitalizeFirstLetter(String input) {
    if (input == null || input.isEmpty()) {
      return input;
    }

    return input.substring(0, 1).toUpperCase() + input.substring(1);
  }
}
