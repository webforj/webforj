package com.webforj.addons.table;

import com.google.gson.annotations.SerializedName;
import com.webforj.addons.table.event.renderer.RendererChangeEvent;
import com.webforj.addons.table.renderer.Renderer;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.Serializable;
import java.util.Comparator;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Function;

/**
 * Represents a column in a table.
 *
 * @param <T> the type of the row data
 * @param <V> the type of the column data
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public final class Column<T, V> implements Serializable {

  /**
   * Describes the pin direction of the column.
   */
  public enum PinDirection {
    /**
     * The column is pinned to the left.
     */
    @SerializedName("left")
    LEFT,

    /**
     * The column is pinned to the right.
     */
    @SerializedName("right")
    RIGHT,

    /**
     * The column is not pinned to the left or right. and appears as according to the insertion
     * order of the columns.
     */
    @SerializedName("auto")
    AUTO
  }

  /**
   * Describes the sort direction of the column.
   */
  public enum SortDirection {
    /**
     * The column is sorted in ascending order.
     */
    @SerializedName("asc")
    ASC,

    /**
     * The column is sorted in descending order.
     */
    @SerializedName("desc")
    DESC,

    /**
     * The column is not sorted.
     */
    @SerializedName("none")
    NONE
  }

  /**
   * Describes the alignment of the column.
   */
  public enum Alignment {
    /**
     * The column content is aligned to the left.
     */
    @SerializedName("left")
    LEFT,

    /**
     * The column content is aligned to the center.
     */
    @SerializedName("center")
    CENTER,

    /**
     * The column content is aligned to the right.
     */
    @SerializedName("right")
    RIGHT
  }

  enum Type {
    @SerializedName("text")
    TEXT,

    @SerializedName("number")
    NUMBER,

    @SerializedName("boolean")
    BOOLEAN,

    @SerializedName("date")
    DATE,

    @SerializedName("datetime")
    DATETIME,

    @SerializedName("time")
    TIME
  }

  private final transient PropertyChangeSupport changeSupport = new PropertyChangeSupport(this);
  private final transient Table<T> table;
  private transient Function<T, V> valueProvider;
  private transient Renderer<T> renderer;
  private transient Comparator<T> comparator;
  private transient ListenerRegistration<RendererChangeEvent> rendererChangeListener;

  private final String id;
  private boolean hidden = false;
  private String label = "";
  private PinDirection pinned = PinDirection.AUTO;
  private boolean sortable = false;
  private SortDirection sort = SortDirection.NONE;
  private boolean suppressNavigable = false;
  private Float minWidth = null;
  @SuppressWarnings("unused")
  private String cellRenderer = null;
  private Alignment align = Alignment.LEFT;
  private Type type = null;

  /**
   * Constructs a column with the given id.
   *
   * <p>
   * Note if a column's ID is null then an auto-generated ID will be used. Once a column is created
   * the column ID cannot be changed.
   * </p>
   *
   * @param table the table
   * @param id the id of the column
   */
  public Column(Table<T> table, String id) {
    this.id = id == null ? UUID.randomUUID().toString().substring(0, 8) : id;
    this.table = table;
  }

  /**
   * Returns the id of the column.
   *
   * @return the id of the column
   */
  public String getId() {
    return id;
  }

  /**
   * Sets the value provider for the column.
   *
   * <p>
   * The value provider is a function that takes an instance of the row data type and returns the
   * value to be displayed in the column for that row. This function is used when the table needs to
   * display data, allowing for dynamic and custom data representation in each column.
   * </p>
   *
   * @param provider the value provider function to set.
   *
   * @return the column itself
   */
  Column<T, V> setValueProvider(Function<T, V> provider) {
    this.valueProvider = provider;

    return this;
  }

  /**
   * Returns the value provider for the column.
   *
   * @return the value provider
   */
  public Function<T, V> getValueProvider() {
    return valueProvider;
  }

  /**
   * Returns the value of the column for the given row.
   *
   * @param row the row
   * @return the value of the column for the given row
   */
  public V getValue(T row) {
    return valueProvider.apply(row);
  }

  /**
   * Sets the renderer for the column.
   *
   * @param renderer the renderer to set
   * @return the column itself
   */
  public Column<T, V> setRenderer(Renderer<T> renderer) {
    Objects.requireNonNull(renderer, "Renderer cannot be null");

    if (this.renderer == renderer) {
      return this;
    }

    if (this.rendererChangeListener != null) {
      this.rendererChangeListener.remove();
    }

    this.renderer = renderer;
    this.rendererChangeListener = renderer.addChangeListener(handleRendererChange(renderer));
    if (this.renderer.getTable() == null) {
      this.renderer.setTable(table);
    } else if (this.renderer.getTable() != table) {
      throw new IllegalArgumentException("Renderer is already attached to another table");
    }

    this.updateClientCellRenderer();

    return this;
  }

  /**
   * Returns the renderer for the column.
   *
   * @return the renderer
   */
  public Renderer<T> getRenderer() {
    return renderer;
  }

  /**
   * Sets the comparator for the column.
   *
   * <p>
   * The comparator is used to compare the values of the column for sorting. This function is used
   * when the table needs to sort data, allowing for dynamic and custom sorting in each column.
   * </p>
   *
   * @param comparator the comparator to set
   * @return the column itself
   */
  public Column<T, V> setComparator(Comparator<T> comparator) {
    this.comparator = comparator;

    return this;
  }

  /**
   * Returns the comparator for the column.
   *
   * @return the comparator
   */
  public Comparator<T> getComparator() {
    return comparator;
  }

  /**
   * Hide/show the column.
   *
   * @param hidden the new hidden status
   * @return the column itself
   */
  public Column<T, V> setHidden(boolean hidden) {
    boolean oldHidden = this.hidden;
    this.hidden = hidden;
    changeSupport.firePropertyChange("hidden", oldHidden, hidden);

    return this;
  }

  /**
   * Returns the hidden status of the column.
   *
   * @return the hidden status
   */
  public boolean isHidden() {
    return hidden;
  }

  /**
   * Sets the label of the column.
   *
   * @param label the new label
   * @return the column itself
   */
  public Column<T, V> setLabel(String label) {
    String oldLabel = this.label;
    this.label = label;
    changeSupport.firePropertyChange("label", oldLabel, label);

    return this;
  }

  /**
   * Returns the label of the column.
   *
   * @return the label of the column
   */
  public String getLabel() {
    return label;
  }

  /**
   * Sets the pin direction of the column.
   *
   * @param direction the new pin direction
   * @return the column itself
   */
  public Column<T, V> setPinDirection(PinDirection direction) {
    PinDirection oldPinned = this.pinned;
    this.pinned = direction;
    changeSupport.firePropertyChange("pinDirection", oldPinned, direction);

    return this;
  }

  /**
   * Returns the pin direction of the column.
   *
   * @return the pin direction
   */
  public PinDirection getPinDirection() {
    return pinned;
  }

  /**
   * Enable or disable sorting for the column.
   *
   * @param sortable the new sortable status
   * @return the column itself
   */
  public Column<T, V> setSortable(boolean sortable) {
    boolean oldSortable = this.sortable;
    this.sortable = sortable;
    changeSupport.firePropertyChange("sortable", oldSortable, sortable);

    return this;
  }

  /**
   * Returns the sortable status of the column.
   *
   * @return the sortable status
   */
  public boolean isSortable() {
    return sortable;
  }

  /**
   * Sets the sort direction of the column.
   *
   * @param direction the new sort direction
   * @return the column itself
   */
  public Column<T, V> setSortDirection(SortDirection direction) {
    SortDirection oldSort = this.sort;
    this.sort = direction;
    changeSupport.firePropertyChange("sortDirection", oldSort, direction);

    return this;
  }

  /**
   * Returns the sort direction of the column.
   *
   * @return the sort direction
   */
  public SortDirection getSortDirection() {
    return sort;
  }

  /**
   * Sets the suppress navigable status of the column.
   *
   * @param suppressNavigable the new suppress navigable status
   * @return the column itself
   */
  public Column<T, V> setSuppressNavigable(boolean suppressNavigable) {
    boolean oldSuppressNavigable = this.suppressNavigable;
    this.suppressNavigable = suppressNavigable;
    changeSupport.firePropertyChange("suppressNavigable", oldSuppressNavigable, suppressNavigable);

    return this;
  }

  /**
   * Returns the suppress navigable status of the column.
   *
   * @return the suppress navigable status
   */
  public boolean isSuppressNavigable() {
    return suppressNavigable;
  }

  /**
   * Sets the minimum width of the column.
   *
   * @param minWidth the new minimum width
   * @return the column itself
   */
  public Column<T, V> setMinWidth(Float minWidth) {
    Float oldMinWidth = this.minWidth;
    this.minWidth = minWidth;
    changeSupport.firePropertyChange("minWidth", oldMinWidth, minWidth);

    return this;
  }

  /**
   * Returns the minimum width of the column.
   *
   * @return the minimum width
   */
  public Float getMinWidth() {
    return minWidth;
  }

  /**
   * Sets the alignment of the column.
   *
   * @param align the new alignment
   * @return the column itself
   */
  public Column<T, V> setAlignment(Alignment align) {
    Alignment oldAlign = this.align;
    this.align = align;
    changeSupport.firePropertyChange("alignment", oldAlign, align);

    return this;
  }

  /**
   * Returns the alignment of the column.
   *
   * @return the alignment
   */
  public Alignment getAlignment() {
    return align;
  }

  /**
   * Returns the client type of the column.
   *
   * @return the client type
   */
  Type getClientType() {
    return type;
  }

  /**
   * Figures out the client type of the column based on the given Java type.
   *
   * @param type the new type
   * @return the column itself
   */
  Column<T, V> figureClientType(Class<?> type) {
    if (type == String.class) {
      this.type = Type.TEXT;
    } else if (Number.class.isAssignableFrom(type)) {
      this.type = Type.NUMBER;
    } else if (type == Boolean.class) {
      this.type = Type.BOOLEAN;
    } else if (type == java.util.Date.class || type == java.sql.Date.class
        || type == java.time.LocalDate.class) {
      this.type = Type.DATE;
    } else if (type == java.time.LocalDateTime.class || type == java.time.ZonedDateTime.class
        || type == java.time.OffsetDateTime.class || type == java.sql.Timestamp.class) {
      this.type = Type.DATETIME;
    } else if (type == java.time.LocalTime.class || type == java.time.OffsetTime.class) {
      this.type = Type.TIME;
    }

    return this;
  }

  /**
   * Get that table instance that this column belongs to.
   *
   * @return the table instance.
   */
  public Table<T> getTable() {
    return table;
  }

  private void updateClientCellRenderer() {
    this.cellRenderer = renderer.build();
    table.refershColumns();
  }

  private EventListener<RendererChangeEvent> handleRendererChange(Renderer<T> renderer) {
    return event -> {
      if (event.getSource() == renderer) {
        this.updateClientCellRenderer();
      }
    };
  }

  /**
   * Adds a property change listener.
   *
   * @param listener the listener
   * @return the column itself
   */
  public Column<T, V> addPropertyChangeListener(PropertyChangeListener listener) {
    this.changeSupport.addPropertyChangeListener(listener);
    return this;
  }

  /**
   * Removes a property change listener.
   *
   * @param listener the listener
   * @return the column itself
   */
  public Column<T, V> removePropertyChangeListener(PropertyChangeListener listener) {
    this.changeSupport.removePropertyChangeListener(listener);
    return this;
  }
}
