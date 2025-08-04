package com.webforj.component.table;

import com.google.gson.annotations.SerializedName;
import com.webforj.component.table.event.renderer.RendererChangeEvent;
import com.webforj.component.table.renderer.Renderer;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.Serializable;
import java.util.Comparator;
import java.util.Objects;
import java.util.Optional;
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
  private int sortIndex = 0;
  private boolean suppressNavigable = false;
  private Float width = null;
  private Float minWidth = null;
  private Float maxWidth = null;
  private float flex = 0;
  private boolean resizable = true;
  private boolean movable = true;
  @SuppressWarnings("unused")
  private String cellRenderer = null;
  private Alignment align = Alignment.LEFT;
  private Type type = null;
  private String propertyName = null;

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
   * Sets the sort index of the column.
   *
   * @param sortIndex the new sort index
   * @return the column itself
   *
   * @throws IllegalArgumentException if the sort index is negative
   */
  public Column<T, V> setSortIndex(int sortIndex) {
    int oldSortIndex = this.sortIndex;
    if (sortIndex < 0) {
      throw new IllegalArgumentException("Sort index cannot be negative");
    }

    this.sortIndex = sortIndex;
    changeSupport.firePropertyChange("sortIndex", oldSortIndex, sortIndex);

    return this;
  }

  /**
   * Returns the sort index of the column.
   *
   * @return the sort index
   */
  public int getSortIndex() {
    return sortIndex;
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
   * Sets the fixed width of the column in pixels.
   *
   * <p>
   * The width property defines the desired/initial width for the column. How this width is used
   * depends on other properties and column type:
   * </p>
   *
   * <ul>
   * <li><b>Regular columns:</b>
   * <ul>
   * <li>With only width: Column renders at specified width but can shrink proportionally when
   * container is too small. The width acts as both desired and minimum width.</li>
   * <li>With width + minWidth: Column renders at specified width but only shrinks down to minWidth
   * when container is small.</li>
   * </ul>
   * </li>
   * <li><b>Pinned columns (left/right):</b> Always maintain their exact width, never participating
   * in responsive shrinking.</li>
   * <li><b>Flex columns:</b> Setting width is incompatible with flex. Use either width (fixed) or
   * flex (proportional), not both.</li>
   * </ul>
   *
   * <p>
   * If not specified, the column will use its estimated width based on content analysis of the
   * first few rows.
   * </p>
   *
   * @param width the desired width in pixels, or null to use estimated width
   * @return the column itself
   *
   * @see #setMinWidth(Float)
   * @see #setMaxWidth(Float)
   * @see #setFlex(float)
   *
   * @since 25.03
   */
  public Column<T, V> setWidth(Float width) {
    Float oldWidth = this.width;
    this.width = width;
    changeSupport.firePropertyChange("width", oldWidth, width);
    return this;
  }

  /**
   * Returns the width of the column.
   *
   * @return the width in pixels
   * @since 25.03
   */
  public Optional<Float> getWidth() {
    return Optional.ofNullable(width);
  }

  /**
   * Sets the minimum width constraint for the column in pixels.
   *
   * <p>
   * The minWidth property controls the smallest width a column can have:
   * </p>
   *
   * <ul>
   * <li><b>Regular columns:</b>
   * <ul>
   * <li>With only minWidth: Column uses minWidth as both desired and minimum width.</li>
   * <li>With width + minWidth: Column can shrink from width down to minWidth but no further.</li>
   * <li>Without minWidth: Column can shrink down to its width (or estimated width if no width
   * set).</li>
   * </ul>
   * </li>
   * <li><b>Pinned columns:</b> If only minWidth is set (no width), it becomes the fixed width.</li>
   * <li><b>Flex columns:</b> Prevents the column from shrinking below this width even when
   * container space is limited. Without minWidth, flex columns can collapse to 0 width.</li>
   * </ul>
   *
   * @param minWidth the minimum width in pixels, or null for no minimum constraint
   * @return the column itself
   *
   * @see #setWidth(Float)
   * @see #setMaxWidth(Float)
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
   * @return the minimum width in pixels
   */
  public Optional<Float> getMinWidth() {
    return Optional.ofNullable(minWidth);
  }

  /**
   * Sets the maximum width constraint for the column in pixels.
   *
   * <p>
   * The maxWidth property limits how wide a column can grow:
   * </p>
   *
   * <ul>
   * <li><b>All column types:</b> Column width will never exceed this value, regardless of content,
   * container size, or flex settings.</li>
   * <li><b>Regular/Pinned columns:</b> If width (or estimated width) exceeds maxWidth, the column
   * renders at maxWidth.</li>
   * <li><b>Flex columns:</b> Limits the maximum space a flex column can take.</li>
   * </ul>
   *
   * <p>
   * This is useful for preventing columns with long content from becoming too wide and affecting
   * readability.
   * </p>
   *
   * @param maxWidth the maximum width in pixels, or null for no maximum constraint
   * @return the column itself
   *
   * @see #setWidth(Float)
   * @see #setMinWidth(Integer)
   *
   * @since 25.03
   */
  public Column<T, V> setMaxWidth(Float maxWidth) {
    Float oldMaxWidth = this.maxWidth;
    this.maxWidth = maxWidth;
    changeSupport.firePropertyChange("maxWidth", oldMaxWidth, maxWidth);
    return this;
  }

  /**
   * Returns the maximum width of the column.
   *
   * @return the maximum width in pixels
   * @since 25.03
   */
  public Optional<Float> getMaxWidth() {
    return Optional.ofNullable(maxWidth);
  }

  /**
   * Sets the flex grow factor for proportional column sizing.
   *
   * <p>
   * The flex property makes columns share available space proportionally after fixed-width columns
   * are allocated. Key behaviors:
   * </p>
   *
   * <ul>
   * <li><b>Flex value:</b> Determines the proportion of available space. A column with flex=2 gets
   * twice the space of a column with flex=1.</li>
   * <li><b>Incompatible with width:</b> Cannot be used together with the width property. Use either
   * fixed width or flex, not both. when flex is bigger than zero then it takes effect over the
   * width setting.</li>
   * <li><b>Respects constraints:</b> Works with minWidth/maxWidth. Without minWidth, flex columns
   * can shrink to 0.</li>
   * </ul>
   *
   * <p>
   * Example scenario with 1000px table width and 3 columns:
   * </p>
   * <ul>
   * <li>Column A: width=200px (fixed)</li>
   * <li>Column B: flex=1</li>
   * <li>Column C: flex=2</li>
   * <li>Result: A=200px, B=267px, C=533px (B and C share remaining 800px in 1:2 ratio)</li>
   * </ul>
   *
   * @param flex the flex grow factor (0 for no flex, positive values for proportional sizing)
   * @return the column itself
   *
   * @see #setMinWidth(Float)
   * @see #setMaxWidth(Float)
   *
   * @since 25.03
   */
  public Column<T, V> setFlex(float flex) {
    float oldFlex = this.flex;
    this.flex = flex;
    changeSupport.firePropertyChange("flex", oldFlex, flex);
    return this;
  }

  /**
   * Returns the flex grow value of the column.
   *
   * @return the flex grow factor (0 means no flex)
   * @since 25.03
   */
  public float getFlex() {
    return flex;
  }

  /**
   * Sets whether the column is resizable by the user.
   *
   * @param resizable true to allow resizing, false to prevent it
   * @return the column itself
   *
   * @since 25.03
   */
  public Column<T, V> setResizable(boolean resizable) {
    boolean oldResizable = this.resizable;
    this.resizable = resizable;
    changeSupport.firePropertyChange("resizable", oldResizable, resizable);
    return this;
  }

  /**
   * Returns whether the column is resizable by the user.
   *
   * @return true if the column is resizable, false otherwise
   * @since 25.03
   */
  public boolean isResizable() {
    return resizable;
  }

  /**
   * Sets whether the column can be dragged and reordered by the user.
   *
   * <p>
   * When set to <code>true</code>, users can drag the column header to reposition the column within
   * the table. When set to <code>false</code>, the column header cannot be dragged.
   * </p>
   *
   * <p>
   * Non-movable columns can still change position when other columns are moved around them. This is
   * because columns must remain contiguous in the table. For example:
   * <ul>
   * <li>Given columns [A, B, C] where B is non-movable</li>
   * <li>If user drags A to position 3, the result is [B, C, A]</li>
   * <li>Column B has shifted position despite being non-movable</li>
   * </ul>
   * </p>
   *
   * <p>
   * To prevent a column from changing position entirely, use pinning. Pinned columns remain in
   * their section regardless of other column movements.
   * </p>
   *
   * <p>
   * <b>Dragging between pinned and unpinned sections:</b>
   * <ul>
   * <li>When dragging an unpinned column to a pinned section (left or right), the column
   * automatically becomes pinned to that side</li>
   * <li>When dragging a pinned column to the center (unpinned) area, the column loses its pinned
   * status and becomes a regular scrollable column</li>
   * <li>Pinned columns can be reordered within their own section (left-pinned columns stay left,
   * right-pinned columns stay right)</li>
   * </ul>
   * </p>
   *
   * @param movable {@code true} to allow dragging and reordering, {@code false} to prevent it
   *
   * @return this column instance for method chaining
   * @see #setPinDirection(PinDirection)
   *
   * @since 25.03
   */
  public Column<T, V> setMovable(boolean movable) {
    boolean oldMovable = this.movable;
    this.movable = movable;
    changeSupport.firePropertyChange("movable", oldMovable, movable);

    return this;
  }

  /**
   * Returns whether the column can be moved by the user.
   *
   * @return true if the column is movable, false otherwise
   * @since 25.03
   */
  public boolean isMovable() {
    return movable;
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
   * Sets the property name/path for this column. This is used by backend repositories that need
   * property names for SQL/REST queries. If not set, backend repositories may use the column ID as
   * a fallback.
   *
   * @param propertyName the property name/path (e.g., "name", "address.city, address.country")
   * @return the column itself
   * @since 25.02
   */
  public Column<T, V> setPropertyName(String propertyName) {
    String oldPropertyName = this.propertyName;
    this.propertyName = propertyName;
    changeSupport.firePropertyChange("propertyName", oldPropertyName, propertyName);

    return this;
  }

  /**
   * Returns the property name/path for this column. If not explicitly set, returns the column ID as
   * a fallback.
   *
   * @return the property name
   * @since 25.02
   */
  public String getPropertyName() {
    return propertyName != null ? propertyName : getId();
  }

  /**
   * Get that table instance that this column belongs to.
   *
   * @return the table instance.
   */
  public Table<T> getTable() {
    return table;
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

  private void updateClientCellRenderer() {
    this.cellRenderer = renderer.build();
    table.refreshColumns();
  }

  private EventListener<RendererChangeEvent> handleRendererChange(Renderer<T> renderer) {
    return event -> {
      if (event.getSource() == renderer) {
        this.updateClientCellRenderer();
      }
    };
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
}
