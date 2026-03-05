package com.webforj.component.table.renderer;

import com.google.gson.Gson;
import com.webforj.component.table.Table;
import com.webforj.component.table.event.renderer.RendererChangeEvent;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Represents a renderer for a column based on a lodash template.
 *
 * <p>
 * Lodash templates enable the insertion of JavaScript logic directly into HTML, making them highly
 * effective for rendering complex cell data in a table. This approach allows for the dynamic
 * generation of HTML based on cell data, facilitating rich and interactive table cell content.
 * </p>
 *
 * <p>
 * Syntax overview for lodash templates:
 * </p>
 * <ul>
 * <li><code>&lt;%= ... %&gt;</code> - Interpolates values, inserting the JavaScript code's result
 * into the template.</li>
 * <li><code>&lt;% ... %&gt;</code> - Executes JavaScript code, allowing loops, conditionals, and
 * more.</li>
 * <li><code>&lt;%- ... %&gt;</code> - Escapes HTML content, ensuring interpolated data is safe from
 * HTML injection attacks.</li>
 * </ul>
 *
 * <p>
 * Examples using cell data:
 * </p>
 *
 * <p>
 * <b>1. Simple value interpolation:</b> Directly display the cell's value.
 * </p>
 *
 * <pre>
 * Template: &lt;%= cell.value %&gt;
 * </pre>
 *
 * <p>
 * <b>2. Conditional rendering:</b> Use JavaScript logic to conditionally render content.
 * </p>
 *
 * <pre>
 * Template: &lt;% if (cell.value > 100) { %&gt; 'High' &lt;% } else { %&gt; 'Normal' &lt;% } %&gt;
 * </pre>
 *
 * <p>
 * <b>3. Combining data fields:</b> Render content using multiple data fields from the cell.
 * </p>
 *
 * <pre>
 * Template: &lt;%= cell.row.getValue('firstName') + ' ' + cell.row.getValue('lastName') %&gt;
 * </pre>
 *
 * <p>
 * <b>4. Escaping HTML content:</b> Safely render user-generated content.
 * </p>
 *
 * <p>
 * The renderer has access to detailed cell, row, and column properties in the client side
 * </p>
 *
 * <p>
 * <b>TableCell Properties:</b>
 * </p>
 * <table border="1">
 * <tr>
 * <th>Property</th>
 * <th>Type</th>
 * <th>Description</th>
 * </tr>
 * <tr>
 * <td>column</td>
 * <td>TableColumn</td>
 * <td>The associated column object.</td>
 * </tr>
 * <tr>
 * <td>first</td>
 * <td>boolean</td>
 * <td>Indicates if the cell is the first in the row.</td>
 * </tr>
 * <tr>
 * <td>id</td>
 * <td>String</td>
 * <td>The cell ID.</td>
 * </tr>
 * <tr>
 * <td>index</td>
 * <td>int</td>
 * <td>The cell's index within its row.</td>
 * </tr>
 * <tr>
 * <td>last</td>
 * <td>boolean</td>
 * <td>Indicates if the cell is the last in the row.</td>
 * </tr>
 * <tr>
 * <td>row</td>
 * <td>TableRow</td>
 * <td>The associated row object for the cell.</td>
 * </tr>
 * <tr>
 * <td>value</td>
 * <td>Object</td>
 * <td>The raw value of the cell, directly from the data source.</td>
 * </tr>
 * </table>
 *
 * <p>
 * <b>TableRow Properties:</b>
 * </p>
 * <table border="1">
 * <tr>
 * <th>Property</th>
 * <th>Type</th>
 * <th>Description</th>
 * </tr>
 * <tr>
 * <td>cells</td>
 * <td>TableCell[]</td>
 * <td>The cells within the row.</td>
 * </tr>
 * <tr>
 * <td>data</td>
 * <td>Object</td>
 * <td>The data provided by the application for the row.</td>
 * </tr>
 * <tr>
 * <td>even</td>
 * <td>boolean</td>
 * <td>Indicates if the row is even-numbered (for styling purposes).</td>
 * </tr>
 * <tr>
 * <td>first</td>
 * <td>boolean</td>
 * <td>Indicates if the row is the first in the table.</td>
 * </tr>
 * <tr>
 * <td>id</td>
 * <td>String</td>
 * <td>Unique ID for the row.</td>
 * </tr>
 * <tr>
 * <td>index</td>
 * <td>int</td>
 * <td>The row index.</td>
 * </tr>
 * <tr>
 * <td>last</td>
 * <td>boolean</td>
 * <td>Indicates if the row is the last in the table.</td>
 * </tr>
 * <tr>
 * <td>odd</td>
 * <td>boolean</td>
 * <td>Indicates if the row is odd-numbered (for styling purposes).</td>
 * </tr>
 * </table>
 *
 * <p>
 * <b>TableColumn Properties:</b>
 * </p>
 * <table border="1">
 * <tr>
 * <th>Property</th>
 * <th>Type</th>
 * <th>Description</th>
 * </tr>
 * <tr>
 * <td>align</td>
 * <td>ColumnAlignment</td>
 * <td>The alignment of the column (left, center, right).</td>
 * </tr>
 * <tr>
 * <td>id</td>
 * <td>String</td>
 * <td>The field of the row object to get the cell's data from.</td>
 * </tr>
 * <tr>
 * <td>label</td>
 * <td>String</td>
 * <td>The name to render in the column header.</td>
 * </tr>
 * <tr>
 * <td>pinned</td>
 * <td>ColumnPinDirection</td>
 * <td>The pin direction of the column (left, right, auto).</td>
 * </tr>
 * <tr>
 * <td>sortable</td>
 * <td>boolean</td>
 * <td>If true, the column can be sorted.</td>
 * </tr>
 * <tr>
 * <td>sort</td>
 * <td>SortDirection</td>
 * <td>The sort order of the column.</td>
 * </tr>
 * <tr>
 * <td>type</td>
 * <td>ColumnType</td>
 * <td>The type of the column (text, number, boolean, etc.).</td>
 * </tr>
 * <tr>
 * <td>minWidth</td>
 * <td>number</td>
 * <td>The minimum width of the column in pixels.</td>
 * </tr>
 * </table>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public abstract class Renderer<T> {
  private final String key = UUID.randomUUID().toString().substring(0, 8);
  private final EventDispatcher eventDispatcher = new EventDispatcher();
  private final Map<String, String> attributes = new HashMap<>();
  private Table<T> table;

  /**
   * Sets an attribute on the rendered element.
   *
   * @param name the attribute name
   * @param value the attribute value
   * @param fireChangeEvent whether to notify listeners of the change
   * @return this renderer
   */
  public Renderer<T> setAttribute(String name, Object value, boolean fireChangeEvent) {
    attributes.put(name, toAttributeValue(value));
    if (fireChangeEvent) {
      fireChangeEvent();
    }

    return this;
  }

  /**
   * Sets an attribute on the rendered element.
   *
   * @param name the attribute name
   * @param value the attribute value
   * @return this renderer
   */
  public Renderer<T> setAttribute(String name, Object value) {
    return setAttribute(name, value, true);
  }

  /**
   * Returns the value of an attribute.
   *
   * @param name the attribute name
   * @return the attribute value
   */
  public String getAttribute(String name) {
    return attributes.get(name);
  }

  /**
   * Removes an attribute from the rendered element.
   *
   * @param name the attribute name
   * @return this renderer
   */
  public Renderer<T> removeAttribute(String name) {
    attributes.remove(name);
    fireChangeEvent();

    return this;
  }

  /**
   * Adds part names to the {@code part} attribute.
   *
   * @param partNames the part names to add
   * @return this renderer
   */
  public Renderer<T> addPart(String... partNames) {
    String parts = getAttribute("part"); // NOSONAR
    if (parts == null) {
      parts = String.join(" ", partNames);
    } else {
      parts += " " + String.join(" ", partNames);
    }

    setAttribute("part", parts);
    return this;
  }

  /**
   * Removes part names from the {@code part} attribute.
   *
   * @param partNames the part names to remove
   * @return this renderer
   */
  public Renderer<T> removePart(String... partNames) {
    String parts = getAttribute("part");
    if (parts != null) {
      List<String> partsList = Arrays.asList(partNames);
      parts = Arrays.stream(parts.split(" ")).filter(c -> !partsList.contains(c))
          .collect(Collectors.joining(" "));
      if (parts.isEmpty()) {
        removeAttribute("part");
      } else {
        setAttribute("part", parts);
      }
    }

    return this;
  }

  /**
   * Returns the unique key identifying this renderer instance.
   *
   * @return the renderer key
   */
  public final String getKey() {
    return key;
  }

  /**
   * Returns the table this renderer is attached to.
   *
   * @return the table, or {@code null} if not yet attached
   */
  public final Table<T> getTable() {
    return table;
  }

  /**
   * Sets the table this renderer is attached to. Can only be called once.
   *
   * @param table the table
   * @throws IllegalStateException if the table is already set
   */
  public final void setTable(Table<T> table) {
    if (this.table != null) {
      throw new IllegalStateException(
          "Once a renderer is associated with a table, it cannot be changed");
    }

    Objects.requireNonNull(table);
    this.table = table;
    onAttach();
  }

  /**
   * Adds a change listener to the renderer.
   *
   * @param listener the change listener
   * @return the listener registration
   */
  public ListenerRegistration<RendererChangeEvent> addChangeListener(
      EventListener<RendererChangeEvent> listener) {
    return eventDispatcher.addListener(RendererChangeEvent.class, listener);
  }

  /**
   * Alias for {@link #addChangeListener(EventListener)}.
   *
   * @param listener the listener
   * @return the listener registration
   */
  public ListenerRegistration<RendererChangeEvent> onChanged(
      EventListener<RendererChangeEvent> listener) {
    return addChangeListener(listener);
  }

  /**
   * Builds the client-side template for rendering cells.
   *
   * @return the template string
   */
  public abstract String build();

  /**
   * Called when the renderer is attached to a table.
   */
  protected void onAttach() {}

  /**
   * Returns the event dispatcher.
   *
   * @return the event dispatcher
   */
  protected EventDispatcher getEventDispatcher() {
    return eventDispatcher;
  }

  /**
   * Fires a change event to notify listeners that the renderer configuration has changed.
   */
  protected void fireChangeEvent() {
    getEventDispatcher().dispatchEvent(new RendererChangeEvent(this));
  }

  /**
   * Converts a value to its string representation for use in HTML attributes. Handles enums,
   * strings, and other objects via JSON serialization.
   *
   * @param value the value to convert
   * @return the string representation
   */
  protected static String toAttributeValue(Object value) {
    Gson gson = new Gson();
    return gson.fromJson(gson.toJson(value), String.class);
  }

  /**
   * Returns all attributes formatted as an HTML attribute string.
   *
   * @return the attributes string (e.g., {@code "name='value' theme='primary'"})
   */
  protected String getAttributesAsString() {
    return attributes.entrySet().stream()
        .map(entry -> String.format("%s='%s'", entry.getKey(), entry.getValue()))
        .collect(Collectors.joining(" "));
  }
}
