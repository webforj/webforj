package org.dwcj.widgets.googlecharts;

import org.dwcj.annotations.Attribute;
import org.dwcj.annotations.JavaScript;
import org.dwcj.annotations.Link;
import org.dwcj.widgets.googlecharts.events.GoogleChartReadyEvent;
import org.dwcj.widgets.googlecharts.events.GoogleChartSelectedEvent;
import org.dwcj.interfaces.HasStyle;
import org.dwcj.webcomponent.PropertyDescriptor;
import org.dwcj.webcomponent.WebComponent;
import org.dwcj.webcomponent.annotations.NodeName;
import org.dwcj.webcomponent.events.EventListener;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

/**
 * A simple implementation for Google Chart Web Component, this component is a
 * wrapper for the <a href=
 * "https://github.com/GoogleWebComponents/google-chart">GoogleWebComponents/google-chart</a>
 * 
 * @see <a href=
 *      "https://developers.google.com/chart/interactive/docs/gallery">Google
 *      Chart Gallery</a>
 * 
 * @author Hyyan Abo Fakher
 */
@NodeName("google-chart")
@Link(value = "https://www.gstatic.com", id = "gstatic-preconnect", attributes = {
    @Attribute(name = "rel", value = "preconnect"),
    @Attribute(name = "crossorigin", value = "")
})
@Link(value = "https://www.gstatic.com", id = "gstatic-dns-prefetch", attributes = {
    @Attribute(name = "rel", value = "dns-prefetch")
})
@JavaScript(value = "https://cdn.jsdelivr.net/npm/@google-web-components/google-chart@5.0.3/+esm", attributes = {
    @Attribute(name = "type", value = "module")
})
public final class GoogleChart extends WebComponent implements HasStyle {

  /**
   * The type of the chart.
   * 
   * @see <a href=
   *      "https://developers.google.com/chart/interactive/docs/gallery">Google
   *      Chart Gallery</a>
   */
  public enum Type {
    /**
     * An area chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/areachart">Area
     *      Chart</a>
     **/
    AREA("area"),
    /**
     * A bar chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/barchart">Bar
     *      Chart</a>
     **/
    BAR("bar"),
    /**
     * A bubble chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/bubblechart">Bubble
     *      Chart</a>
     **/
    BUBBLE("bubble"),
    /**
     * A calendar chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/calendar">Calendar
     *      Chart</a>
     **/
    CALENDAR("calendar"),
    /**
     * A candlestick chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/candlestickchart">Candlestick
     *      Chart</a>
     **/
    CANDLESTICK("candlestick"),
    /**
     * A column chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/columnchart">Column
     *      Chart</a>
     **/
    COLUMN("column"),
    /**
     * A combo chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/combochart">Combo
     *      Chart</a>
     **/
    COMBO("combo"),
    /**
     * A gantt chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/ganttchart">Gantt
     *      Chart</a>
     **/
    GANTT("gantt"),
    /**
     * A gauge chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/gauge">Gauge
     *      Chart</a>
     **/
    GAUGE("gauge"),
    /**
     * A geo chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/geochart">Geo
     *      Chart</a>
     **/
    GEO("geo"),
    /**
     * A histogram chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/histogram">Histogram
     *      Chart</a>
     **/
    HISTOGRAM("histogram"),
    /**
     * A line chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/linechart">Line
     *      Chart</a>
     **/
    LINE("line"),
    /**
     * A map chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/map">Map
     *      Chart</a>
     **/
    ORG("org"),
    /**
     * A pie chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/piechart">Pie
     *      Chart</a>
     **/
    PIE("pie"),
    /**
     * A scatter chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/scatterchart">Scatter
     *      Chart</a>
     **/
    SANKEY("sankey"),
    /**
     * A scatter chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/scatterchart">Scatter
     *      Chart</a>
     **/
    SCATTER("scatter"),
    /**
     * A stepped area chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/steppedareachart">Stepped
     *      Area Chart</a>
     **/
    STEPPED_AREA("stepped-area"),
    /**
     * A table chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/table">Table
     *      Chart</a>
     **/
    TABLE("table"),
    /**
     * A timeline chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/timeline">Timeline
     *      Chart</a>
     * @see <a
     *      href=https://developers.google.com/chart/interactive/docs/datesandtimes#dates-and-times-using-the-date-string-representation>Timeline
     *      Chart Date Format</a>
     **/
    TIMELINE("timeline"),
    /**
     * A treemap chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/treemap">Treemap
     *      Chart</a>
     **/
    TREEMAP("treemap"),
    /**
     * A word tree chart.
     * 
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/wordtree">Word
     *      Tree Chart</a>
     **/
    WORDTREE("wordtree");

    private final String chartType;

    Type(String type) {
      this.chartType = type;
    }

    /**
     * Get the value of the type.
     * 
     * @return The value of the type.
     */
    public String getValue() {
      return chartType;
    }

    /**
     * Get the type from the value.
     * 
     * @param type The value of the type.
     * @return The type.
     */
    public static Type fromValue(String type) {
      for (Type t : Type.values()) {
        if (t.chartType.equalsIgnoreCase(type)) {
          return t;
        }
      }

      return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
      return chartType;
    }
  }

  // Properties
  private final PropertyDescriptor<String> TYPE = PropertyDescriptor.property("type", Type.BAR.getValue());
  private final PropertyDescriptor<JsonArray> DATA = PropertyDescriptor.property("data", null);
  private final PropertyDescriptor<JsonObject> OPTIONS = PropertyDescriptor.property("options", null);
  private final PropertyDescriptor<JsonArray> SELECTION = PropertyDescriptor.property("selection", null);

  private EventListener<GoogleChartReadyEvent> firstRenderListener;

  /**
   * Create a new GoogleChart.
   */
  public GoogleChart() {
    setStyle("overflow", "hidden");

    this.firstRenderListener = this::handleFirstRender;
    addReadyListener(this.firstRenderListener);
  }

  /**
   * Create a new GoogleChart.
   * 
   * @param type The type of chart to display.
   * 
   * @see Type
   * @see <a href=
   *      "https://developers.google.com/chart/interactive/docs/gallery">oogle
   *      Visualization API reference (Chart Gallery)</a>
   */
  public GoogleChart(Type type) {
    setType(type);
  }

  /**
   * Set the type of chart to display.
   * 
   * @param type The type of chart to display.
   * @return The chart
   * 
   * @see Type
   * @see <a href=
   *      "https://developers.google.com/chart/interactive/docs/gallery">oogle
   *      Visualization API reference (Chart Gallery)</a>
   */
  public GoogleChart setType(Type type) {
    set(TYPE, type.getValue());
    return this;
  }

  /**
   * Get the type of chart to display.
   * 
   * @return The type of chart to display.
   * @see Type
   */
  public Type getType() {
    return Type.fromValue(get(TYPE));
  }

  /**
   * Set the options to use when displaying the chart.
   * 
   * <p>
   * 
   * <pre>
   * {@code
   * JsonArray options = new JsonArray();
   * 
   * // update the title
   * options.addProperty("title", "My Chart Title");
   * 
   * // update the colors
   * JsonArr colors = new JsonArray();
   * colors.add("#e0440e");
   * colors.add("#e6693e");
   * colors.add("#ec8f6e");
   * colors.add("#f3b49f");
   * colors.add("#f6c7b6");
   * options.add("colors", colors);
   * 
   * chart.setOptions(options);
   * }
   * </pre>
   * 
   * For more information on the options available, see the
   * <a href="https://developers.google.com/chart/interactive/docs/gallery">Google
   * Visualization API reference (Chart Gallery)</a>.
   * </p>
   * 
   * <p>
   * Note that the options are passed as a JSON array, not a JSON object. This is
   * because the options are passed to the Google Visualization API as a
   * JavaScript array, not a JavaScript object.
   * </p>
   * 
   * <p>
   * Note that Updating the options JsonArray will not cause the chart to be
   * redrawn.
   * You need to call {@link #setOptions(JsonObject)} again to update the chart.
   * </p>
   * 
   * @param options The options to use when displaying the chart.
   * @return The chart
   */
  public GoogleChart setOptions(JsonObject options) {
    set(OPTIONS, options);
    return this;
  }

  /**
   * Get the options to use when displaying the chart.
   * 
   * @return The options to use when displaying the chart.
   */
  public JsonObject getOptions() {
    return get(OPTIONS);
  }

  /**
   * Sets the entire dataset for the chart.
   * 
   * For instance, to set the data for a bar chart:
   * 
   * <pre>
   * {@code
   * JsonArray data = new JsonArray();
   * 
   * // add the column headers
   * JsonArray header = new JsonArray();
   * header.add("Year");
   * header.add("Sales");
   * header.add("Expenses");
   * data.add(header);
   * 
   * // add rows
   * for (int i = 0; i < 10; i++) {
   *   JsonArray row = new JsonArray();
   *   row.add(2000 + i);
   *   row.add(Math.random() * 1000);
   *   row.add(Math.random() * 400);
   *   data.add(row);
   * }
   * 
   * chart.setData(data);
   * }
   * </pre>
   * 
   * @param data The data to use when displaying the chart.
   * @return The chart
   */
  public GoogleChart setData(JsonArray data) {
    set(DATA, data);
    return this;
  }

  /**
   * Get the data to use when displaying the chart.
   * 
   * @return The data to use when displaying the chart.
   */
  public JsonArray getData() {
    return get(DATA);
  }

  /**
   * Sets the selected datapoint(s) in the chart.
   * 
   * <p>
   * An array of objects, each with a numeric row and/or column property.
   * `row` and `column` are the zero-based row or column number of an item
   * in the data table to select.
   * </p>
   * 
   * <ul>
   * <li>To select a whole column, set row to null</li>
   * <li>To select a whole row, set column to null.</li>
   * </ul>
   * 
   * For example, to select the first column, set `selection` to
   * 
   * <pre>
   * {@code
   * JsonArray selection = new JsonArray();
   * JsonObject item = new JsonObject();
   * item.addProperty("row", 1);
   * item.addProperty("column", 1);
   * selection.add(item);
   * 
   * chart.setSelection(selection);
   * }
   * </pre>
   * 
   * @param selection The selected items in the chart.
   * @return The chart
   */
  public GoogleChart setSelection(JsonArray selection) {
    set(SELECTION, selection);
    return this;
  }

  /**
   * Get the selected items in the chart.
   * 
   * @return The selected items in the chart.
   */
  public JsonArray getSelection() {
    return get(SELECTION, true, JsonArray.class);
  }

  /**
   * Redraw the chart.
   * 
   * Called automatically when data/type/selection/options are changed.
   * Call manually to handle view updates, page resizes, etc.
   * 
   * @return The chart
   */
  public GoogleChart redraw() {
    callAsyncFunction("redraw");
    return this;
  }

  /**
   * Returns the chart serialized as an image URI.
   * 
   * Call this after the chart is drawn (`ready` event).
   * 
   * @return The URI of the image of the chart.
   */
  public String getImageURI() {
    return (String) getComponentProperty("imageURI",
        "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==", true);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public GoogleChart setStyle(String property, String value) {
    setComponentStyle(property, value);
    return this;
  }
 
  /**
   * {@inheritDoc}
   */
  @Override
  public GoogleChart removeStyle(String property) {
    removeComponentStyle(property);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getStyle(String property) {
    return getComponentStyle(property);
  }
    
  /**
   * {@inheritDoc}
   */
  @Override
  public String getComputedStyle(String property) {
    return getComponentComputedStyle(property);
  }

  /**
   * Add a listener for the selected event.
   * 
   * @param listener the listener
   * @return The chart
   */
  public GoogleChart addSelectedListener(EventListener<GoogleChartSelectedEvent> listener) {
    addEventListener(GoogleChartSelectedEvent.class, listener);
    return this;
  }

  /**
   * Alias for {@link #addSelectedListener(EventListener)}
   * 
   * @param listener the listener
   * @return The chart
   */
  public GoogleChart onSelect(EventListener<GoogleChartSelectedEvent> listener) {
    return addSelectedListener(listener);
  }

  /**
   * Remove a listener for the selected event.
   * 
   * @param listener the listener
   * @return The chart
   */
  public GoogleChart removeSelectedListener(EventListener<GoogleChartSelectedEvent> listener) {
    removeEventListener(GoogleChartSelectedEvent.class, listener);
    return this;
  }

  /**
   * Add a listener for the ready event.
   * 
   * @param listener the listener
   * @return The chart
   */
  public GoogleChart addReadyListener(EventListener<GoogleChartReadyEvent> listener) {
    addEventListener(GoogleChartReadyEvent.class, listener);
    return this;
  }

  /**
   * Alias for {@link #addReadyListener(EventListener)}
   * 
   * @param listener the listener
   * @return The chart
   */
  public GoogleChart onReady(EventListener<GoogleChartReadyEvent> listener) {
    return addReadyListener(listener);
  }

  /**
   * Remove a listener for the ready event.
   * 
   * @param listener the listener
   * @return The chart
   */
  public GoogleChart removeReadyListener(EventListener<GoogleChartReadyEvent> listener) {
    removeEventListener(GoogleChartReadyEvent.class, listener);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void destroy() {
    if (!super.isDestroyed()) {
      // clean the window resize listener
      StringBuilder sb = new StringBuilder();
      sb.append("if (component && component.__dwcj_handleResize__) {");
      sb.append("  window.removeEventListener('resize', component.__dwcj_handleResize__);");
      sb.append("}");
      sb.append("return"); // to avoid auto wrapping

      executeAsyncExpression(sb.toString());
    }

    super.destroy();
  }

  private void handleFirstRender(GoogleChartReadyEvent event) {
    // add a window resize listener
    StringBuilder sb = new StringBuilder();
    sb.append("if (component && component.__dwcj_handleResize__) {");
    sb.append("  window.removeEventListener('resize', component.__dwcj_handleResize__);");
    sb.append("}");
    sb.append("component.__dwcj_handleResize__ = () => component.redraw();");
    sb.append("window.addEventListener('resize', component.__dwcj_handleResize__);");
    sb.append("return"); // to avoid auto wrapping
    executeAsyncExpression(sb.toString());

    removeReadyListener(this.firstRenderListener);
    executeAsyncExpression("requestAnimationFrame(() => component.redraw());");
  }
}
