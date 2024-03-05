package com.webforj.component.googlecharts;

import com.google.gson.annotations.SerializedName;
import com.webforj.annotation.Attribute;
import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.annotation.JavaScript;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.googlecharts.events.GoogleChartReadyEvent;
import com.webforj.component.googlecharts.events.GoogleChartSelectedEvent;
import com.webforj.concern.HasStyle;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * A simple implementation for Google Chart Web Component, this component is a wrapper for the
 * <a href=
 * "https://github.com/GoogleWebComponents/google-chart">GoogleWebComponents/google-chart</a>.
 *
 * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery">Google Chart
 *      Gallery</a>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@NodeName("google-chart")
@JavaScript(value = "https://cdn.jsdelivr.net/npm/@google-web-components/google-chart@5.0.3/+esm",
    attributes = {@Attribute(name = "type", value = "module")})
public final class GoogleChart extends ElementComposite implements HasStyle<GoogleChart> {

  /**
   * The type of the chart.
   *
   * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery">Google Chart
   *      Gallery</a>
   */
  public enum Type {
    /**
     * An area chart.
     *
     * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery/areachart">Area
     *      Chart</a>
     **/
    @SerializedName("area")
    AREA,
    /**
     * A bar chart.
     *
     * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery/barchart">Bar
     *      Chart</a>
     **/
    @SerializedName("bar")
    BAR,
    /**
     * A bubble chart.
     *
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/bubblechart">Bubble
     *      Chart</a>
     **/
    @SerializedName("bubble")
    BUBBLE,
    /**
     * A calendar chart.
     *
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/calendar">Calendar
     *      Chart</a>
     **/
    @SerializedName("calendar")
    CALENDAR,
    /**
     * A candlestick chart.
     *
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/candlestickchart">Candlestick
     *      Chart</a>
     **/
    @SerializedName("candlestick")
    CANDLESTICK,
    /**
     * A column chart.
     *
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/columnchart">Column
     *      Chart</a>
     **/
    @SerializedName("column")
    COLUMN,
    /**
     * A combo chart.
     *
     * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery/combochart">Combo
     *      Chart</a>
     **/
    @SerializedName("combo")
    COMBO,
    /**
     * A gantt chart.
     *
     * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery/ganttchart">Gantt
     *      Chart</a>
     **/
    @SerializedName("gantt")
    GANTT,
    /**
     * A gauge chart.
     *
     * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery/gauge">Gauge
     *      Chart</a>
     **/
    @SerializedName("gauge")
    GAUGE,
    /**
     * A geo chart.
     *
     * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery/geochart">Geo
     *      Chart</a>
     **/
    @SerializedName("geo")
    GEO,
    /**
     * A histogram chart.
     *
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/histogram">Histogram
     *      Chart</a>
     **/
    @SerializedName("histogram")
    HISTOGRAM,
    /**
     * A line chart.
     *
     * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery/linechart">Line
     *      Chart</a>
     **/
    @SerializedName("line")
    LINE,
    /**
     * A map chart.
     *
     * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery/map">Map
     *      Chart</a>
     **/
    @SerializedName("org")
    ORG,
    /**
     * A pie chart.
     *
     * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery/piechart">Pie
     *      Chart</a>
     **/
    @SerializedName("pie")
    PIE,
    /**
     * A scatter chart.
     *
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/scatterchart">Scatter
     *      Chart</a>
     **/
    @SerializedName("sankey")
    SANKEY,
    /**
     * A scatter chart.
     *
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/scatterchart">Scatter
     *      Chart</a>
     **/
    @SerializedName("scatter")
    SCATTER,
    /**
     * A stepped area chart.
     *
     * @see <a href=
     *      "https://developers.google.com/chart/interactive/docs/gallery/steppedareachart">Stepped
     *      Area Chart</a>
     **/
    @SerializedName("stepped-area")
    STEPPED_AREA,
    /**
     * A table chart.
     *
     * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery/table">Table
     *      Chart</a>
     **/
    @SerializedName("table")
    TABLE,
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
    @SerializedName("timeline")
    TIMELINE,
    /**
     * A treemap chart.
     *
     * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery/treemap">Treemap
     *      Chart</a>
     **/
    @SerializedName("treemap")
    TREEMAP,
    /**
     * A word tree chart.
     *
     * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery/wordtree">Word
     *      Tree Chart</a>
     **/
    @SerializedName("wordtree")
    WORDTREE;
  }

  // Properties
  private final PropertyDescriptor<Type> typeProp = PropertyDescriptor.property("type", Type.BAR);
  private final PropertyDescriptor<List<Object>> dataProp =
      PropertyDescriptor.property("data", new ArrayList<>());
  private final PropertyDescriptor<Map<String, Object>> optionProp =
      PropertyDescriptor.property("options", new HashMap<>());
  private final PropertyDescriptor<List<Object>> selectionProp =
      PropertyDescriptor.property("selection", new ArrayList<>());

  private final ListenerRegistration<GoogleChartReadyEvent> firstRenderListenerRegistration;

  /**
   * Create a new GoogleChart.
   */
  public GoogleChart() {
    setStyle("overflow", "hidden");
    this.firstRenderListenerRegistration = addReadyListener(this::handleFirstRender);
  }

  /**
   * Create a new GoogleChart.
   *
   * @param type The type of chart to display.
   *
   * @see Type
   * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery">Google
   *      Visualization API reference (Chart Gallery)</a>
   */
  public GoogleChart(Type type) {
    this();
    setType(type);
  }

  /**
   * Set the type of chart to display.
   *
   * @param type The type of chart to display.
   * @return the component itself
   *
   * @see Type
   * @see <a href= "https://developers.google.com/chart/interactive/docs/gallery">Google
   *      Visualization API reference (Chart Gallery)</a>
   */
  public GoogleChart setType(Type type) {
    set(typeProp, type);
    return this;
  }

  /**
   * Get the type of chart to display.
   *
   * @return The type of chart to display.
   * @see Type
   */
  public Type getType() {
    return get(typeProp);
  }

  /**
   * Set the options to use when displaying the chart.
   *
   * <pre>
   * {@code
   * Map<String, Object> options = new JsonArray();
   *
   * // update the title
   * options.put("title", "My Chart Title");
   *
   * // update the colors
   * List<String> colors = new ArrayList();
   * colors.add("#e0440e");
   * colors.add("#e6693e");
   * colors.add("#ec8f6e");
   * colors.add("#f3b49f");
   * colors.add("#f6c7b6");
   * options.put("colors", colors);
   *
   * chart.setOptions(options);
   * }
   * </pre>
   *
   * <p>
   * For more information on the options available, see the
   * <a href="https://developers.google.com/chart/interactive/docs/gallery">Google Visualization API
   * reference (Chart Gallery)</a>.
   * </p>
   *
   * <p>
   * <b>Note</b> that Updating the options will not cause the chart to be redrawn. You need to call
   * {@link #setOptions(Map)} to redraw the chart.
   * </p>
   *
   * @param options The options to use when displaying the chart.
   * @return the component itself
   */
  public GoogleChart setOptions(Map<String, Object> options) {
    set(optionProp, options);
    return this;
  }

  /**
   * Get the options to use when displaying the chart.
   *
   * @return The options to use when displaying the chart.
   */
  public Map<String, Object> getOptions() {
    return get(optionProp);
  }

  /**
   * Sets the entire dataset for the chart.
   *
   * <p>
   * For instance, to set the data for a bar chart:
   * </p>
   *
   * <pre>
   * {@code
   * List<Object> data = new ArrayList();
   *
   * // add the column ArrayList
   * List<String> header = new ArrayList();
   * header.add("Year");
   * header.add("Sales");
   * header.add("Expenses");
   * data.add(header);
   *
   * // add rows
   * for (int i = 0; i < 10; i++) {
   *   List<Number> row = new ArrayList();
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
   * @return the component itself
   */
  public GoogleChart setData(List<Object> data) {
    set(dataProp, data);
    return this;
  }

  /**
   * Get the data to use when displaying the chart.
   *
   * @return The data to use when displaying the chart.
   */
  public List<Object> getData() {
    return get(dataProp);
  }

  /**
   * Sets the selected datapoint(s) in the chart.
   *
   * <p>
   * An array of objects, each with a numeric row and/or column property. {@code row} and
   * {@code column} are the zero-based row or column number of an item in the data table to select.
   * </p>
   *
   * <ul>
   * <li>To select a whole column, set row to null</li>
   * <li>To select a whole row, set column to null.</li>
   * </ul>
   *
   * <p>
   * For example, to select the first column, set selection to.
   * </p>
   *
   * <pre>
   * {@code
   * List<Map<String, Integer>> selection = new ArrayList();
   * Map<String, Integer> item = new HashMap();
   * item.put("row", 1);
   * item.put("column", 1);
   * selection.add(item);
   *
   * chart.setSelection(selection);
   * }
   * </pre>
   *
   * @param selection The selected items in the chart.
   * @return the component itself
   */
  public GoogleChart setSelection(List<Object> selection) {
    set(selectionProp, selection);
    return this;
  }

  /**
   * Get the selected items in the chart.
   *
   * @return The selected items in the chart.
   */
  public List<Object> getSelection() {
    return get(selectionProp, true, List.class);
  }

  /**
   * Redraw the chart.
   *
   * <p>
   * Called automatically when data/type/selection/options are changed. Call manually to handle view
   * updates, page resizes, etc.
   * </p>
   *
   * @return the component itself
   */
  public GoogleChart redraw() {
    whenAttached().thenAccept(gc -> getElement().callJsFunctionAsync("redraw"));
    return this;
  }

  /**
   * Returns the chart serialized as an image URI.
   *
   * <p>
   * Call this after the chart is drawn ({@code ready} event).
   * </p>
   *
   * @return The URI of the image of the chart.
   */
  public String getImageUri() {
    Optional<String> imageUri =
        Optional.ofNullable(getElement().getProperty("imageURI", String.class));
    return imageUri.isPresent() ? imageUri.get()
        : "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public GoogleChart setStyle(String property, String value) {
    getElement().setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public GoogleChart removeStyle(String property) {
    getElement().removeStyle(property);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public String getStyle(String property) {
    return getElement().getStyle(property);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public String getComputedStyle(String property) {
    return getElement().getComputedStyle(property);
  }

  /**
   * Add a listener for the selected event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<GoogleChartSelectedEvent> addSelectedListener(
      EventListener<GoogleChartSelectedEvent> listener) {
    return addEventListener(GoogleChartSelectedEvent.class, listener);
  }

  /**
   * Alias for {@link #addSelectedListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<GoogleChartSelectedEvent> onSelect(
      EventListener<GoogleChartSelectedEvent> listener) {
    return addSelectedListener(listener);
  }

  /**
   * Add a listener for the ready event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<GoogleChartReadyEvent> addReadyListener(
      EventListener<GoogleChartReadyEvent> listener) {
    return addEventListener(GoogleChartReadyEvent.class, listener);
  }

  /**
   * Alias for {@link #addReadyListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<GoogleChartReadyEvent> onReady(
      EventListener<GoogleChartReadyEvent> listener) {
    return addReadyListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onDestroy() {
    super.onDestroy();

    // clean the window resize listener
    StringBuilder sb = new StringBuilder();
    sb.append("if (component && component.__dwcj_handleResize__) {");
    sb.append("  window.removeEventListener('resize', component.__dwcj_handleResize__);");
    sb.append("}");

    getElement().executeJsAsync(sb.toString());
  }

  private void handleFirstRender(GoogleChartReadyEvent event) {
    // add a window resize listener
    StringBuilder sb = new StringBuilder();
    sb.append("if (component && component.__dwcj_handleResize__) {");
    sb.append("  window.removeEventListener('resize', component.__dwcj_handleResize__);");
    sb.append("}");
    sb.append("component.__dwcj_handleResize__ = () => component.redraw();");
    sb.append("window.addEventListener('resize', component.__dwcj_handleResize__);");
    getElement().executeJsAsync(sb.toString());

    this.firstRenderListenerRegistration.remove();
    getElement().executeJsAsync("requestAnimationFrame(() => component.redraw());");
  }

  Element getOriginalElement() {
    return getElement();
  }
}
