package org.dwcj.widgets.googlecharts.events;

import java.util.Map;

import org.dwcj.widgets.googlecharts.GoogleChart;
import org.dwcj.webcomponent.annotations.EventExpressions;
import org.dwcj.webcomponent.annotations.EventName;
import org.dwcj.webcomponent.events.Event;

import com.google.gson.Gson;
import com.google.gson.JsonArray;

/**
 * Emitted when data is selected in the chart.
 * 
 * @author Hyyan Abo Fakher
 */
@EventName(value = "google-chart-select")
@EventExpressions(detail = "event.detail = event.detail.chart.getSelection();")
public final class GoogleChartSelectedEvent extends Event<GoogleChart> {

  /**
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public GoogleChartSelectedEvent(GoogleChart target, Map<String, Object> detail) {
    super(target, detail);
  }

  /**
   * Get the selected data.
   * 
   * @return the selected data as JsonArray
   */
  public JsonArray getSelection() {
    Map<String, Object> detail = getData();
    return new Gson().fromJson(detail.get("detail").toString(), JsonArray.class);
  }
}
