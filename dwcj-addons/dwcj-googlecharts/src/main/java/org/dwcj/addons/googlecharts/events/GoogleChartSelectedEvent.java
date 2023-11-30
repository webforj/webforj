package org.dwcj.addons.googlecharts.events;

import java.util.List;
import java.util.Map;
import org.dwcj.addons.googlecharts.GoogleChart;
import org.dwcj.component.element.annotation.EventName;
import org.dwcj.component.element.annotation.EventOptions;
import org.dwcj.component.event.ComponentEvent;

/**
 * Emitted when data is selected in the chart.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@EventName(value = "google-chart-select")
@EventOptions(map = {
    @EventOptions.EventMapItem(key = "selection", exp = "event.detail.chart.getSelection();")})
public final class GoogleChartSelectedEvent extends ComponentEvent<GoogleChart> {

  /**
   * Construct new GoogleChartSelectedEvent.
   *
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
  public List<Object> getSelection() {
    @SuppressWarnings("unchecked")
    List<Object> selection = (List<Object>) getData().get("selection");

    return selection;
  }

}
