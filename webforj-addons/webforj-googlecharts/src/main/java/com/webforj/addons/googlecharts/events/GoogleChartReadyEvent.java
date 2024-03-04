package com.webforj.addons.googlecharts.events;

import com.webforj.addons.googlecharts.GoogleChart;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.event.ComponentEvent;
import java.util.Map;

/**
 * Most charts are rendered asynchronously; all Google charts throw a ready event after they have
 * finished rendering.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@EventName(value = "google-chart-ready")
public final class GoogleChartReadyEvent extends ComponentEvent<GoogleChart> {

  /**
   * Construct new GoogleChartReadyEvent.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public GoogleChartReadyEvent(GoogleChart target, Map<String, Object> detail) {
    super(target, detail);
  }
}
