package org.dwcj.widgets.googlecharts.events;

import java.util.Map;

import org.dwcj.widgets.googlecharts.GoogleChart;
import org.dwcj.webcomponent.annotations.EventExpressions;
import org.dwcj.webcomponent.annotations.EventName;
import org.dwcj.webcomponent.events.Event;

/**
 * Most charts are rendered asynchronously; all Google charts throw a ready
 * event after they have finished rendering.
 * 
 * @author Hyyan Abo Fakher
 */
@EventName(value = "google-chart-ready")
@EventExpressions(detail = "event.detail = 'ready';")
public final class GoogleChartReadyEvent extends Event<GoogleChart> {

  /**
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public GoogleChartReadyEvent(GoogleChart target, Map<String, Object> detail) {
    super(target, detail);
  }
}
