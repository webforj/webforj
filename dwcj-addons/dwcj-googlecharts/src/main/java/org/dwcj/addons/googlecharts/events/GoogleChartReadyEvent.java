package org.dwcj.addons.googlecharts.events;

import java.util.Map;
import org.dwcj.addons.googlecharts.GoogleChart;
import org.dwcj.component.events.Event;
import org.dwcj.component.webcomponent.annotations.EventExpressions;
import org.dwcj.component.webcomponent.annotations.EventName;

/**
 * Most charts are rendered asynchronously; all Google charts throw a ready event after they have
 * finished rendering.
 *
 * @author Hyyan Abo Fakher
 */
@EventName(value = "google-chart-ready")
@EventExpressions(detail = "event.detail = 'ready';")
public final class GoogleChartReadyEvent extends Event<GoogleChart> {

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
