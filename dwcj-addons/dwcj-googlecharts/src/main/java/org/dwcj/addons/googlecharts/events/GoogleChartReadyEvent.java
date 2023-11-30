package org.dwcj.addons.googlecharts.events;

import java.util.Map;
import org.dwcj.addons.googlecharts.GoogleChart;
import org.dwcj.component.element.annotation.EventName;
import org.dwcj.component.event.ComponentEvent;

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
