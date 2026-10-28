package com.webforj.automation.act.userinteraction;

import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.event.ComponentEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.Map;

/** An element composite with a click event of its own, as an application writes one. */
@NodeName("x-gauge")
public final class Gauge extends ElementComposite {

  /**
   * Adds a listener for the click of the gauge.
   *
   * @param listener the listener
   *
   * @return the registration
   */
  public ListenerRegistration<GaugeClickEvent> onClick(EventListener<GaugeClickEvent> listener) {
    return addEventListener(GaugeClickEvent.class, listener);
  }

  /** The click of the gauge. */
  @EventName("click")
  public static final class GaugeClickEvent extends ComponentEvent<Gauge> {

    /**
     * Creates the event.
     *
     * @param gauge the gauge
     * @param eventMap the data of the event
     */
    public GaugeClickEvent(Gauge gauge, Map<String, Object> eventMap) {
      super(gauge, eventMap);
    }
  }
}
