package com.webforj.component.slider.sink;

import com.basis.bbj.proxies.event.BBjControlScrollEvent;
import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.slider.Slider;
import com.webforj.component.slider.event.SliderSlideEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;

/**
 * This class will map the BBjControlScrollEvent event to a {@link SliderSlideEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public final class SliderSlideEventSink extends AbstractDwcEventSink {

  /**
   * Creates a new instance of SliderScrollEventSink.
   *
   * @param component The component which will handle the event.
   * @param dispatcher The event dispatcher.
   */
  public SliderSlideEventSink(Slider component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_CONTROL_SCROLL);
  }

  /**
   * Handles the BBj event and dispatches a new {@link SliderSlideEvent}.
   *
   * @param ev A BBj button push event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjControlScrollEvent event = (BBjControlScrollEvent) ev;
    HashMap<String, Object> map = new HashMap<>();

    map.put("value", event.getPosition());
    map.put("adjusting", event.isAdjusting());

    SliderSlideEvent webforjEv = new SliderSlideEvent((Slider) getComponent(), map);
    getEventDispatcher().dispatchEvent(webforjEv);
  }
}
