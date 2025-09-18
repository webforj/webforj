package com.webforj.component.webswing.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.webswing.WebswingConnector;
import java.util.Map;

/**
 * Event fired when the Webswing application has been started and is actively running.
 *
 * <p>
 * This occurs after calling the {@code start()} method or if {@code autoStart} was set to true in
 * options.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
@EventName("dwc-webswing-started")
public class WebswingStartEvent extends ComponentEvent<WebswingConnector> {

  /**
   * Creates a new Webswing start event.
   *
   * @param component the source component
   */
  public WebswingStartEvent(WebswingConnector component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
