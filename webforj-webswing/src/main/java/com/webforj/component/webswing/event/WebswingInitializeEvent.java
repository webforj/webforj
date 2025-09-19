package com.webforj.component.webswing.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.webswing.WebswingConnector;
import java.util.Map;

/**
 * Event fired when the Webswing instance has been successfully initialized and is ready to receive
 * commands.
 *
 * <p>
 * The instance is bootstrapped but may not be started yet if {@code autoStart} is false.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
@EventName("dwc-webswing-initialized")
public class WebswingInitializeEvent extends ComponentEvent<WebswingConnector> {

  /**
   * Creates a new webswing initialized event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  public WebswingInitializeEvent(WebswingConnector component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
