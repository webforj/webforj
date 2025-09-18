package com.webforj.component.webswing.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.webswing.WebswingConnector;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Map;
import java.util.Optional;

/**
 * Event fired when a custom action is triggered from the Webswing application.
 *
 * <p>
 * The event detail contains the action name, optional data payload, and optional binary data
 * encoded as Base64.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
@EventName("dwc-webswing-action")
@EventOptions(data = {@EventData(key = "actionName", exp = "event.detail.actionName"),
    @EventData(key = "data", exp = "event.detail.data"),
    @EventData(key = "binaryDataBase64", exp = "event.detail.binaryDataBase64")})
public class WebswingActionEvent extends ComponentEvent<WebswingConnector> {
  private final String actionName;
  private final String data;
  private final String binaryData;

  /**
   * Creates a new Webswing action event.
   *
   * @param component the source component
   * @param eventMap the event data map
   */
  public WebswingActionEvent(WebswingConnector component, Map<String, Object> eventMap) {
    super(component, eventMap);
    this.actionName = (String) eventMap.get("actionName");
    this.data = (String) eventMap.get("data");
    this.binaryData = (String) eventMap.get("binaryDataBase64");
  }

  /**
   * Gets the name of the action that was triggered.
   *
   * @return the action name
   */
  public String getActionName() {
    return actionName;
  }

  /**
   * Gets the optional data payload sent with the action.
   *
   * @return the data payload, or null if none was sent
   */
  public Optional<String> getActionData() {
    return Optional.ofNullable(data);
  }

  /**
   * Gets the binary data.
   *
   * @return the binary data
   */
  public Optional<String> getActionBinaryData() {
    if (binaryData != null) {
      return Optional
          .of(new String(Base64.getDecoder().decode(binaryData.getBytes(StandardCharsets.UTF_8)),
              StandardCharsets.UTF_8));
    }

    return Optional.empty();
  }
}
