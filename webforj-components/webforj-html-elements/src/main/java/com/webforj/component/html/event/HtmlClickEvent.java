package com.webforj.component.html.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.html.HtmlComponent;
import java.util.Map;

/**
 * Event fired when an HtmlComponent is clicked.
 *
 * @param <T> the component type
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@EventName("click")
@EventOptions(data = {@EventData(key = "screenX", exp = "event.screenX"),
    @EventData(key = "screenY", exp = "component.screenY"),
    @EventData(key = "clientX", exp = "event.clientX"),
    @EventData(key = "clientY", exp = "component.clientY"),
    @EventData(key = "detail", exp = "event.detail"),
    @EventData(key = "button", exp = "event.button"),
    @EventData(key = "ctrlKey", exp = "event.ctrlKey"),
    @EventData(key = "shiftKey", exp = "event.shiftKey"),
    @EventData(key = "altKey", exp = "event.altKey"),
    @EventData(key = "metaKey", exp = "event.metaKey")})
public class HtmlClickEvent<T extends HtmlComponent<T>> extends ComponentEvent<HtmlComponent<T>> {

  /**
   * Creates a new component event.
   *
   * @param component the source component
   * @param eventMap the event data
   */
  public HtmlClickEvent(HtmlComponent<T> component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }

  /**
   * Gets the x coordinate of the click event, relative to the upper left corner of the screen.
   *
   * @return the x coordinate, -1 if unknown
   */
  public int getScreenX() {
    return (int) getData().get("screenX");
  }

  /**
   * Gets the y coordinate of the click event, relative to the upper left corner of the screen.
   *
   * @return the y coordinate, -1 if unknown
   */
  public int getScreenY() {
    return (int) getData().get("screenY");
  }

  /**
   * Gets the x coordinate of the click event, relative to the upper left corner of the browser's
   * client area.
   *
   * @return the x coordinate, -1 if unknown
   */
  public int getClientX() {
    return (int) getData().get("clientX");
  }

  /**
   * Gets the y coordinate of the click event, relative to the upper left corner of the browser's
   * client area.
   *
   * @return the y coordinate, -1 if unknown
   */
  public int getClientY() {
    return (int) getData().get("clientY");
  }

  /**
   * Gets the number of times the mouse button was clicked.
   *
   * @return the number of times the mouse button was clicked
   */
  public int getDetail() {
    return (int) getData().get("detail");
  }

  /**
   * Gets the button number that was clicked.
   *
   * @return the button number that was clicked
   */
  public int getButton() {
    return (int) getData().get("button");
  }

  /**
   * Gets whether the control key was pressed during the click event.
   *
   * @return <code>true</code> if the control key was pressed, <code>false</code> otherwise
   */
  public boolean isCtrlKey() {
    return (boolean) getData().get("ctrlKey");
  }

  /**
   * Gets whether the shift key was pressed during the click event.
   *
   * @return <code>true</code> if the shift key was pressed, <code>false</code> otherwise
   */
  public boolean isShiftKey() {
    return (boolean) getData().get("shiftKey");
  }

  /**
   * Gets whether the alt key was pressed during the click event.
   *
   * @return <code>true</code> if the alt key was pressed, <code>false</code> otherwise
   */
  public boolean isAltKey() {
    return (boolean) getData().get("altKey");
  }

  /**
   * Gets whether the meta key was pressed during the click event.
   *
   * @return <code>true</code> if the meta key was pressed, <code>false</code> otherwise
   */
  public boolean isMetaKey() {
    return (boolean) getData().get("metaKey");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T getComponent() {
    return (T) super.getComponent();
  }
}
