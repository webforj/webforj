package com.webforj.component.layout.appnav.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.router.history.Location;
import java.util.Map;

/**
 * Emitted when the user changes the navigation location by clicking on a navigation item.
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 *
 * @see AppNav
 */
@EventName(value = "dwc-location-changed")
@EventOptions(data = {@EventData(key = "path", exp = "event.detail")})
public final class AppNavLocationChangedEvent extends ComponentEvent<AppNav> {

  /**
   * Creates a new {@code AppNavLocationChangedEvent} with the given target and detail.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public AppNavLocationChangedEvent(AppNav target, Map<String, Object> detail) {
    super(target, detail);
  }

  /**
   * Gets the location of the navigation.
   *
   * @return the location
   */
  public Location getLocation() {
    String path = (String) getData().get("path");
    return new Location(path);
  }
}
