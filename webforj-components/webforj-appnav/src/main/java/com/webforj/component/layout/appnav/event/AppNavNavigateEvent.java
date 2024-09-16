package com.webforj.component.layout.appnav.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.layout.appnav.AppNav;
import com.webforj.router.history.Location;
import static com.webforj.App.console;
import java.util.Map;

/**
 * Emitted when the user navigates to a new location through the app nav.
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 *
 * @see AppNav
 */
@EventName(value = "dwc-navigate")
@EventOptions(data = {@EventData(key = "path", exp = "event.detail")})
public final class AppNavNavigateEvent extends ComponentEvent<AppNav> {

  /**
   * Creates a navigate Event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public AppNavNavigateEvent(AppNav target, Map<String, Object> detail) {
    super(target, detail);
  }

  /**
   * Gets the location of the navigation.
   *
   * @return the location
   */
  public Location getLocation() {
    String path = (String) getData().get("path");
    console().log("path: " + path);
    return new Location(path);
  }
}
