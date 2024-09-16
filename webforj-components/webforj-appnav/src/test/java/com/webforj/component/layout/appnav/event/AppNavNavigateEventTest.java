package com.webforj.component.layout.appnav.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.webforj.component.layout.appnav.AppNav;
import com.webforj.router.history.Location;
import java.util.Map;
import org.junit.jupiter.api.Test;

class AppNavNavigateEventTest {

  @Test
  void shouldParseLocation() {
    AppNavNavigateEvent event =
        new AppNavNavigateEvent(new AppNav(), Map.of("path", "/test?query=1"));
    Location location = event.getLocation();
    assertNotNull(location);
    assertEquals("/test?query=1", location.toString());
  }
}
