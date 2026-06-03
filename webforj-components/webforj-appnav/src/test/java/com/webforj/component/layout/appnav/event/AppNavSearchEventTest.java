package com.webforj.component.layout.appnav.event;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.layout.appnav.AppNav;
import java.util.Map;
import org.junit.jupiter.api.Test;

class AppNavSearchEventTest {

  @Test
  void shouldReturnTerm() {
    AppNavSearchEvent event = new AppNavSearchEvent(new AppNav(), Map.of("term", "orders"));

    assertEquals("orders", event.getTerm());
  }
}
