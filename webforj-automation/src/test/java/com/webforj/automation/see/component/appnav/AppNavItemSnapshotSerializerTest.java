package com.webforj.automation.see.component.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.component.layout.appnav.AppNavItem;
import org.junit.jupiter.api.Test;

class AppNavItemSnapshotSerializerTest {

  @Test
  void shouldPrintThePathByDefault() {
    AppNavItem item = new AppNavItem("Orders", "/orders");

    assertEquals("- AppNavItem [path=/orders]\n", Snapshot.render(item));
  }
}
