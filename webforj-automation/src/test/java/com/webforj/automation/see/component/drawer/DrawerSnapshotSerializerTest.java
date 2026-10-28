package com.webforj.automation.see.component.drawer;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.drawer.Drawer;
import org.junit.jupiter.api.Test;

class DrawerSnapshotSerializerTest {

  @Test
  void shouldDescribeTheOpenedDrawerByItsLabelByDefault() {
    Drawer drawer = new Drawer("Filters");
    drawer.open();

    assertEquals("- Drawer [label=Filters]\n", Snapshot.render(drawer));
  }

  @Test
  void shouldFlagTheOpenedDrawerWithBrowserState() {
    Drawer drawer = new Drawer("Filters");
    drawer.open();

    assertEquals("- Drawer [label=Filters] [opened]\n",
        Snapshot.render(drawer, new SnapshotOptions().setBrowserState(true)));
  }

  @Test
  void shouldLeaveTheClosedDrawerUnflaggedWithBrowserState() {
    Drawer drawer = new Drawer("Filters");
    drawer.close();

    assertEquals("- Drawer [label=Filters]\n",
        Snapshot.render(drawer, new SnapshotOptions().setBrowserState(true)));
  }
}
