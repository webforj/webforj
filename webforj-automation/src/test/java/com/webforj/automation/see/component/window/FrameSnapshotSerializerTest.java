package com.webforj.automation.see.component.window;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.Environment;
import com.webforj.automation.FrameEnvironment;
import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.window.Frame;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;

class FrameSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldPrintTheTitleAtBothLevels(boolean browserState) throws Exception {
    Environment environment = FrameEnvironment.create(null);

    try (MockedStatic<Environment> current = mockStatic(Environment.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame orders = new Frame("Orders");

      assertEquals("- Frame \"Orders\"\n",
          Snapshot.render(orders, new SnapshotOptions().setBrowserState(browserState)));
    }
  }

  @Test
  void shouldPrintTheLocationOfTheRouterWithBrowserState() throws Exception {
    Router router = mock(Router.class, RETURNS_DEEP_STUBS);
    when(router.getHistory().getLocation()).thenReturn(Optional.of(new Location("/orders?page=2")));
    Environment environment = FrameEnvironment.create(router);

    try (MockedStatic<Environment> current = mockStatic(Environment.class)) {
      current.when(Environment::getCurrent).thenReturn(environment);
      Frame orders = new Frame("Orders");

      assertEquals("- Frame \"Orders\"\n", Snapshot.render(orders));
      assertEquals("- Frame \"Orders\" [location=\"/orders?page=2\"]\n",
          Snapshot.render(orders, new SnapshotOptions().setBrowserState(true)));
    }
  }
}
