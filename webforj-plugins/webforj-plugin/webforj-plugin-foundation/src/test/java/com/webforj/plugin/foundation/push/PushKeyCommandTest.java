package com.webforj.plugin.foundation.push;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.interaso.webpush.VapidKeys;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import java.util.List;
import org.junit.jupiter.api.Test;

class PushKeyCommandTest {

  @Test
  void shouldRenderTheBannerAndTheThreeConfigurationLines() {
    List<String> lines = PushKeyCommand.render();

    assertEquals(5, lines.size());
    assertEquals(PushKeyCommand.BANNER, lines.get(0));
    assertEquals("", lines.get(1));
    assertTrue(lines.get(2).startsWith(PushKeyCommand.PUBLIC_KEY + " = \""), lines.get(2));
    assertTrue(lines.get(3).startsWith(PushKeyCommand.PRIVATE_KEY + " = \""), lines.get(3));
    assertEquals(PushKeyCommand.SUBJECT + " = \"mailto:you@example.com\"", lines.get(4));
  }

  @Test
  void shouldRenderLinesTheApplicationConfigurationParses() {
    List<String> lines = PushKeyCommand.render();
    Config config = ConfigFactory.parseString(String.join("\n", lines.subList(2, 5)));

    assertEquals("mailto:you@example.com", config.getString(PushKeyCommand.SUBJECT));
    assertDoesNotThrow(() -> VapidKeys.create(config.getString(PushKeyCommand.PUBLIC_KEY),
        config.getString(PushKeyCommand.PRIVATE_KEY)));
  }

  @Test
  void shouldGenerateTheFreshPairOnEveryRun() {
    assertNotEquals(PushKeyCommand.render().get(3), PushKeyCommand.render().get(3));
  }
}
