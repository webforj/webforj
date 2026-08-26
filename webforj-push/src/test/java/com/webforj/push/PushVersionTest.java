package com.webforj.push;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import org.junit.jupiter.api.Test;

class PushVersionTest {

  private static String read(String content) throws IOException {
    return PushVersion.read(new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8)));
  }

  @Test
  void shouldReadTheStampedVersion() throws IOException {
    assertEquals("26.02", read("version=26.02\n"));
  }

  @Test
  void shouldFallBackWhenTheBuildDidNotStampTheVersion() throws IOException {
    assertEquals(PushVersion.UNKNOWN, read("version=${project.version}\n"));
    assertEquals(PushVersion.UNKNOWN, read("other=1\n"));
    assertEquals(PushVersion.UNKNOWN, PushVersion.read(null));
  }

  @Test
  void shouldAlwaysReturnTheValue() {
    assertNotNull(PushVersion.get());
  }
}
