package com.webforj.bbjship.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.google.gson.GsonBuilder;
import java.util.List;
import org.junit.jupiter.api.Test;

final class BBjShipVersionTest {

  @Test
  void shouldWriteTheVersionField() {
    BBjShipVersion version = new BBjShipVersion("26.03-SNAPSHOT",
        List.of("-Xmx1024m", "-XX:+AllowEnhancedClassRedefinition"), 4242);

    assertEquals("26.03-SNAPSHOT", version.getVersion());
    assertEquals(List.of("-Xmx1024m", "-XX:+AllowEnhancedClassRedefinition"),
        version.getJavaArguments());
    assertEquals(4242, version.getPid());
    assertEquals("""
        {
          "version": "26.03-SNAPSHOT",
          "javaArguments": [
            "-Xmx1024m",
            "-XX:+AllowEnhancedClassRedefinition"
          ],
          "pid": 4242
        }""", new GsonBuilder().setPrettyPrinting().create().toJson(version));
  }
}
