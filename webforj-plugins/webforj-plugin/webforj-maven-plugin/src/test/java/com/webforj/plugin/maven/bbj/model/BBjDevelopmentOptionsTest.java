package com.webforj.plugin.maven.bbj.model;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.File;
import org.junit.jupiter.api.Test;

class BBjDevelopmentOptionsTest {

  @Test
  void shouldStartWithoutClassFolder() {
    BBjDevelopmentOptions options = new BBjDevelopmentOptions();

    assertNull(options.getClassFolder());
  }

  @Test
  void shouldHoldTheConfiguredClassFolder() {
    BBjDevelopmentOptions options = new BBjDevelopmentOptions();
    File classFolder = new File("/work/inventory/target/classes");

    options.setClassFolder(classFolder);

    assertSame(classFolder, options.getClassFolder());
  }

  @Test
  void shouldRejectNullAndKeepTheConfiguredClassFolder() {
    BBjDevelopmentOptions options = new BBjDevelopmentOptions();
    File classFolder = new File("/work/inventory/target/classes");
    options.setClassFolder(classFolder);
    File missing = null;

    assertThrows(NullPointerException.class, () -> options.setClassFolder(missing));

    assertSame(classFolder, options.getClassFolder());
  }
}
