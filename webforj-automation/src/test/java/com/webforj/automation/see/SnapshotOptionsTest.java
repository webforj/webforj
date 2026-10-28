package com.webforj.automation.see;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class SnapshotOptionsTest {

  @Test
  void shouldDescribeTheServerStateOfTheShownComponentsByDefault() {
    SnapshotOptions options = new SnapshotOptions();

    assertEquals(10, options.getMaxWidth());
    assertFalse(options.isBrowserState());
    assertFalse(options.isHiddenComponents());
    assertFalse(options.isComposites());
  }

  @Test
  void shouldKeepWhatWasSet() {
    SnapshotOptions options = new SnapshotOptions().setMaxWidth(3).setBrowserState(true)
        .setHiddenComponents(true).setComposites(true);

    assertEquals(3, options.getMaxWidth());
    assertTrue(options.isBrowserState());
    assertTrue(options.isHiddenComponents());
    assertTrue(options.isComposites());
  }

  @Test
  void shouldRejectWidthBelowOne() {
    SnapshotOptions options = new SnapshotOptions();

    IllegalArgumentException failure =
        assertThrows(IllegalArgumentException.class, () -> options.setMaxWidth(0));

    assertEquals("The snapshot width must be at least one", failure.getMessage());
  }
}
