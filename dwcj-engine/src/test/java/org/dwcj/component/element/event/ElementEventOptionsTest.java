package org.dwcj.component.element.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ElementEventOptionsTest {

  @Test
  @DisplayName("Test in-place merging of event options with non-empty base and override options")
  void testMergeEventOptions() {
    ElementEventOptions baseOptions = new ElementEventOptions();
    baseOptions.addItem("key1", "value1");
    baseOptions.setCode("base code");
    baseOptions.setFilter("base filter");

    ElementEventOptions overrideOptions = new ElementEventOptions();
    overrideOptions.addItem("key2", "value2");
    overrideOptions.setCode("override code");

    baseOptions.mergeWith(overrideOptions);

    assertEquals("value1", baseOptions.getItems().get("key1"));
    assertEquals("value2", baseOptions.getItems().get("key2"));
    assertEquals("override code", baseOptions.getCode());
    assertEquals("base filter", baseOptions.getFilter());
  }

  @Test
  @DisplayName("Test in-place merging of event options with a null override")
  void testMergeWithNull() {
    ElementEventOptions options = new ElementEventOptions();
    options.addItem("key1", "value1");

    options.mergeWith(null);
    assertEquals("value1", options.getItems().get("key1"));
  }

  @Test
  @DisplayName("Test in-place merging with empty event options")
  void testMergeEmptyOptions() {
    ElementEventOptions options = new ElementEventOptions();

    options.mergeWith(new ElementEventOptions());

    assertTrue(options.getItems().isEmpty());
    assertEquals("", options.getCode());
    assertEquals("", options.getFilter());
  }
}
