package org.dwcj.component.element.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ElementEventOptionsTest {

  @Test
  @DisplayName("Test debounce configuration")
  void testDebounceConfig() {
    ElementEventOptions options = new ElementEventOptions();
    options.setDebounce(100);

    assertTrue(options.isDebounce());
    assertFalse(options.isThrottle());
    assertEquals(100, options.getDebounceTimeout());
    assertEquals(DebouncePhase.TRAILING, options.getDebouncePhase());
  }

  @Test
  @DisplayName("Test throttle configuration")
  void testThrottleConfig() {
    ElementEventOptions options = new ElementEventOptions();
    options.setThrottle(200);

    assertFalse(options.isDebounce());
    assertTrue(options.isThrottle());
    assertEquals(200, options.getThrottleTimeout());
  }

  @Test
  @DisplayName("Test immediate event")
  void testImmediateConfig() {
    ElementEventOptions options = new ElementEventOptions();
    options.setImmediate();

    assertFalse(options.isDebounce());
    assertFalse(options.isThrottle());
    assertTrue(options.isImmediate());
  }

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
  @DisplayName("Test in-place merging debounce and throttle options")
  void testMergeDebounceThrottle() {
    ElementEventOptions debounceOptions = new ElementEventOptions();
    debounceOptions.setDebounce(100);

    ElementEventOptions throttleOptions = new ElementEventOptions();
    throttleOptions.setThrottle(200);

    ElementEventOptions mergedOptions = new ElementEventOptions();
    mergedOptions.mergeWith(debounceOptions, throttleOptions);

    assertFalse(mergedOptions.isDebounce());
    assertTrue(mergedOptions.isThrottle());
    assertEquals(0, mergedOptions.getDebounceTimeout());
    assertEquals(200, mergedOptions.getThrottleTimeout());
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
