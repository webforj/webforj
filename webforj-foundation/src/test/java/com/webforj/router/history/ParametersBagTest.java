package com.webforj.router.history;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;

class ParametersBagTest {

  @Test
  void shouldConstructEmptyParametersBag() {
    ParametersBag bag = new ParametersBag();
    assertEquals(0, bag.size());
  }

  @Test
  void shouldConstructParametersBagFromQueryString() {
    ParametersBag bag = ParametersBag.of("param1=value1&param2=value2");
    assertEquals(2, bag.size());
    assertEquals("value1", bag.get("param1").orElse(null));
    assertEquals("value2", bag.get("param2").orElse(null));
  }

  @Test
  void shouldConstructParametersBagFromMap() {
    Map<String, String> params = new HashMap<>();
    params.put("param1", "value1");
    params.put("param2", "value2");
    ParametersBag bag = ParametersBag.of(params);
    assertEquals(2, bag.size());
    assertEquals("value1", bag.get("param1").orElse(null));
    assertEquals("value2", bag.get("param2").orElse(null));
  }

  @Test
  void shouldAddNewParameter() {
    ParametersBag bag = new ParametersBag();
    bag.put("param1", "value1");
    assertEquals(1, bag.size());
    assertEquals("value1", bag.get("param1").orElse(null));
  }

  @Test
  void shouldRemoveParameter() {
    ParametersBag bag = new ParametersBag();
    bag.put("param1", "value1");
    bag.remove("param1");
    assertEquals(0, bag.size());
  }

  @Test
  void shouldCheckIfParameterExists() {
    ParametersBag bag = new ParametersBag();
    bag.put("param1", "value1");
    assertTrue(bag.containsKey("param1"));
    assertFalse(bag.containsKey("param2"));
  }

  @Test
  void shouldReturnAllParameters() {
    ParametersBag bag = new ParametersBag();
    bag.put("param1", "value1");
    bag.put("param2", "value2");
    Map<String, String> allParams = bag.all();
    assertEquals(2, allParams.size());
    assertEquals("value1", allParams.get("param1"));
    assertEquals("value2", allParams.get("param2"));
  }


  @Test
  void shouldReturnAlphabeticCharacters() {
    ParametersBag bag = new ParametersBag();
    bag.put("param1", "a1b2c3");
    assertEquals("abc", bag.getAlpha("param1").get());
  }

  @Test
  void shouldReturnAlphanumericCharacters() {
    ParametersBag bag = new ParametersBag();
    bag.put("param1", "a1b2c3!");
    assertEquals("a1b2c3", bag.getAlnum("param1").get());
  }

  @Test
  void shouldReturnDigitsOnly() {
    ParametersBag bag = new ParametersBag();
    bag.put("param1", "a1b2c3");
    assertEquals("123", bag.getDigits("param1").get());
  }

  @Test
  void shouldReturnIntegerValue() {
    ParametersBag bag = new ParametersBag();
    bag.put("param1", "123");
    assertEquals(123, bag.getInt("param1").get());
  }

  @Test
  void shouldReturnFloatValue() {
    ParametersBag bag = new ParametersBag();
    bag.put("param1", "123.45");
    assertEquals(123.45f, bag.getFloat("param1").get(), 0.001);
  }

  @Test
  void shouldReturnDoubleValue() {
    ParametersBag bag = new ParametersBag();
    bag.put("param1", "123.456");
    assertEquals(123.456, bag.getDouble("param1").get(), 0.001);
  }

  @Test
  void shouldReturnBooleanValue() {
    ParametersBag bag = new ParametersBag();
    bag.put("param1", "true");
    assertTrue(bag.getBoolean("param1").get());
  }

  @Test
  void shouldReturnQueryString() {
    ParametersBag bag = new ParametersBag();
    bag.put("param1", "value1");
    bag.put("param2", "value2");
    assertEquals("param1=value1&param2=value2", bag.getQueryString());
  }

  @Test
  void shouldIterateOverParameters() {
    ParametersBag bag = new ParametersBag();
    bag.put("param1", "value1");
    bag.put("param2", "value2");
    int count = 0;
    for (@SuppressWarnings("unused")
    Map.Entry<String, String> entry : bag) {
      count++;
    }

    assertEquals(2, count);
  }
}
