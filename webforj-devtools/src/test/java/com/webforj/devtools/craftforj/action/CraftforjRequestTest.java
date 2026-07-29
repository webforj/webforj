package com.webforj.devtools.craftforj.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class CraftforjRequestTest {

  @Test
  @DisplayName("Should return null for missing or empty JSON")
  void shouldReturnNullForMissingJson() {
    assertNull(CraftforjRequest.fromJson(null));
    assertNull(CraftforjRequest.fromJson(""));
    assertNull(CraftforjRequest.fromJson("null"));
  }

  @Test
  @DisplayName("Should return null for malformed JSON instead of throwing")
  void shouldReturnNullForMalformedJson() {
    assertNull(CraftforjRequest.fromJson("{"));
    assertNull(CraftforjRequest.fromJson("[1]"));
    assertNull(CraftforjRequest.fromJson("\"just a string\"::"));
  }

  @Test
  @DisplayName("Should parse a full request")
  void shouldParseFullRequest() {
    CraftforjRequest request =
        CraftforjRequest.fromJson("{\"requestId\":\"r1\",\"action\":\"a\",\"params\":{\"x\":1}}");

    assertNotNull(request);
    assertEquals("r1", request.getRequestId());
    assertEquals("a", request.getAction());
    assertEquals(1, request.getParams().get("x").getAsInt());
  }

  @Test
  @DisplayName("Should default missing params to an empty object")
  void shouldDefaultMissingParams() {
    CraftforjRequest request = CraftforjRequest.fromJson("{\"requestId\":\"r1\",\"action\":\"a\"}");

    assertNotNull(request);
    assertNotNull(request.getParams());
    assertEquals(0, request.getParams().size());
  }
}
