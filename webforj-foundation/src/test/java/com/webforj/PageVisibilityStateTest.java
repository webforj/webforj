package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class PageVisibilityStateTest {

  @Test
  void shouldReturnReportedValue() {
    assertEquals("visible", PageVisibilityState.VISIBLE.getValue());
    assertEquals("hidden", PageVisibilityState.HIDDEN.getValue());
  }

  @Test
  void shouldResolveVisibleFromValueIgnoringCase() {
    assertEquals(PageVisibilityState.VISIBLE, PageVisibilityState.fromValue("VISIBLE"));
    assertEquals(PageVisibilityState.VISIBLE, PageVisibilityState.fromValue("Visible"));
  }

  @Test
  void shouldFallBackToHiddenForLegacyValues() {
    assertEquals(PageVisibilityState.HIDDEN, PageVisibilityState.fromValue("prerender"));
    assertEquals(PageVisibilityState.HIDDEN, PageVisibilityState.fromValue("unloaded"));
  }

  @Test
  void shouldFallBackToHiddenForNullValue() {
    assertEquals(PageVisibilityState.HIDDEN, PageVisibilityState.fromValue(null));
  }

  @Test
  void shouldFallBackToHiddenForEmptyValue() {
    assertEquals(PageVisibilityState.HIDDEN, PageVisibilityState.fromValue(""));
  }
}
