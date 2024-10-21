package com.webforj.router.history;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import org.junit.jupiter.api.Test;

class LocationTest {

  @Test
  void shouldConstructFromPathSegmentsQueryAndFragment() {
    Location location =
        new Location(List.of("segment1", "segment2"), "param1=value1&param2=value2", "fragment");
    assertEquals(List.of("segment1", "segment2"), location.getSegments().all());
    assertEquals("value1", location.getQueryParameters().get("param1").orElse(null));
    assertEquals("value2", location.getQueryParameters().get("param2").orElse(null));
    assertEquals("fragment", location.getFragment());
  }

  @Test
  void shouldConstructFromPathSegmentsAndQuery() {
    Location location =
        new Location(List.of("segment1", "segment2"), "param1=value1&param2=value2");
    assertEquals(List.of("segment1", "segment2"), location.getSegments().all());
    assertEquals("value1", location.getQueryParameters().get("param1").orElse(null));
    assertEquals("value2", location.getQueryParameters().get("param2").orElse(null));
    assertNull(location.getFragment());
  }

  @Test
  void shouldConstructFromPathSegmentsOnly() {
    Location location = new Location(List.of("segment1", "segment2"));
    assertEquals(List.of("segment1", "segment2"), location.getSegments().all());
    assertTrue(location.getQueryParameters().all().isEmpty());
    assertNull(location.getFragment());
  }

  @Test
  void shouldConstructFromLocationString() {
    Location location = new Location("segment1/segment2?param1=value1&param2=value2#fragment");
    assertEquals(List.of("segment1", "segment2"), location.getSegments().all());
    assertEquals("value1", location.getQueryParameters().get("param1").orElse(null));
    assertEquals("value2", location.getQueryParameters().get("param2").orElse(null));
    assertEquals("fragment", location.getFragment());
  }

  @Test
  void shouldThrowExceptionForInvalidLocationString() {
    assertThrows(IllegalArgumentException.class, () -> new Location("://invalid-uri"));
  }

  @Test
  void shouldGetQueryParameters() {
    Location location = new Location("segment1/segment2?param1=value1&param2=value2");
    assertEquals("value1", location.getQueryParameters().get("param1").orElse(null));
    assertEquals("value2", location.getQueryParameters().get("param2").orElse(null));
  }

  @Test
  void shouldGetPathSegments() {
    Location location = new Location("segment1/segment2");
    assertEquals(List.of("segment1", "segment2"), location.getSegments().all());
  }

  @Test
  void shouldGetFragment() {
    Location location = new Location("segment1/segment2#fragment");
    assertEquals("fragment", location.getFragment());
  }

  @Test
  void shouldGetFullURI() {
    Location location =
        new Location(List.of("segment1", "segment2"), "param1=value1&param2=value2", "fragment");
    String fullURI = location.getFullURI();
    assertEquals("/segment1/segment2?param1=value1&param2=value2#fragment", fullURI);
  }

  @Test
  void shouldGetFullURIWithoutFragment() {
    Location location =
        new Location(List.of("segment1", "segment2"), "param1=value1&param2=value2");
    String fullURI = location.getFullURI();
    assertEquals("/segment1/segment2?param1=value1&param2=value2", fullURI);
  }

  @Test
  void shouldGetFullURIWithoutQuery() {
    Location location = new Location(List.of("segment1", "segment2"));
    String fullURI = location.getFullURI();
    assertEquals("/segment1/segment2", fullURI);
  }
}
