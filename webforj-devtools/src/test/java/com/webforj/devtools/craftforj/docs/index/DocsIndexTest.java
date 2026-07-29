package com.webforj.devtools.craftforj.docs.index;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DocsIndexTest {

  private DocsIndex index;

  @BeforeEach
  void setUp() {
    index = new DocsIndex();
  }

  @Test
  void shouldLoadIndexFromClasspath() {
    assertNotNull(index.getEntries());
    assertFalse(index.getEntries().isEmpty());
  }

  @Test
  void shouldFindAlertByServerComponent() {
    var entry = index.findByServerComponent("com.webforj.component.alert.Alert");
    assertNotNull(entry);
    assertNotNull(entry.getTitle());
  }

  @Test
  void shouldFindAlertByClientComponent() {
    var entry = index.findByClientComponent("dwc-alert");
    assertNotNull(entry);
    assertNotNull(entry.getTitle());
  }

  @Test
  void shouldReturnNullForUnknownServerComponent() {
    var entry = index.findByServerComponent("com.unknown.Component");
    assertNull(entry);
  }

  @Test
  void shouldReturnNullForUnknownClientComponent() {
    var entry = index.findByClientComponent("unknown-component");
    assertNull(entry);
  }

  @Test
  void shouldReturnNullForNullInput() {
    assertNull(index.findByServerComponent(null));
    assertNull(index.findByServerComponent(""));
    assertNull(index.findByClientComponent(null));
    assertNull(index.findByClientComponent(""));
  }
}
