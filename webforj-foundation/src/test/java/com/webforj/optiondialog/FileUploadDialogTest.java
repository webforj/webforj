package com.webforj.optiondialog;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class FileUploadDialogTest {

  FileUploadDialog component;
  FileChooserFilter mockFilter1;
  FileChooserFilter mockFilter2;

  @BeforeEach
  void setUp() {
    component = new FileUploadDialog();
    mockFilter1 = new FileChooserFilter("Text Files (*.txt)", "*.txt");
    mockFilter2 = new FileChooserFilter("Java Files (*.java)", "*.java");
  }

  @Nested
  class Constructors {

    @Test
    void shouldConstructWithAllParameters() {
      component = new FileUploadDialog("Title", Arrays.asList(mockFilter1, mockFilter2),
          mockFilter1.getDescription());

      assertEquals("Title", component.getTitle());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), component.getFilters());
      assertEquals(mockFilter1, component.getActiveFilter());
    }

    @Test
    void shouldConstructWithTitleAndFilers() {
      component = new FileUploadDialog("Title", Arrays.asList(mockFilter1, mockFilter2));

      assertEquals("Title", component.getTitle());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), component.getFilters());
      assertNull(component.getActiveFilter());
    }

    @Test
    void shouldConstructWithTitle() {
      component = new FileUploadDialog("Title");

      assertEquals("Title", component.getTitle());
      assertTrue(component.getFilters().isEmpty());
      assertNull(component.getActiveFilter());
    }
  }

  @Test
  void shouldSetAndGetDrop() {
    component.setDrop(true);
    assertTrue(component.isDrop());
    assertTrue(component.getAttributes().containsKey("drop"));
    assertEquals("true", component.getAttributes().get("drop"));

    component.setDrop(false);
    assertFalse(component.isDrop());
    assertEquals("false", component.getAttributes().get("drop"));
  }

  @Test
  void shouldSetGetFiltersVisible() {
    component.setFiltersVisible(true);
    assertTrue(component.isFiltersVisible());
    assertEquals("true", component.getAttributes().get("filters-visible"));

    component.setFiltersVisible(false);
    assertFalse(component.isFiltersVisible());
    assertEquals("false", component.getAttributes().get("filters-visible"));
  }

  @Test
  void shouldSetGetMultiFilterSelection() {
    component.setMultiFilterSelection(true);
    assertTrue(component.isMultiFilterSelection());
    assertTrue(component.getAttributes().containsKey("multi-filter-selection"));

    component.setMultiFilterSelection(false);
    assertFalse(component.isMultiFilterSelection());
    assertEquals("false", component.getAttributes().get("multi-filter-selection"));
  }

  @Test
  void shouldSetGetMaxFileSize() {
    component.setMaxFileSize(100);
    assertEquals(100, component.getMaxFileSize());

    assertTrue(component.getAttributes().containsKey("max-size"));
    assertEquals("100", component.getAttributes().get("max-size"));
  }
}
