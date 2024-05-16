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

  FileUploadDialog dialog;
  FileChooserFilter mockFilter1;
  FileChooserFilter mockFilter2;

  @BeforeEach
  void setUp() {
    dialog = new FileUploadDialog();
    mockFilter1 = new FileChooserFilter("Text Files (*.txt)", "*.txt");
    mockFilter2 = new FileChooserFilter("Java Files (*.java)", "*.java");
  }

  @Nested
  class Constructors {

    @Test
    void shouldConstructWithAllParameters() {
      dialog = new FileUploadDialog("Title", Arrays.asList(mockFilter1, mockFilter2),
          mockFilter1.getDescription());

      assertEquals("Title", dialog.getTitle());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), dialog.getFilters());
      assertEquals(mockFilter1, dialog.getActiveFilter());
    }

    @Test
    void shouldConstructWithTitleAndFilers() {
      dialog = new FileUploadDialog("Title", Arrays.asList(mockFilter1, mockFilter2));

      assertEquals("Title", dialog.getTitle());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), dialog.getFilters());
      assertNull(dialog.getActiveFilter());
    }

    @Test
    void shouldConstructWithTitle() {
      dialog = new FileUploadDialog("Title");

      assertEquals("Title", dialog.getTitle());
      assertTrue(dialog.getFilters().isEmpty());
      assertNull(dialog.getActiveFilter());
    }
  }

  @Test
  void shouldSetAndGetDrop() {
    dialog.setDrop(true);
    assertTrue(dialog.isDrop());
    assertTrue(dialog.getAttributes().containsKey("drop"));
    assertEquals("true", dialog.getAttributes().get("drop"));

    dialog.setDrop(false);
    assertFalse(dialog.isDrop());
    assertEquals("false", dialog.getAttributes().get("drop"));
  }

  @Test
  void shouldSetGetFiltersVisible() {
    dialog.setFiltersVisible(true);
    assertTrue(dialog.isFiltersVisible());
    assertEquals("true", dialog.getAttributes().get("filters-visible"));

    dialog.setFiltersVisible(false);
    assertFalse(dialog.isFiltersVisible());
    assertEquals("false", dialog.getAttributes().get("filters-visible"));
  }

  @Test
  void shouldSetGetMultiFilterSelection() {
    dialog.setMultiFilterSelection(true);
    assertTrue(dialog.isMultiFilterSelection());
    assertTrue(dialog.getAttributes().containsKey("multi-filter-selection"));

    dialog.setMultiFilterSelection(false);
    assertFalse(dialog.isMultiFilterSelection());
    assertEquals("false", dialog.getAttributes().get("multi-filter-selection"));
  }

  @Test
  void shouldSetGetMaxFileSize() {
    dialog.setMaxFileSize(100);
    assertEquals(100, dialog.getMaxFileSize());

    assertTrue(dialog.getAttributes().containsKey("max-size"));
    assertEquals("100", dialog.getAttributes().get("max-size"));
  }
}
