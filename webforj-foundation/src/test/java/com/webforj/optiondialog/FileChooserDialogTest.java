package com.webforj.optiondialog;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.Arrays;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class FileChooserDialogTest {

  FileChooserDialog dialog;
  FileChooserFilter mockFilter1;
  FileChooserFilter mockFilter2;

  @BeforeEach
  void setUp() {
    mockFilter1 = mock(FileChooserFilter.class);
    mockFilter2 = mock(FileChooserFilter.class);
  }

  @Nested
  class Constructors {

    @Test
    void shouldConstructWithAllParameters() {
      dialog = new FileChooserDialog("Title", "/path", Arrays.asList(mockFilter1, mockFilter2),
          "ActiveFilter", true, FileChooserDialog.SelectionMode.FILES_AND_DIRECTORIES);
      assertEquals("Title", dialog.getTitle());
      assertEquals("/path", dialog.getInitialPath());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), dialog.getFilters());
      assertEquals(true, dialog.isRestricted());
      assertEquals(FileChooserDialog.SelectionMode.FILES_AND_DIRECTORIES,
          dialog.getSelectionMode());
    }

    @Test
    void shouldConstructWithTitleInitialPathAndFilters() {
      dialog = new FileChooserDialog("Title", "/path", Arrays.asList(mockFilter1, mockFilter2));
      assertEquals("Title", dialog.getTitle());
      assertEquals("/path", dialog.getInitialPath());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), dialog.getFilters());
    }

    @Test
    void shouldConstructWithTitleInitialPathAndSelectionMode() {
      dialog = new FileChooserDialog("Title", "/path", FileChooserDialog.SelectionMode.DIRECTORIES);
      assertEquals("Title", dialog.getTitle());
      assertEquals("/path", dialog.getInitialPath());
      assertEquals(FileChooserDialog.SelectionMode.DIRECTORIES, dialog.getSelectionMode());
    }

    @Test
    void shouldConstructWithTitleInitialPathFiltersAndActiveFilter() {
      dialog = new FileChooserDialog("Title", "/path", Arrays.asList(mockFilter1, mockFilter2),
          "ActiveFilter");
      assertEquals("Title", dialog.getTitle());
      assertEquals("/path", dialog.getInitialPath());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), dialog.getFilters());
    }

    @Test
    void shouldConstructWithTitleInitialPathFiltersAndRestricted() {
      dialog =
          new FileChooserDialog("Title", "/path", Arrays.asList(mockFilter1, mockFilter2), true);
      assertEquals("Title", dialog.getTitle());
      assertEquals("/path", dialog.getInitialPath());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), dialog.getFilters());
      assertTrue(dialog.isRestricted());
    }

    @Test
    void shouldConstructWithTitleInitialPathAndRestricted() {
      dialog = new FileChooserDialog("Title", "/path", true);
      assertEquals("Title", dialog.getTitle());
      assertEquals("/path", dialog.getInitialPath());
      assertTrue(dialog.isRestricted());
    }

    @Test
    void shouldConstructWithTitleInitialPathFiltersRestrictedAndSelectionMode() {
      dialog = new FileChooserDialog("Title", "/path", Arrays.asList(mockFilter1, mockFilter2),
          true, FileChooserDialog.SelectionMode.DIRECTORIES);
      assertEquals("Title", dialog.getTitle());
      assertEquals("/path", dialog.getInitialPath());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), dialog.getFilters());
      assertTrue(dialog.isRestricted());
      assertEquals(FileChooserDialog.SelectionMode.DIRECTORIES, dialog.getSelectionMode());
    }

    @Test
    void shouldConstructWithTitleAndInitialPath() {
      dialog = new FileChooserDialog("Title", "/path");
      assertEquals("Title", dialog.getTitle());
      assertEquals("/path", dialog.getInitialPath());
    }

    @Test
    void shouldConstructWithTitleOnly() {
      dialog = new FileChooserDialog("Title");
      assertEquals("Title", dialog.getTitle());
    }

    @Test
    void shouldConstructWithDefaultTitle() {
      dialog = new FileChooserDialog();
      assertEquals(FileChooserDialog.DEFAULT_TITLE, dialog.getTitle());
    }
  }

  @Test
  void shouldSetAndGetId() {
    dialog = new FileChooserDialog();
    dialog.setId("id");
    assertEquals("id", dialog.getId());

    assertEquals("id", dialog.getAttributes().get("dwc-fs-server-id"));
  }

  @Test
  void shouldSetAndGetInitialPath() {
    dialog = new FileChooserDialog();
    dialog.setInitialPath("/newpath");
    assertEquals("/newpath", dialog.getInitialPath());
  }

  @Test
  void shouldSetAndGetRestricted() {
    dialog = new FileChooserDialog();
    dialog.setRestricted(true);
    assertTrue(dialog.isRestricted());
  }

  @Test
  void shouldSetAndGetSelectionMode() {
    dialog = new FileChooserDialog();
    dialog.setSelectionMode(FileChooserDialog.SelectionMode.DIRECTORIES);
    assertEquals(FileChooserDialog.SelectionMode.DIRECTORIES, dialog.getSelectionMode());
  }

  @Test
  void shouldSetAndGetCustomFilters() {
    dialog = new FileChooserDialog();
    dialog.setCustomFilters(true);
    assertTrue(dialog.isCustomFilters());

    assertEquals("true", dialog.getAttributes().get("custom-filters"));
  }

  @Test
  void shouldSetAndGetCacheCustomFilters() {
    dialog = new FileChooserDialog();
    dialog.setCacheCustomFilters(true);
    assertTrue(dialog.isCacheCustomFilters());

    assertEquals("true", dialog.getAttributes().get("cache-custom-filters"));
  }

  @Test
  void shouldSetAndGetGridView() {
    dialog = new FileChooserDialog();

    dialog.setGridView(true);
    assertTrue(dialog.isGridView());
    assertEquals("grid", dialog.getAttributes().get("view"));

    dialog.setGridView(false);
    assertFalse(dialog.isGridView());
    assertEquals("detail", dialog.getAttributes().get("view"));
  }
}
