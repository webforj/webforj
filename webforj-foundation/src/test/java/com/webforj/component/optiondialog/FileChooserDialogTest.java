package com.webforj.component.optiondialog;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.Arrays;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class FileChooserDialogTest {

  FileChooserDialog component;
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
      component = new FileChooserDialog("Title", "/path", Arrays.asList(mockFilter1, mockFilter2),
          "ActiveFilter", true, FileChooserDialog.SelectionMode.FILES_AND_DIRECTORIES);
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), component.getFilters());
      assertEquals(true, component.isRestricted());
      assertEquals(FileChooserDialog.SelectionMode.FILES_AND_DIRECTORIES,
          component.getSelectionMode());
    }

    @Test
    void shouldConstructWithTitleInitialPathAndFilters() {
      component = new FileChooserDialog("Title", "/path", Arrays.asList(mockFilter1, mockFilter2));
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), component.getFilters());
    }

    @Test
    void shouldConstructWithTitleInitialPathAndSelectionMode() {
      component =
          new FileChooserDialog("Title", "/path", FileChooserDialog.SelectionMode.DIRECTORIES);
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals(FileChooserDialog.SelectionMode.DIRECTORIES, component.getSelectionMode());
    }

    @Test
    void shouldConstructWithTitleInitialPathFiltersAndActiveFilter() {
      component = new FileChooserDialog("Title", "/path", Arrays.asList(mockFilter1, mockFilter2),
          "ActiveFilter");
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), component.getFilters());
    }

    @Test
    void shouldConstructWithTitleInitialPathFiltersAndRestricted() {
      component =
          new FileChooserDialog("Title", "/path", Arrays.asList(mockFilter1, mockFilter2), true);
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), component.getFilters());
      assertTrue(component.isRestricted());
    }

    @Test
    void shouldConstructWithTitleInitialPathAndRestricted() {
      component = new FileChooserDialog("Title", "/path", true);
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertTrue(component.isRestricted());
    }

    @Test
    void shouldConstructWithTitleInitialPathFiltersRestrictedAndSelectionMode() {
      component = new FileChooserDialog("Title", "/path", Arrays.asList(mockFilter1, mockFilter2),
          true, FileChooserDialog.SelectionMode.DIRECTORIES);
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), component.getFilters());
      assertTrue(component.isRestricted());
      assertEquals(FileChooserDialog.SelectionMode.DIRECTORIES, component.getSelectionMode());
    }

    @Test
    void shouldConstructWithTitleAndInitialPath() {
      component = new FileChooserDialog("Title", "/path");
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
    }

    @Test
    void shouldConstructWithTitleOnly() {
      component = new FileChooserDialog("Title");
      assertEquals("Title", component.getTitle());
    }

    @Test
    void shouldConstructWithDefaultTitle() {
      component = new FileChooserDialog();
      assertEquals(FileChooserDialog.DEFAULT_TITLE, component.getTitle());
    }
  }

  @Test
  void shouldSetAndGetId() {
    component = new FileChooserDialog();
    component.setId("id");
    assertEquals("id", component.getId());

    assertEquals("id", component.getAttributes().get("dwc-fs-server-id"));
  }

  @Test
  void shouldSetAndGetInitialPath() {
    component = new FileChooserDialog();
    component.setInitialPath("/newpath");
    assertEquals("/newpath", component.getInitialPath());
  }

  @Test
  void shouldSetAndGetRestricted() {
    component = new FileChooserDialog();
    component.setRestricted(true);
    assertTrue(component.isRestricted());
  }

  @Test
  void shouldSetAndGetSelectionMode() {
    component = new FileChooserDialog();
    component.setSelectionMode(FileChooserDialog.SelectionMode.DIRECTORIES);
    assertEquals(FileChooserDialog.SelectionMode.DIRECTORIES, component.getSelectionMode());
  }

  @Test
  void shouldSetAndGetCustomFilters() {
    component = new FileChooserDialog();
    component.setCustomFilters(true);
    assertTrue(component.isCustomFilters());

    assertEquals("true", component.getAttributes().get("custom-filters"));
  }

  @Test
  void shouldSetAndGetCacheCustomFilters() {
    component = new FileChooserDialog();
    component.setCacheCustomFilters(true);
    assertTrue(component.isCacheCustomFilters());

    assertEquals("true", component.getAttributes().get("cache-custom-filters"));
  }

  @Test
  void shouldSetAndGetGridView() {
    component = new FileChooserDialog();

    component.setGridView(true);
    assertTrue(component.isGridView());
    assertEquals("grid", component.getAttributes().get("view"));

    component.setGridView(false);
    assertFalse(component.isGridView());
    assertEquals("detail", component.getAttributes().get("view"));
  }
}
