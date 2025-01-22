package com.webforj.component.optiondialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.Arrays;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class FileSaveDialogTest {

  FileSaveDialog component;
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
      component = new FileSaveDialog("Title", "/path", "defaultName",
          Arrays.asList(mockFilter1, mockFilter2), "ActiveFilter", true,
          FileChooserDialog.SelectionMode.FILES_AND_DIRECTORIES);
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals("defaultName", component.getName());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), component.getFilters());
      assertEquals(true, component.isRestricted());
      assertEquals(FileChooserDialog.SelectionMode.FILES_AND_DIRECTORIES,
          component.getSelectionMode());
    }

    @Test
    void shouldConstructWithTitleInitialPathNameAndFilters() {
      component = new FileSaveDialog("Title", "/path", "defaultName",
          Arrays.asList(mockFilter1, mockFilter2));
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals("defaultName", component.getName());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), component.getFilters());
    }

    @Test
    void shouldConstructWithTitleInitialPathNameAndSelectionMode() {
      component = new FileSaveDialog("Title", "/path", "defaultName",
          FileChooserDialog.SelectionMode.DIRECTORIES);
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals("defaultName", component.getName());
      assertEquals(FileChooserDialog.SelectionMode.DIRECTORIES, component.getSelectionMode());
    }

    @Test
    void shouldConstructWithTitleInitialPathNameFiltersAndActiveFilter() {
      component = new FileSaveDialog("Title", "/path", "defaultName",
          Arrays.asList(mockFilter1, mockFilter2), "ActiveFilter");
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals("defaultName", component.getName());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), component.getFilters());
    }

    @Test
    void shouldConstructWithTitleInitialPathNameFiltersAndRestricted() {
      component = new FileSaveDialog("Title", "/path", "defaultName",
          Arrays.asList(mockFilter1, mockFilter2), true);
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals("defaultName", component.getName());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), component.getFilters());
      assertTrue(component.isRestricted());
    }

    @Test
    void shouldConstructWithTitleInitialPathNameAndRestricted() {
      component = new FileSaveDialog("Title", "/path", "defaultName", true);
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals("defaultName", component.getName());
      assertTrue(component.isRestricted());
    }

    @Test
    void shouldConstructWithTitleInitialPathNameFiltersRestrictedAndSelectionMode() {
      component = new FileSaveDialog("Title", "/path", "defaultName",
          Arrays.asList(mockFilter1, mockFilter2), true,
          FileChooserDialog.SelectionMode.DIRECTORIES);
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals("defaultName", component.getName());
      assertEquals(Arrays.asList(mockFilter1, mockFilter2), component.getFilters());
      assertTrue(component.isRestricted());
      assertEquals(FileChooserDialog.SelectionMode.DIRECTORIES, component.getSelectionMode());
    }

    @Test
    void shouldConstructWithTitleInitialPathAndName() {
      component = new FileSaveDialog("Title", "/path", "defaultName");
      assertEquals("Title", component.getTitle());
      assertEquals("/path", component.getInitialPath());
      assertEquals("defaultName", component.getName());
    }

    @Test
    void shouldConstructWithTitleAndName() {
      component = new FileSaveDialog("Title", "defaultName");
      assertEquals("Title", component.getTitle());
      assertEquals("defaultName", component.getName());
    }

    @Test
    void shouldConstructWithDefaultTitleAndName() {
      component = new FileSaveDialog("Title");
      assertEquals("Title", component.getTitle());
    }

    @Test
    void shouldConstructWithDefaultTitle() {
      component = new FileSaveDialog();
      assertEquals(FileSaveDialog.DEFAULT_TITLE, component.getTitle());
    }
  }
}
