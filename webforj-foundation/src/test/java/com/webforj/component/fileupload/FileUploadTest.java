package com.webforj.component.fileupload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import com.basis.bbj.proxies.sysgui.BBjFileChooser;
import com.basis.startup.type.BBjException;
import com.webforj.component.ReflectionUtils;
import com.webforj.component.fileupload.event.FileUploadCancelEvent;
import com.webforj.component.fileupload.event.FileUploadChangeEvent;
import com.webforj.component.fileupload.event.FileUploadEvent;
import com.webforj.component.fileupload.event.FileUploadFilterChangeEvent;
import com.webforj.component.optiondialog.FileChooserFilter;
import com.webforj.component.optiondialog.FileUploadI18n;
import com.webforj.dispatcher.EventListener;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class FileUploadTest {

  @Mock
  BBjFileChooser control;

  @InjectMocks
  FileUpload component = new FileUpload();

  @Nested
  class Constructors {

    @Test
    void shouldCreateWithDefaults() {
      FileUpload fresh = new FileUpload();
      assertNotNull(fresh.getI18n());
      assertEquals(0, fresh.getFilters().size());
      assertNull(fresh.getActiveFilter());
      assertEquals(FileUpload.SelectionMode.MULTIPLE, fresh.getSelectionMode());
      assertEquals(FileUpload.Picker.FILES, fresh.getPicker());
      assertTrue(fresh.isDrop());
      assertTrue(fresh.isFiltersVisible());
      assertFalse(fresh.isMultiFilterSelection());
      assertTrue(fresh.isFileSystemAccess());
      assertTrue(fresh.isAllFilesFilterEnabled());
      assertNull(fresh.getMaxFileSize());
    }

    @Test
    void shouldCreateWithFilters() {
      List<FileChooserFilter> filters = Arrays.asList(new FileChooserFilter("Text", "*.txt"),
          new FileChooserFilter("Images", "*.png"));

      FileUpload fresh = new FileUpload(filters);

      assertEquals(2, fresh.getFilters().size());
      assertEquals("Text", fresh.getFilters().get(0).getDescription());
      assertEquals("Images", fresh.getFilters().get(1).getDescription());
    }

    @Test
    void shouldCreateWithChangeListener() {
      EventListener<FileUploadChangeEvent> listener = ev -> {
      };

      FileUpload fresh = new FileUpload(listener);

      assertEquals(1, fresh.getEventListeners(FileUploadChangeEvent.class).size());
    }

    @Test
    void shouldIgnoreNullChangeListener() {
      FileUpload fresh = new FileUpload((EventListener<FileUploadChangeEvent>) null);

      assertEquals(0, fresh.getEventListeners(FileUploadChangeEvent.class).size());
    }
  }

  @Nested
  class Filters {

    @Test
    void shouldAddFilterAndCallBbj() throws BBjException {
      FileChooserFilter filter = new FileChooserFilter("Text", "*.txt");

      component.addFilter(filter);

      assertEquals(1, component.getFilters().size());
      assertSame(filter, component.getFilters().get(0));
      verify(control, times(1)).addFileFilter("Text", "*.txt");
    }

    @Test
    void shouldAddFilterByDescriptionAndPattern() throws BBjException {
      component.addFilter("Text", "*.txt");

      assertEquals(1, component.getFilters().size());
      verify(control, times(1)).addFileFilter("Text", "*.txt");
    }

    @Test
    void shouldIgnoreNullFilter() {
      component.addFilter((FileChooserFilter) null);

      assertEquals(0, component.getFilters().size());
      verifyNoInteractions(control);
    }

    @Test
    void shouldRemoveFilterAndCallBbj() throws BBjException {
      FileChooserFilter filter = new FileChooserFilter("Text", "*.txt");
      component.addFilter(filter);

      component.removeFilter(filter);

      assertEquals(0, component.getFilters().size());
      verify(control, times(1)).removeFileFilter("Text");
    }

    @Test
    void shouldRemoveFilterByDescription() throws BBjException {
      component.addFilter("Text", "*.txt");
      component.addFilter("Images", "*.png");

      component.removeFilter("Text");

      assertEquals(1, component.getFilters().size());
      assertEquals("Images", component.getFilters().get(0).getDescription());
      verify(control, times(1)).removeFileFilter("Text");
    }

    @Test
    void shouldIgnoreUnknownFilterRemoval() {
      component.removeFilter("Missing");
      component.removeFilter((FileChooserFilter) null);

      verifyNoInteractions(control);
    }

    @Test
    void shouldClearActiveFilterWhenItIsRemoved() {
      FileChooserFilter filter = new FileChooserFilter("Text", "*.txt");
      component.addFilter(filter);
      component.setActiveFilter(filter);

      component.removeFilter(filter);

      assertNull(component.getActiveFilter());
    }

    @Test
    void shouldReplaceFiltersWhenSetFiltersIsCalled() throws BBjException {
      component.addFilter("Text", "*.txt");
      component.setActiveFilter("Text");

      component.setFilters(Arrays.asList(new FileChooserFilter("Images", "*.png")));

      assertEquals(1, component.getFilters().size());
      assertEquals("Images", component.getFilters().get(0).getDescription());
      assertNull(component.getActiveFilter());
      verify(control, times(1)).removeFileFilter("Text");
      verify(control, times(1)).addFileFilter("Images", "*.png");
    }

    @Test
    void shouldClearFiltersWhenSetFiltersReceivesNull() {
      component.addFilter("Text", "*.txt");

      component.setFilters(null);

      assertEquals(0, component.getFilters().size());
    }

    @Test
    void shouldExposeImmutableFilterList() {
      component.addFilter("Text", "*.txt");
      List<FileChooserFilter> view = component.getFilters();

      assertThrows(UnsupportedOperationException.class,
          () -> view.add(new FileChooserFilter("X", "*.x")));
    }

    @Test
    void shouldSetActiveFilterByReference() throws BBjException {
      FileChooserFilter filter = new FileChooserFilter("Text", "*.txt");
      component.addFilter(filter);

      component.setActiveFilter(filter);

      assertSame(filter, component.getActiveFilter());
      verify(control, times(1)).setActiveFileFilter("Text");
    }

    @Test
    void shouldSetActiveFilterByDescription() throws BBjException {
      component.addFilter("Text", "*.txt");

      component.setActiveFilter("Text");

      assertNotNull(component.getActiveFilter());
      assertEquals("Text", component.getActiveFilter().getDescription());
      verify(control, times(1)).setActiveFileFilter("Text");
    }

    @Test
    void shouldClearActiveFilterWhenSetToNull() throws BBjException {
      component.addFilter("Text", "*.txt");
      component.setActiveFilter("Text");

      component.setActiveFilter((FileChooserFilter) null);

      assertNull(component.getActiveFilter());
      verify(control, times(1)).setActiveFileFilter("");
    }

    @Test
    void shouldStoreFiltersWhenDetached() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.addFilter("Text", "*.txt");
      component.setActiveFilter("Text");

      assertEquals(1, component.getFilters().size());
      assertEquals("Text", component.getActiveFilter().getDescription());
      verifyNoInteractions(control);
    }

    @Test
    void shouldKeepActiveFilterWhenDifferentFilterIsRemoved() {
      FileChooserFilter active = new FileChooserFilter("Text", "*.txt");
      FileChooserFilter other = new FileChooserFilter("Images", "*.png");
      component.addFilter(active);
      component.addFilter(other);
      component.setActiveFilter(active);

      component.removeFilter(other);

      assertSame(active, component.getActiveFilter());
    }

    @Test
    void shouldReturnNullActiveFilterWhenDescriptionDoesNotExist() {
      component.addFilter("Text", "*.txt");

      component.setActiveFilter("Missing");

      assertNull(component.getActiveFilter());
    }

    @Test
    void shouldClearActiveFilterWhenDescriptionIsNull() {
      component.addFilter("Text", "*.txt");
      component.setActiveFilter("Text");

      component.setActiveFilter((String) null);

      assertNull(component.getActiveFilter());
    }

    @Test
    void shouldHandleFilterWithNullDescriptionAndPattern() throws BBjException {
      FileChooserFilter loose = new FileChooserFilter(null, null);

      component.addFilter(loose);

      assertEquals(1, component.getFilters().size());
      verify(control, times(1)).addFileFilter("", "");
    }
  }

  @Nested
  class AllFilesFilter {

    @Test
    void shouldDefaultToTrue() {
      assertTrue(component.isAllFilesFilterEnabled());
    }

    @Test
    void shouldRoundTripValue() {
      component.setAllFilesFilterEnabled(false);

      assertFalse(component.isAllFilesFilterEnabled());
    }
  }

  @Nested
  class Drop {

    @Test
    void shouldDefaultToTrue() {
      assertTrue(component.isDrop());
    }

    @Test
    void shouldRoundTripValue() {
      component.setDrop(false);

      assertFalse(component.isDrop());
    }
  }

  @Nested
  class FiltersVisible {

    @Test
    void shouldDefaultToTrue() {
      assertTrue(component.isFiltersVisible());
    }

    @Test
    void shouldRoundTripValue() {
      component.setFiltersVisible(false);

      assertFalse(component.isFiltersVisible());
    }
  }

  @Nested
  class MultiFilterSelection {

    @Test
    void shouldDefaultToFalse() {
      assertFalse(component.isMultiFilterSelection());
    }

    @Test
    void shouldRoundTripValue() {
      component.setMultiFilterSelection(true);

      assertTrue(component.isMultiFilterSelection());
    }
  }

  @Nested
  class MaxFileSize {

    @Test
    void shouldDefaultToNull() {
      assertNull(component.getMaxFileSize());
    }

    @Test
    void shouldStoreValue() {
      component.setMaxFileSize(1024);

      assertEquals(1024, component.getMaxFileSize());
    }

    @Test
    void shouldRejectNull() {
      assertThrows(NullPointerException.class, () -> component.setMaxFileSize(null));
    }

    @Test
    void shouldKeepPreviousValueWhenNullRejected() {
      component.setMaxFileSize(1024);

      assertThrows(NullPointerException.class, () -> component.setMaxFileSize(null));
      assertEquals(1024, component.getMaxFileSize());
    }
  }

  @Nested
  class MaxFiles {

    @Test
    void shouldDefaultToNull() {
      assertNull(component.getMaxFiles());
    }

    @Test
    void shouldStoreValue() {
      component.setMaxFiles(5);

      assertEquals(5, component.getMaxFiles());
    }

    @Test
    void shouldRejectNull() {
      assertThrows(NullPointerException.class, () -> component.setMaxFiles(null));
    }

    @Test
    void shouldKeepPreviousValueWhenNullRejected() {
      component.setMaxFiles(5);

      assertThrows(NullPointerException.class, () -> component.setMaxFiles(null));
      assertEquals(5, component.getMaxFiles());
    }
  }

  @Nested
  class FileSystemAccess {

    @Test
    void shouldDefaultToTrue() {
      assertTrue(component.isFileSystemAccess());
    }

    @Test
    void shouldRoundTripValue() {
      component.setFileSystemAccess(false);

      assertFalse(component.isFileSystemAccess());
    }
  }

  @Nested
  class PartVisibility {

    @Test
    void shouldDefaultUploadButtonToVisible() {
      assertTrue(component.isVisible(FileUpload.Part.UPLOAD_BUTTON));
    }

    @Test
    void shouldDefaultCancelButtonToVisible() {
      assertTrue(component.isVisible(FileUpload.Part.CANCEL_BUTTON));
    }

    @Test
    void shouldHideUploadButton() {
      component.setVisible(false, FileUpload.Part.UPLOAD_BUTTON);

      assertFalse(component.isVisible(FileUpload.Part.UPLOAD_BUTTON));
      assertTrue(component.isVisible(FileUpload.Part.CANCEL_BUTTON));
    }

    @Test
    void shouldHideCancelButton() {
      component.setVisible(false, FileUpload.Part.CANCEL_BUTTON);

      assertFalse(component.isVisible(FileUpload.Part.CANCEL_BUTTON));
      assertTrue(component.isVisible(FileUpload.Part.UPLOAD_BUTTON));
    }

    @Test
    void shouldRejectNullPart() {
      assertThrows(NullPointerException.class, () -> component.setVisible(false, null));
      assertThrows(NullPointerException.class, () -> component.isVisible(null));
    }
  }

  @Nested
  class SelectionModeBehavior {

    @Test
    void shouldDefaultToMultiple() {
      assertEquals(FileUpload.SelectionMode.MULTIPLE, component.getSelectionMode());
    }

    @Test
    void shouldCallBbjWhenAttached() throws BBjException {
      component.setSelectionMode(FileUpload.SelectionMode.SINGLE);

      assertEquals(FileUpload.SelectionMode.SINGLE, component.getSelectionMode());
      verify(control, times(1)).setMultiSelectionEnabled(false);
    }

    @Test
    void shouldFallBackToMultipleWhenNull() {
      component.setSelectionMode(null);

      assertEquals(FileUpload.SelectionMode.MULTIPLE, component.getSelectionMode());
    }

    @Test
    void shouldStoreValueWhenDetached() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setSelectionMode(FileUpload.SelectionMode.SINGLE);

      assertEquals(FileUpload.SelectionMode.SINGLE, component.getSelectionMode());
      verifyNoInteractions(control);
    }
  }

  @Nested
  class PickerBehavior {

    @Test
    void shouldDefaultToFiles() {
      assertEquals(FileUpload.Picker.FILES, component.getPicker());
    }

    @Test
    void shouldCallBbjWithFilesOnlyForFilesMode() throws BBjException {
      component.setPicker(FileUpload.Picker.FILES);

      assertEquals(FileUpload.Picker.FILES, component.getPicker());
      verify(control, times(1)).setFileSelectionMode(BBjFileChooser.FILES_ONLY);
    }

    @Test
    void shouldCallBbjWithDirectoriesOnlyForDirectoryMode() throws BBjException {
      component.setPicker(FileUpload.Picker.DIRECTORY);

      assertEquals(FileUpload.Picker.DIRECTORY, component.getPicker());
      verify(control, times(1)).setFileSelectionMode(BBjFileChooser.DIRECTORIES_ONLY);
    }

    @Test
    void shouldCallBbjWithDirectoriesOnlyForRecursiveMode() throws BBjException {
      component.setPicker(FileUpload.Picker.DIRECTORY_RECURSIVE);

      assertEquals(FileUpload.Picker.DIRECTORY_RECURSIVE, component.getPicker());
      verify(control, times(1)).setFileSelectionMode(BBjFileChooser.DIRECTORIES_ONLY);
    }

    @Test
    void shouldFallBackToFilesWhenNull() {
      component.setPicker(null);

      assertEquals(FileUpload.Picker.FILES, component.getPicker());
    }

    @Test
    void shouldStoreValueWhenDetached() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setPicker(FileUpload.Picker.DIRECTORY_RECURSIVE);

      assertEquals(FileUpload.Picker.DIRECTORY_RECURSIVE, component.getPicker());
      verifyNoInteractions(control);
    }
  }

  @Nested
  class I18n {

    @Test
    void shouldExposeNonNullDefault() {
      assertNotNull(component.getI18n());
    }

    @Test
    void shouldStoreNewBundle() {
      FileUploadI18n bundle = new FileUploadI18n();

      component.setI18n(bundle);

      assertSame(bundle, component.getI18n());
    }

    @Test
    void shouldFallBackToFreshBundleWhenNull() {
      FileUploadI18n previous = component.getI18n();

      component.setI18n(null);

      assertNotNull(component.getI18n());
      assertNotSame(previous, component.getI18n());
    }
  }

  @Nested
  class ProgrammaticActions {

    @Test
    void shouldUploadSelectionViaBbj() throws BBjException {
      component.upload();

      verify(control, times(1)).approveSelection();
    }

    @Test
    void shouldCancelSelectionViaBbj() throws BBjException {
      component.cancel();

      verify(control, times(1)).cancelSelection();
    }

    @Test
    void shouldNoOpUploadWhenDetached() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.upload();

      verifyNoInteractions(control);
    }

    @Test
    void shouldNoOpCancelWhenDetached() throws IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.cancel();

      verifyNoInteractions(control);
    }
  }

  @Nested
  class EventListeners {

    @Test
    void shouldRegisterChangeListener() {
      component.addChangeListener(ev -> {
      });
      component.onChange(ev -> {
      });

      assertEquals(2, component.getEventListeners(FileUploadChangeEvent.class).size());
    }

    @Test
    void shouldRegisterUploadListener() {
      component.addUploadListener(ev -> {
      });
      component.onUpload(ev -> {
      });

      assertEquals(2, component.getEventListeners(FileUploadEvent.class).size());
    }

    @Test
    void shouldRegisterCancelListener() {
      component.addCancelListener(ev -> {
      });
      component.onCancel(ev -> {
      });

      assertEquals(2, component.getEventListeners(FileUploadCancelEvent.class).size());
    }

    @Test
    void shouldRegisterFilterChangeListener() {
      component.addFilterChangeListener(ev -> {
      });
      component.onFilterChange(ev -> {
      });

      assertEquals(2, component.getEventListeners(FileUploadFilterChangeEvent.class).size());
    }
  }

  @Nested
  class OnAttachReplay {

    @Test
    void shouldReapplyFiltersSelectionModeAndPicker() throws BBjException, IllegalAccessException {
      ReflectionUtils.nullifyControl(component);

      component.setSelectionMode(FileUpload.SelectionMode.SINGLE);
      component.setPicker(FileUpload.Picker.DIRECTORY_RECURSIVE);
      component.addFilter("Text", "*.txt");
      component.setActiveFilter("Text");
      verifyNoInteractions(control);

      ReflectionUtils.unNullifyControl(component, control);
      component.onAttach();

      verify(control, times(1)).setMultiSelectionEnabled(false);
      verify(control, times(1)).setFileSelectionMode(BBjFileChooser.DIRECTORIES_ONLY);
      verify(control, times(1)).addFileFilter("Text", "*.txt");
      verify(control, times(1)).setActiveFileFilter("Text");
    }
  }
}
