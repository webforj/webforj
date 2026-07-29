package com.webforj.devtools.craftforj.inspector.contribution.state.upload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class UploadPickerContributionTest {

  private final UploadPickerContribution contribution = new UploadPickerContribution();

  @Test
  void shouldGet() {
    Upload component = mock(Upload.class);
    when(component.getPicker()).thenReturn(Upload.Picker.DIRECTORY);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Picker", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.upload.Upload.Picker.DIRECTORY", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Upload component = mock(Upload.class);

    assertTrue(contribution.set(component, Upload.Picker.FILES));
    verify(component).setPicker(Upload.Picker.FILES);
  }

  @Test
  void shouldGetCorrectEnumClass() {
    Upload component = mock(Upload.class);
    assertEquals(Upload.Picker.class, contribution.findEnumClass(component));
  }
}
