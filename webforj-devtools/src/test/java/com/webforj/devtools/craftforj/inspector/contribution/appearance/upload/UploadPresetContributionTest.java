package com.webforj.devtools.craftforj.inspector.contribution.appearance.upload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class UploadPresetContributionTest {

  private final UploadPresetContribution contribution = new UploadPresetContribution();

  @Test
  void shouldGet() {
    Upload component = mock(Upload.class);
    when(component.getPreset()).thenReturn(Upload.Preset.DROPZONE);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Preset", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.upload.Upload.Preset.DROPZONE", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Upload component = mock(Upload.class);

    assertTrue(contribution.set(component, Upload.Preset.INLINE));
    verify(component).setPreset(Upload.Preset.INLINE);
  }

  @Test
  void shouldGetCorrectEnumClass() {
    Upload component = mock(Upload.class);
    assertEquals(Upload.Preset.class, contribution.findEnumClass(component));
  }
}
