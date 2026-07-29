package com.webforj.devtools.craftforj.inspector.contribution.state.upload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class UploadCaptureContributionTest {

  private final UploadCaptureContribution contribution = new UploadCaptureContribution();

  @Test
  void shouldGet() {
    Upload component = mock(Upload.class);
    when(component.getCapture()).thenReturn(Upload.Capture.USER);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Capture", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.upload.Upload.Capture.USER", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Upload component = mock(Upload.class);

    assertTrue(contribution.set(component, Upload.Capture.ENVIRONMENT));
    verify(component).setCapture(Upload.Capture.ENVIRONMENT);
  }

  @Test
  void shouldGetCorrectEnumClass() {
    Upload component = mock(Upload.class);
    assertEquals(Upload.Capture.class, contribution.findEnumClass(component));
  }
}
