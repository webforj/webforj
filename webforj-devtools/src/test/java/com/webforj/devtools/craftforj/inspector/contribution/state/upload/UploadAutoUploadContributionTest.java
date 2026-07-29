package com.webforj.devtools.craftforj.inspector.contribution.state.upload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class UploadAutoUploadContributionTest {

  private final UploadAutoUploadContribution contribution = new UploadAutoUploadContribution();

  @Test
  void shouldGet() {
    Upload component = mock(Upload.class);
    when(component.getAutoUpload()).thenReturn(Upload.AutoUpload.ON_SELECT);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("AutoUpload", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.upload.Upload.AutoUpload.ON_SELECT",
        result.get().getValue());
  }

  @Test
  void shouldSet() {
    Upload component = mock(Upload.class);

    assertTrue(contribution.set(component, Upload.AutoUpload.ALWAYS));
    verify(component).setAutoUpload(Upload.AutoUpload.ALWAYS);
  }

  @Test
  void shouldGetCorrectEnumClass() {
    Upload component = mock(Upload.class);
    assertEquals(Upload.AutoUpload.class, contribution.findEnumClass(component));
  }
}
