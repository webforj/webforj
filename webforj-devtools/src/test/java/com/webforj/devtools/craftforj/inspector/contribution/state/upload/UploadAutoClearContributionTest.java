package com.webforj.devtools.craftforj.inspector.contribution.state.upload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class UploadAutoClearContributionTest {

  private final UploadAutoClearContribution contribution = new UploadAutoClearContribution();

  @Test
  void shouldGet() {
    Upload component = mock(Upload.class);
    when(component.getAutoClear()).thenReturn(Upload.AutoClear.COMPLETED);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("AutoClear", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.upload.Upload.AutoClear.COMPLETED",
        result.get().getValue());
  }

  @Test
  void shouldSet() {
    Upload component = mock(Upload.class);

    assertTrue(contribution.set(component, Upload.AutoClear.ALL));
    verify(component).setAutoClear(Upload.AutoClear.ALL);
  }

  @Test
  void shouldGetCorrectEnumClass() {
    Upload component = mock(Upload.class);
    assertEquals(Upload.AutoClear.class, contribution.findEnumClass(component));
  }
}
