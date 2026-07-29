package com.webforj.devtools.craftforj.inspector.contribution.state.upload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class UploadSelectionModeContributionTest {

  private final UploadSelectionModeContribution contribution =
      new UploadSelectionModeContribution();

  @Test
  void shouldGet() {
    Upload component = mock(Upload.class);
    when(component.getSelectionMode()).thenReturn(Upload.SelectionMode.MULTIPLE);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("SelectionMode", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.upload.Upload.SelectionMode.MULTIPLE",
        result.get().getValue());
  }

  @Test
  void shouldSet() {
    Upload component = mock(Upload.class);

    assertTrue(contribution.set(component, Upload.SelectionMode.SINGLE));
    verify(component).setSelectionMode(Upload.SelectionMode.SINGLE);
  }

  @Test
  void shouldGetCorrectEnumClass() {
    Upload component = mock(Upload.class);
    assertEquals(Upload.SelectionMode.class, contribution.findEnumClass(component));
  }
}
