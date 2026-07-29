package com.webforj.devtools.craftforj.inspector.contribution.state.upload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class UploadAllFilesFilterEnabledContributionTest {

  private final UploadAllFilesFilterEnabledContribution contribution =
      new UploadAllFilesFilterEnabledContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Upload component = mock(Upload.class);
    when(component.isAllFilesFilterEnabled()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("AllFilesFilterEnabled", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Upload component = mock(Upload.class);

    assertTrue(contribution.set(component, value));
    verify(component).setAllFilesFilterEnabled(value);
  }
}
