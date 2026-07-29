package com.webforj.devtools.craftforj.inspector.contribution.validation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasMaxFilesContributionTest {

  private final HasMaxFilesContribution contribution = new HasMaxFilesContribution();

  @Test
  void shouldGet() {
    Upload component = mock(Upload.class);
    when(component.getMaxFiles()).thenReturn(5);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("MaxFiles", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(5, result.get().getValue());
  }

  @Test
  void shouldSet() {
    Upload component = mock(Upload.class);

    assertTrue(contribution.set(component, 10));
    verify(component).setMaxFiles(10);
  }
}
