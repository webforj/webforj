package com.webforj.devtools.craftforj.inspector.contribution.validation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class HasMaxFileSizeContributionTest {

  private final HasMaxFileSizeContribution contribution = new HasMaxFileSizeContribution();

  @Test
  void shouldGet() {
    Upload component = mock(Upload.class);
    when(component.getMaxFileSize()).thenReturn(1048576);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("MaxFileSize", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(1048576, result.get().getValue());
  }

  @Test
  void shouldSet() {
    Upload component = mock(Upload.class);

    assertTrue(contribution.set(component, 2048));
    verify(component).setMaxFileSize(2048);
  }
}
