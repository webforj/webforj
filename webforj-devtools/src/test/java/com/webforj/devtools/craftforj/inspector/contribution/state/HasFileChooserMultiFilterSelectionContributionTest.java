package com.webforj.devtools.craftforj.inspector.contribution.state;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.upload.Upload;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class HasFileChooserMultiFilterSelectionContributionTest {

  private final HasFileChooserMultiFilterSelectionContribution contribution =
      new HasFileChooserMultiFilterSelectionContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Upload component = mock(Upload.class);
    when(component.isMultiFilterSelection()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("MultiFilterSelection", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Upload component = mock(Upload.class);

    assertTrue(contribution.set(component, value));
    verify(component).setMultiFilterSelection(value);
  }
}
