package com.webforj.devtools.craftforj.inspector.contribution.appearance.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.list.ComboBox;
import com.webforj.component.list.DwcList;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ListSearchFieldVisibleContributionTest {

  private final ListSearchFieldVisibleContribution contribution =
      new ListSearchFieldVisibleContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    ComboBox component = mock(ComboBox.class);
    DwcList.Search search = mock(DwcList.Search.class);
    when(component.getSearch()).thenReturn(search);
    when(search.isFieldVisible()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("SearchFieldVisible", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    ComboBox component = mock(ComboBox.class);
    DwcList.Search search = mock(DwcList.Search.class);
    when(component.getSearch()).thenReturn(search);

    assertTrue(contribution.set(component, value));
    verify(search).setFieldVisible(value);
  }

  @Test
  void shouldGenerateSourceThroughAccessor() {
    assertEquals("setFieldVisible", contribution.getSourceMethodName("SearchFieldVisible"));
    assertEquals("getSearch", contribution.getSourceAccessor());
  }
}
