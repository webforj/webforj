package com.webforj.devtools.craftforj.inspector.contribution.content.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.list.ComboBox;
import com.webforj.component.list.DwcList;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class ListSearchPlaceholderContributionTest {

  private final ListSearchPlaceholderContribution contribution =
      new ListSearchPlaceholderContribution();

  @Test
  void shouldGet() {
    ComboBox component = mock(ComboBox.class);
    DwcList.Search search = mock(DwcList.Search.class);
    when(component.getSearch()).thenReturn(search);
    when(search.getPlaceholder()).thenReturn("Type to filter");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("SearchPlaceholder", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("Type to filter", result.get().getValue());
  }

  @Test
  void shouldSet() {
    ComboBox component = mock(ComboBox.class);
    DwcList.Search search = mock(DwcList.Search.class);
    when(component.getSearch()).thenReturn(search);

    assertTrue(contribution.set(component, "Filter items"));
    verify(search).setPlaceholder("Filter items");
  }

  @Test
  void shouldGenerateSourceThroughAccessor() {
    assertEquals("setPlaceholder", contribution.getSourceMethodName("SearchPlaceholder"));
    assertEquals("getSearch", contribution.getSourceAccessor());
  }
}
