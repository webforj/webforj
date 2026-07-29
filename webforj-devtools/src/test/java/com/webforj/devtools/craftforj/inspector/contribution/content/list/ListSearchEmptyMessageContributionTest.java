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

class ListSearchEmptyMessageContributionTest {

  private final ListSearchEmptyMessageContribution contribution =
      new ListSearchEmptyMessageContribution();

  @Test
  void shouldGet() {
    ComboBox component = mock(ComboBox.class);
    DwcList.Search search = mock(DwcList.Search.class);
    when(component.getSearch()).thenReturn(search);
    when(search.getEmptyMessage()).thenReturn("No matches");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("SearchEmptyMessage", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("No matches", result.get().getValue());
  }

  @Test
  void shouldSet() {
    ComboBox component = mock(ComboBox.class);
    DwcList.Search search = mock(DwcList.Search.class);
    when(component.getSearch()).thenReturn(search);

    assertTrue(contribution.set(component, "Nothing here"));
    verify(search).setEmptyMessage("Nothing here");
  }

  @Test
  void shouldGenerateSourceThroughAccessor() {
    assertEquals("setEmptyMessage", contribution.getSourceMethodName("SearchEmptyMessage"));
    assertEquals("getSearch", contribution.getSourceAccessor());
  }
}
