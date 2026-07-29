package com.webforj.devtools.craftforj.inspector.contribution.content.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.appnav.AppNav;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class AppNavSearchEmptyMessageContributionTest {

  private final AppNavSearchEmptyMessageContribution contribution =
      new AppNavSearchEmptyMessageContribution();

  @Test
  void shouldGet() {
    AppNav component = mock(AppNav.class);
    AppNav.Search search = mock(AppNav.Search.class);
    when(component.getSearch()).thenReturn(search);
    when(search.getEmptyMessage()).thenReturn("No results");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("SearchEmptyMessage", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("No results", result.get().getValue());
  }

  @Test
  void shouldSet() {
    AppNav component = mock(AppNav.class);
    AppNav.Search search = mock(AppNav.Search.class);
    when(component.getSearch()).thenReturn(search);

    assertTrue(contribution.set(component, "Nothing found"));
    verify(search).setEmptyMessage("Nothing found");
  }

  @Test
  void shouldGenerateSourceThroughAccessor() {
    assertEquals("setEmptyMessage", contribution.getSourceMethodName("SearchEmptyMessage"));
    assertEquals("getSearch", contribution.getSourceAccessor());
  }
}
