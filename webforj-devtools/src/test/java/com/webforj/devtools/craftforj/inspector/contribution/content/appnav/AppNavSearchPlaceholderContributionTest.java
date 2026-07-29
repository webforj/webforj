package com.webforj.devtools.craftforj.inspector.contribution.content.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.appnav.AppNav;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class AppNavSearchPlaceholderContributionTest {

  private final AppNavSearchPlaceholderContribution contribution =
      new AppNavSearchPlaceholderContribution();

  @Test
  void shouldGet() {
    AppNav component = mock(AppNav.class);
    AppNav.Search search = mock(AppNav.Search.class);
    when(component.getSearch()).thenReturn(search);
    when(search.getPlaceholder()).thenReturn("Find pages");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("SearchPlaceholder", result.get().getName());
    assertEquals(PropertyType.TEXT, result.get().getEditorType());
    assertEquals("Find pages", result.get().getValue());
  }

  @Test
  void shouldSet() {
    AppNav component = mock(AppNav.class);
    AppNav.Search search = mock(AppNav.Search.class);
    when(component.getSearch()).thenReturn(search);

    assertTrue(contribution.set(component, "Search menu"));
    verify(search).setPlaceholder("Search menu");
  }

  @Test
  void shouldGenerateSourceThroughAccessor() {
    assertEquals("setPlaceholder", contribution.getSourceMethodName("SearchPlaceholder"));
    assertEquals("getSearch", contribution.getSourceAccessor());
  }
}
