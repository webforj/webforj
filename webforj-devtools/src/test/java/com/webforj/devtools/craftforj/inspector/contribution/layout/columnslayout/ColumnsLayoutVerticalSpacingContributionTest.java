package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class ColumnsLayoutVerticalSpacingContributionTest {

  private final ColumnsLayoutVerticalSpacingContribution contribution =
      new ColumnsLayoutVerticalSpacingContribution();

  @Test
  void shouldSupportColumnsLayout() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    assertTrue(contribution.supports(layout));
  }

  @Test
  void shouldGet() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    when(layout.getVerticalSpacing()).thenReturn("1.5rem");

    var result = contribution.get(layout);

    assertTrue(result.isPresent());
    assertEquals("VerticalSpacing", result.get().getName());
    assertEquals(PropertyType.SIZE, result.get().getEditorType());
    assertEquals("1.5rem", result.get().getValue());
  }

  @Test
  void shouldSet() {
    ColumnsLayout layout = mock(ColumnsLayout.class);

    assertTrue(contribution.set(layout, "30px"));
    verify(layout).setVerticalSpacing("30px");
  }
}
