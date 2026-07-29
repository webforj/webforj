package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.component.layout.columnslayout.ColumnsLayout.Alignment;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class ColumnsLayoutHorizontalAlignmentContributionTest {

  private final ColumnsLayoutHorizontalAlignmentContribution contribution =
      new ColumnsLayoutHorizontalAlignmentContribution();

  @Test
  void shouldSupportColumnsLayout() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    assertTrue(contribution.supports(layout));
  }

  @Test
  void shouldGet() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    when(layout.getHorizontalAlignment()).thenReturn(Alignment.CENTER);

    var result = contribution.get(layout);

    assertTrue(result.isPresent());
    assertEquals("HorizontalAlignment", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(Alignment.class.getCanonicalName() + ".CENTER", result.get().getValue());
    assertEquals(6, ((java.util.List<?>) result.get().getEditorConfig().get("options")).size());
  }

  @Test
  void shouldNotSetNull() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    assertFalse(contribution.set(layout, null));
  }

  @Test
  void shouldNotSetEmpty() {
    ColumnsLayout layout = mock(ColumnsLayout.class);
    assertFalse(contribution.set(layout, ""));
  }
}
