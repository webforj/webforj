package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.flexlayout.FlexAlignment;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class FlexLayoutAlignItemsContributionTest {

  private final FlexLayoutAlignItemsContribution contribution =
      new FlexLayoutAlignItemsContribution();

  @Test
  void shouldGet() {
    FlexLayout layout = mock(FlexLayout.class);
    when(layout.getAlignment()).thenReturn(FlexAlignment.CENTER);

    var result = contribution.get(layout);

    assertTrue(result.isPresent());
    assertEquals("Alignment", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(FlexAlignment.class.getName() + ".CENTER", result.get().getValue());
  }

  @Test
  void shouldSet() {
    FlexLayout layout = mock(FlexLayout.class);

    assertTrue(contribution.set(layout, "BASELINE"));
    verify(layout).setAlignment(FlexAlignment.BASELINE);
  }
}
