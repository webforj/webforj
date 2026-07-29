package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class FlexLayoutSpacingContributionTest {

  private final FlexLayoutSpacingContribution contribution = new FlexLayoutSpacingContribution();

  @Test
  void shouldGet() {
    FlexLayout layout = mock(FlexLayout.class);
    when(layout.getSpacing()).thenReturn("10px");

    var result = contribution.get(layout);

    assertTrue(result.isPresent());
    assertEquals("Spacing", result.get().getName());
    assertEquals(PropertyType.SIZE, result.get().getEditorType());
    assertEquals("10px", result.get().getValue());
  }

  @Test
  void shouldSet() {
    FlexLayout layout = mock(FlexLayout.class);

    assertTrue(contribution.set(layout, "20px"));
    verify(layout).setSpacing("20px");
  }
}
