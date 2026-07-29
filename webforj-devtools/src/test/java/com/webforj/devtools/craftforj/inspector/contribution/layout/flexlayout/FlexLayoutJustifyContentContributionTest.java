package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.flexlayout.FlexJustifyContent;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class FlexLayoutJustifyContentContributionTest {

  private final FlexLayoutJustifyContentContribution contribution =
      new FlexLayoutJustifyContentContribution();

  @Test
  void shouldGet() {
    FlexLayout layout = mock(FlexLayout.class);
    when(layout.getJustifyContent()).thenReturn(FlexJustifyContent.CENTER);

    var result = contribution.get(layout);

    assertTrue(result.isPresent());
    assertEquals("JustifyContent", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(FlexJustifyContent.class.getName() + ".CENTER", result.get().getValue());
  }

  @Test
  void shouldSet() {
    FlexLayout layout = mock(FlexLayout.class);

    assertTrue(contribution.set(layout, "BETWEEN"));
    verify(layout).setJustifyContent(FlexJustifyContent.BETWEEN);
  }
}
