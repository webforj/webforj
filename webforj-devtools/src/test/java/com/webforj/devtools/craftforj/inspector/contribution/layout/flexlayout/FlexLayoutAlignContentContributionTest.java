package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.flexlayout.FlexContentAlignment;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class FlexLayoutAlignContentContributionTest {

  private final FlexLayoutAlignContentContribution contribution =
      new FlexLayoutAlignContentContribution();

  @Test
  void shouldGet() {
    FlexLayout layout = mock(FlexLayout.class);
    when(layout.getAlignContent()).thenReturn(FlexContentAlignment.BETWEEN);

    var result = contribution.get(layout);

    assertTrue(result.isPresent());
    assertEquals("AlignContent", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(FlexContentAlignment.class.getName() + ".BETWEEN", result.get().getValue());
  }

  @Test
  void shouldSet() {
    FlexLayout layout = mock(FlexLayout.class);

    assertTrue(contribution.set(layout, "AROUND"));
    verify(layout).setAlignContent(FlexContentAlignment.AROUND);
  }
}
