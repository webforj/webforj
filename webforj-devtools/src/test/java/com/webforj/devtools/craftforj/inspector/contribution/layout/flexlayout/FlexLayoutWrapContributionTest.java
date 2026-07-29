package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.layout.flexlayout.FlexWrap;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class FlexLayoutWrapContributionTest {

  private final FlexLayoutWrapContribution contribution = new FlexLayoutWrapContribution();

  @Test
  void shouldGet() {
    FlexLayout layout = mock(FlexLayout.class);
    when(layout.getWrap()).thenReturn(FlexWrap.WRAP);

    var result = contribution.get(layout);

    assertTrue(result.isPresent());
    assertEquals("Wrap", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(FlexWrap.class.getName() + ".WRAP", result.get().getValue());
  }

  @Test
  void shouldSet() {
    FlexLayout layout = mock(FlexLayout.class);

    assertTrue(contribution.set(layout, "WRAP_REVERSE"));
    verify(layout).setWrap(FlexWrap.WRAP_REVERSE);
  }
}
