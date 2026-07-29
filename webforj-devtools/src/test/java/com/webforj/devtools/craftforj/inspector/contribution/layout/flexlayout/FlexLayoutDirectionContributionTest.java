package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.flexlayout.FlexDirection;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class FlexLayoutDirectionContributionTest {

  private final FlexLayoutDirectionContribution contribution =
      new FlexLayoutDirectionContribution();

  @Test
  void shouldSupportFlexLayout() {
    FlexLayout layout = mock(FlexLayout.class);
    assertTrue(contribution.supports(layout));
  }

  @Test
  void shouldGet() {
    FlexLayout layout = mock(FlexLayout.class);
    when(layout.getDirection()).thenReturn(FlexDirection.COLUMN);

    var result = contribution.get(layout);

    assertTrue(result.isPresent());
    assertEquals("Direction", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(FlexDirection.class.getName() + ".COLUMN", result.get().getValue());
    assertEquals(4, ((java.util.List<?>) result.get().getEditorConfig().get("options")).size());
  }

  @Test
  void shouldNotSetNull() {
    FlexLayout layout = mock(FlexLayout.class);
    assertFalse(contribution.set(layout, null));
  }

  @Test
  void shouldNotSetEmpty() {
    FlexLayout layout = mock(FlexLayout.class);
    assertFalse(contribution.set(layout, ""));
  }
}
