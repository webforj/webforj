package com.webforj.devtools.craftforj.inspector.contribution.appearance.selectdropdown;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.list.ComboBox;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class SelectDropdownOpenWidthContributionTest {

  private final SelectDropdownOpenWidthContribution contribution =
      new SelectDropdownOpenWidthContribution();

  @Test
  void shouldGet() {
    ComboBox component = mock(ComboBox.class);
    when(component.getOpenWidth()).thenReturn("200px");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("OpenWidth", result.get().getName());
    assertEquals(PropertyType.SIZE, result.get().getEditorType());
    assertEquals("200px", result.get().getValue());
  }

  @Test
  void shouldSet() {
    ComboBox component = mock(ComboBox.class);

    assertTrue(contribution.set(component, "300px"));
    verify(component).setOpenWidth("300px");
  }
}
