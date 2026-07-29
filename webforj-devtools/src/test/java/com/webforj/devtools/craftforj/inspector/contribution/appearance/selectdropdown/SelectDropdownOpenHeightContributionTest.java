package com.webforj.devtools.craftforj.inspector.contribution.appearance.selectdropdown;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.list.ComboBox;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class SelectDropdownOpenHeightContributionTest {

  private final SelectDropdownOpenHeightContribution contribution =
      new SelectDropdownOpenHeightContribution();

  @Test
  void shouldGet() {
    ComboBox component = mock(ComboBox.class);
    when(component.getOpenHeight()).thenReturn("400px");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("OpenHeight", result.get().getName());
    assertEquals(PropertyType.SIZE, result.get().getEditorType());
    assertEquals("400px", result.get().getValue());
  }

  @Test
  void shouldSet() {
    ComboBox component = mock(ComboBox.class);

    assertTrue(contribution.set(component, "500px"));
    verify(component).setOpenHeight("500px");
  }
}
