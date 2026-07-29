package com.webforj.devtools.craftforj.inspector.contribution.appearance.selectdropdown;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.list.ComboBox;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class SelectDropdownMaxRowCountContributionTest {

  private final SelectDropdownMaxRowCountContribution contribution =
      new SelectDropdownMaxRowCountContribution();

  @Test
  void shouldGet() {
    ComboBox component = mock(ComboBox.class);
    when(component.getMaxRowCount()).thenReturn(10);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("MaxRowCount", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(10, result.get().getValue());
  }

  @Test
  void shouldSet() {
    ComboBox component = mock(ComboBox.class);

    assertTrue(contribution.set(component, 15));
    verify(component).setMaxRowCount(15);
  }
}
