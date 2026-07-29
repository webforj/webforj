package com.webforj.devtools.craftforj.inspector.contribution.state.listbox;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.list.ListBox;
import com.webforj.component.list.MultipleSelectableList;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class ListBoxSelectionModeContributionTest {

  private final ListBoxSelectionModeContribution contribution =
      new ListBoxSelectionModeContribution();

  @Test
  void shouldGet() {
    ListBox component = mock(ListBox.class);
    when(component.getSelectionMode()).thenReturn(MultipleSelectableList.SelectionMode.MULTIPLE);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("SelectionMode", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals("com.webforj.component.list.MultipleSelectableList.SelectionMode.MULTIPLE",
        result.get().getValue());
  }

  @Test
  void shouldSet() {
    ListBox component = mock(ListBox.class);

    assertTrue(contribution.set(component, MultipleSelectableList.SelectionMode.MULTIPLE));
    verify(component).setSelectionMode(MultipleSelectableList.SelectionMode.MULTIPLE);
  }

  @Test
  void shouldSetSingle() {
    ListBox component = mock(ListBox.class);

    assertTrue(contribution.set(component, MultipleSelectableList.SelectionMode.SINGLE));
    verify(component).setSelectionMode(MultipleSelectableList.SelectionMode.SINGLE);
  }
}
