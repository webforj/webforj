package com.webforj.devtools.craftforj.inspector.contribution.state.tree;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.tree.Tree;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.api.Test;

class TreeSelectionModeContributionTest {

  private final TreeSelectionModeContribution contribution = new TreeSelectionModeContribution();

  @Test
  void shouldGet() {
    Tree component = mock(Tree.class);
    when(component.getSelectionMode()).thenReturn(Tree.SelectionMode.SINGLE);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("SelectionMode", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(Tree.SelectionMode.class.getCanonicalName() + ".SINGLE", result.get().getValue());
  }

  @Test
  void shouldSet() {
    Tree component = mock(Tree.class);

    assertTrue(
        contribution.set(component, Tree.SelectionMode.class.getCanonicalName() + ".MULTIPLE"));
    verify(component).setSelectionMode(Tree.SelectionMode.MULTIPLE);
  }
}
