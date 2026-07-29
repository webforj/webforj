package com.webforj.devtools.craftforj.inspector.contribution.appearance.tree;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.tree.Tree;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class TreeLeafIconsVisibleContributionTest {

  private final TreeLeafIconsVisibleContribution contribution =
      new TreeLeafIconsVisibleContribution();

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldGet(boolean value) {
    Tree component = mock(Tree.class);
    when(component.isLeafIconsVisible()).thenReturn(value);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("LeafIconsVisible", result.get().getName());
    assertEquals(PropertyType.BOOLEAN, result.get().getEditorType());
    assertEquals(value, result.get().getValue());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldSet(boolean value) {
    Tree component = mock(Tree.class);

    assertTrue(contribution.set(component, value));
    verify(component).setLeafIconsVisible(value);
  }
}
