package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout.item;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.github.javaparser.ast.expr.DoubleLiteralExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.NameExpr;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import org.junit.jupiter.api.Test;

class FlexItemShrinkContributionTest {

  private final FlexItemShrinkContribution contribution = new FlexItemShrinkContribution();

  @Test
  void shouldGet() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    when(component.getStyle("flex-shrink")).thenReturn("0");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Shrink", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(0.0, result.get().getValue());
    assertTrue(result.get().isParentScoped());
  }

  @Test
  void shouldGetNullForEmptyValue() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    when(component.getStyle("flex-shrink")).thenReturn("");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertNull(result.get().getValue());
  }

  @Test
  void shouldAlwaysReturnFalseForTwoArgSet() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);

    assertFalse(contribution.set(component, 1.0));
  }

  @Test
  void shouldSetThroughParent() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    var layout = mock(FlexLayout.class);

    assertTrue(contribution.set(component, layout, 1.0));
    verify(layout).setItemShrink(1.0, component);
  }

  @Test
  void shouldResetChildStyleDirectlyForEmptyValue() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    var layout = mock(FlexLayout.class);

    assertTrue(contribution.set(component, layout, null));
    verify(component).setStyle("flex-shrink", "");
    verifyNoInteractions(layout);
  }

  @Test
  void shouldSupportParentFlexLayout() {
    assertTrue(contribution.supportsParent(FlexLayout.class.getName()));
  }

  @Test
  void shouldNotSupportOtherParent() {
    assertFalse(contribution.supportsParent("com.example.OtherLayout"));
  }

  @Test
  void shouldBuildSourceChangeWithItemLast() {
    FeatureProperty property =
        FeatureProperty.builder("Shrink", "FlexItemShrink").decimal().value(0.5).build();

    SourceChange change = contribution.buildItemSourceChange(property, "btn");

    assertEquals("setItemShrink", change.getMethodName());

    Expression first = change.getArguments().get(0);
    Expression second = change.getArguments().get(1);
    assertTrue(first instanceof DoubleLiteralExpr);
    assertEquals("0.5", first.toString());
    assertTrue(second instanceof NameExpr);
    assertEquals("btn", second.toString());
    assertEquals(SourceChange.ItemPosition.LAST, change.getItemPosition());
  }
}
