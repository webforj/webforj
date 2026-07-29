package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout.item;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.javaparser.ast.expr.DoubleLiteralExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.NameExpr;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import org.junit.jupiter.api.Test;

class FlexItemGrowContributionTest {

  private final FlexItemGrowContribution contribution = new FlexItemGrowContribution();

  @Test
  void shouldGet() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    when(component.getStyle("flex-grow")).thenReturn("2");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Grow", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(2.0, result.get().getValue());
    assertTrue(result.get().isParentScoped());
  }

  @Test
  void shouldGetNullForEmptyValue() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    when(component.getStyle("flex-grow")).thenReturn("");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertNull(result.get().getValue());
  }

  @Test
  void shouldGetNullForUnsetValue() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    when(component.getStyle("flex-grow")).thenReturn(null);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertNull(result.get().getValue());
  }

  @Test
  void shouldAlwaysReturnFalseForTwoArgSet() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);

    assertFalse(contribution.set(component, 3.0));
  }

  @Test
  void shouldSetThroughParent() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    var layout = mock(FlexLayout.class);

    assertTrue(contribution.set(component, layout, 3.0));
    verify(layout).setItemGrow(3.0, component);
  }

  @Test
  void shouldReturnFalseWhenParentIsNull() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);

    assertFalse(contribution.set(component, null, 3.0));
  }

  @Test
  void shouldResetThroughParentForEmptyValue() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    var layout = mock(FlexLayout.class);

    assertTrue(contribution.set(component, layout, null));
    verify(layout).setItemGrow(0, component);
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
  void shouldGetSourceMethodName() {
    assertEquals("setItemGrow", contribution.getSourceMethodName("flex-grow"));
  }

  @Test
  void shouldReturnNullSourceChangeForEmptyValue() {
    FeatureProperty property =
        FeatureProperty.builder("Grow", "FlexItemGrow").decimal().value(null).build();

    assertNull(contribution.buildItemSourceChange(property, "btn"));
  }

  @Test
  void shouldBuildSourceChangeWithItemLast() {
    FeatureProperty property =
        FeatureProperty.builder("Grow", "FlexItemGrow").decimal().value(3.0).build();

    SourceChange change = contribution.buildItemSourceChange(property, "btn");

    assertEquals("setItemGrow", change.getMethodName());
    assertEquals(2, change.getArguments().size());

    Expression first = change.getArguments().get(0);
    Expression second = change.getArguments().get(1);
    assertTrue(first instanceof DoubleLiteralExpr);
    assertEquals("3.0", first.toString());
    assertTrue(second instanceof NameExpr);
    assertEquals("btn", second.toString());

    assertEquals("btn", change.getItemRef());
    assertEquals(SourceChange.ItemPosition.LAST, change.getItemPosition());
  }
}
