package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout.item;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import org.junit.jupiter.api.Test;

class FlexItemBasisContributionTest {

  private final FlexItemBasisContribution contribution = new FlexItemBasisContribution();

  @Test
  void shouldGet() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    when(component.getStyle("flex-basis")).thenReturn("100px");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Basis", result.get().getName());
    assertEquals(PropertyType.SIZE, result.get().getEditorType());
    assertEquals("100px", result.get().getValue());
    assertTrue(result.get().isParentScoped());
  }

  @Test
  void shouldGetNullForUnsetValue() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    when(component.getStyle("flex-basis")).thenReturn(null);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertNull(result.get().getValue());
  }

  @Test
  void shouldAlwaysReturnFalseForTwoArgSet() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);

    assertFalse(contribution.set(component, "50%"));
  }

  @Test
  void shouldSetThroughParent() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    var layout = mock(FlexLayout.class);

    assertTrue(contribution.set(component, layout, "50%"));
    verify(layout).setItemBasis("50%", component);
  }

  @Test
  void shouldResetThroughParentForEmptyValue() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    var layout = mock(FlexLayout.class);

    assertTrue(contribution.set(component, layout, null));
    verify(layout).setItemBasis(null, component);
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
        FeatureProperty.builder("Basis", "FlexItemBasis").size().value("50%").build();

    SourceChange change = contribution.buildItemSourceChange(property, "btn");

    assertEquals("setItemBasis", change.getMethodName());

    Expression first = change.getArguments().get(0);
    Expression second = change.getArguments().get(1);
    assertTrue(first instanceof StringLiteralExpr);
    assertEquals("50%", ((StringLiteralExpr) first).asString());
    assertTrue(second instanceof NameExpr);
    assertEquals("btn", second.toString());
    assertEquals(SourceChange.ItemPosition.LAST, change.getItemPosition());
  }
}
