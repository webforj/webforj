package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout.item;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import org.junit.jupiter.api.Test;

class ColumnsLayoutItemSpanContributionTest {

  private final ColumnsLayoutItemSpanContribution contribution =
      new ColumnsLayoutItemSpanContribution();

  @Test
  void shouldGet() {
    var component = mock(ColumnsLayoutItemTestHelper.AttributableComponent.class);
    when(component.getAttribute("data-span")).thenReturn("3");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Span", result.get().getName());
    assertEquals(PropertyType.NUMBER, result.get().getEditorType());
    assertEquals(3, result.get().getValue());
    assertTrue(result.get().isParentScoped());
  }

  @Test
  void shouldGetNullWhenAttributeUnset() {
    var component = mock(ColumnsLayoutItemTestHelper.AttributableComponent.class);
    when(component.getAttribute("data-span")).thenReturn(null);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertNull(result.get().getValue());
  }

  @Test
  void shouldAlwaysReturnFalseForTwoArgSet() {
    var component = mock(ColumnsLayoutItemTestHelper.AttributableComponent.class);

    assertFalse(contribution.set(component, 2.0));
  }

  @Test
  void shouldSetThroughParent() {
    var component = mock(ColumnsLayoutItemTestHelper.AttributableComponent.class);
    var layout = mock(ColumnsLayout.class);

    assertTrue(contribution.set(component, layout, 2.0));
    verify(layout).setSpan(component, 2);
  }

  @Test
  void shouldRemoveAttributeDirectlyForEmptyValue() {
    var component = mock(ColumnsLayoutItemTestHelper.AttributableComponent.class);
    var layout = mock(ColumnsLayout.class);

    assertTrue(contribution.set(component, layout, null));
    verify(component).removeAttribute("data-span");
    verifyNoInteractions(layout);
  }

  @Test
  void shouldSupportParentColumnsLayout() {
    assertTrue(contribution.supportsParent(ColumnsLayout.class.getName()));
  }

  @Test
  void shouldNotSupportOtherParent() {
    assertFalse(contribution.supportsParent("com.example.OtherLayout"));
  }

  @Test
  void shouldBuildSourceChangeWithItemFirst() {
    FeatureProperty property =
        FeatureProperty.builder("Span", "ColumnsLayoutItemSpan").integer().value(2).build();

    SourceChange change = contribution.buildItemSourceChange(property, "field1");

    assertEquals("setSpan", change.getMethodName());

    Expression first = change.getArguments().get(0);
    Expression second = change.getArguments().get(1);
    assertTrue(first instanceof NameExpr);
    assertEquals("field1", first.toString());
    assertTrue(second instanceof IntegerLiteralExpr);
    assertEquals("2", second.toString());
    assertEquals(SourceChange.ItemPosition.FIRST, change.getItemPosition());
  }
}
