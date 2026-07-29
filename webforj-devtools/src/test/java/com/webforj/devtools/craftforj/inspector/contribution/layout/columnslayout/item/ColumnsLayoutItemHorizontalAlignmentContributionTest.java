package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout.item;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.component.layout.columnslayout.ColumnsLayout.Alignment;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import com.webforj.devtools.craftforj.inspector.model.SelectOption;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import java.util.List;
import org.junit.jupiter.api.Test;

class ColumnsLayoutItemHorizontalAlignmentContributionTest {

  private final ColumnsLayoutItemHorizontalAlignmentContribution contribution =
      new ColumnsLayoutItemHorizontalAlignmentContribution();

  @Test
  void shouldGet() {
    var component = mock(ColumnsLayoutItemTestHelper.StylableComponent.class);
    when(component.getStyle("justify-self")).thenReturn("center");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("SelfHorizontalAlignment", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(Alignment.class.getCanonicalName() + ".CENTER", result.get().getValue());
    assertTrue(result.get().isParentScoped());
  }

  @Test
  void shouldGetNullWhenStyleUnset() {
    var component = mock(ColumnsLayoutItemTestHelper.StylableComponent.class);
    when(component.getStyle("justify-self")).thenReturn(null);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertNull(result.get().getValue());
  }

  @Test
  void shouldGetOptions() {
    var component = mock(ColumnsLayoutItemTestHelper.StylableComponent.class);
    when(component.getStyle("justify-self")).thenReturn("start");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    @SuppressWarnings("unchecked")
    var options = (List<SelectOption>) result.get().getEditorConfig().get("options");
    assertEquals(Alignment.values().length, options.size());
    assertTrue(options.stream()
        .anyMatch(o -> o.getValue().equals(Alignment.class.getCanonicalName() + ".START")));
  }

  @Test
  void shouldAlwaysReturnFalseForTwoArgSet() {
    var component = mock(ColumnsLayoutItemTestHelper.StylableComponent.class);

    assertFalse(contribution.set(component, Alignment.class.getCanonicalName() + ".CENTER"));
  }

  @Test
  void shouldSetThroughParent() {
    var component = mock(ColumnsLayoutItemTestHelper.StylableComponent.class);
    var layout = mock(ColumnsLayout.class);

    assertTrue(contribution.set(component, layout, Alignment.class.getCanonicalName() + ".END"));
    verify(layout).setHorizontalAlignment(component, Alignment.END);
  }

  @Test
  void shouldResetChildStyleDirectlyForEmptyValue() {
    var component = mock(ColumnsLayoutItemTestHelper.StylableComponent.class);
    var layout = mock(ColumnsLayout.class);

    assertTrue(contribution.set(component, layout, null));
    verify(component).setStyle("justify-self", "");
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
        FeatureProperty.builder("SelfHorizontalAlignment", "ColumnsLayoutItemHorizontalAlignment")
            .value(Alignment.class.getCanonicalName() + ".CENTER").build();

    SourceChange change = contribution.buildItemSourceChange(property, "field1");

    assertEquals("setHorizontalAlignment", change.getMethodName());

    Expression first = change.getArguments().get(0);
    Expression second = change.getArguments().get(1);
    assertTrue(first instanceof NameExpr);
    assertEquals("field1", first.toString());
    assertTrue(second instanceof FieldAccessExpr);
    assertEquals("Alignment.CENTER", second.toString());
    assertEquals(1, change.getImports().size());
    assertEquals(Alignment.class.getCanonicalName(), change.getImports().get(0));
    assertEquals(SourceChange.ItemPosition.FIRST, change.getItemPosition());
  }

  @Test
  void shouldThrowForInvalidEnumConstant() {
    FeatureProperty property =
        FeatureProperty.builder("SelfHorizontalAlignment", "ColumnsLayoutItemHorizontalAlignment")
            .value(Alignment.class.getCanonicalName() + ".NOT_A_VALUE").build();

    assertThrows(SourceModificationException.class,
        () -> contribution.buildItemSourceChange(property, "field1"));
  }
}
