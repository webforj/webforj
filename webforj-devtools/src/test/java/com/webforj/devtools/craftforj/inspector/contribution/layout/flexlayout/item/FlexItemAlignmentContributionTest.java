package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout.item;

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
import com.webforj.component.layout.flexlayout.FlexAlignment;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import com.webforj.devtools.craftforj.inspector.model.SelectOption;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import java.util.List;
import org.junit.jupiter.api.Test;

class FlexItemAlignmentContributionTest {

  private final FlexItemAlignmentContribution contribution = new FlexItemAlignmentContribution();

  @Test
  void shouldGet() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    when(component.getStyle("align-self")).thenReturn("center");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertEquals("Alignment", result.get().getName());
    assertEquals(PropertyType.SELECT, result.get().getEditorType());
    assertEquals(FlexAlignment.class.getCanonicalName() + ".CENTER", result.get().getValue());
    assertTrue(result.get().isParentScoped());
  }

  @Test
  void shouldGetNullForUnsetValue() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    when(component.getStyle("align-self")).thenReturn(null);

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    assertNull(result.get().getValue());
  }

  @Test
  void shouldGetOptions() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    when(component.getStyle("align-self")).thenReturn("stretch");

    var result = contribution.get(component);

    assertTrue(result.isPresent());
    @SuppressWarnings("unchecked")
    var options = (List<SelectOption>) result.get().getEditorConfig().get("options");
    assertEquals(FlexAlignment.values().length, options.size());
    assertTrue(options.stream()
        .anyMatch(o -> o.getValue().equals(FlexAlignment.class.getCanonicalName() + ".STRETCH")));
    assertTrue(options.stream()
        .anyMatch(o -> o.getValue().equals(FlexAlignment.class.getCanonicalName() + ".CENTER")));
  }

  @Test
  void shouldAlwaysReturnFalseForTwoArgSet() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);

    assertFalse(contribution.set(component, "flex-end"));
  }

  @Test
  void shouldSetThroughParent() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    var layout = mock(FlexLayout.class);

    assertTrue(
        contribution.set(component, layout, FlexAlignment.class.getCanonicalName() + ".END"));
    verify(layout).setItemAlignment(FlexAlignment.END, component);
  }

  @Test
  void shouldResetChildStyleDirectlyForEmptyValue() {
    var component = mock(FlexItemTestHelper.StylableComponent.class);
    var layout = mock(FlexLayout.class);

    assertTrue(contribution.set(component, layout, null));
    verify(component).setStyle("align-self", "");
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
    FeatureProperty property = FeatureProperty.builder("Alignment", "FlexItemAlignment")
        .value(FlexAlignment.class.getCanonicalName() + ".CENTER").build();

    SourceChange change = contribution.buildItemSourceChange(property, "btn");

    assertEquals("setItemAlignment", change.getMethodName());

    Expression first = change.getArguments().get(0);
    Expression second = change.getArguments().get(1);
    assertTrue(first instanceof FieldAccessExpr);
    assertEquals("FlexAlignment.CENTER", first.toString());
    assertTrue(second instanceof NameExpr);
    assertEquals("btn", second.toString());
    assertEquals(1, change.getImports().size());
    assertEquals(FlexAlignment.class.getCanonicalName(), change.getImports().get(0));
    assertEquals(SourceChange.ItemPosition.LAST, change.getItemPosition());
  }

  @Test
  void shouldThrowForInvalidEnumConstant() {
    FeatureProperty property = FeatureProperty.builder("Alignment", "FlexItemAlignment")
        .value(FlexAlignment.class.getCanonicalName() + ".NOT_A_VALUE").build();

    assertThrows(SourceModificationException.class,
        () -> contribution.buildItemSourceChange(property, "btn"));
  }
}
