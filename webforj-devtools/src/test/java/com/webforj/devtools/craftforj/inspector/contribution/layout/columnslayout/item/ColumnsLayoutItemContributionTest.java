package com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout.item;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.component.layout.columnslayout.ColumnsLayout;
import com.webforj.concern.HasStyle;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class ColumnsLayoutItemContributionTest {

  private final TestColumnsLayoutItemContribution contribution =
      new TestColumnsLayoutItemContribution();

  @Test
  void shouldAlwaysPositionItemFirst() {
    assertEquals(SourceChange.ItemPosition.FIRST, contribution.getItemPosition());
  }

  @Nested
  class ParseIntTests {

    @Test
    void shouldReturnNullForNullValue() {
      assertNull(ColumnsLayoutItemContribution.parseInt(null));
    }

    @Test
    void shouldReturnNullForEmptyValue() {
      assertNull(ColumnsLayoutItemContribution.parseInt(""));
    }

    @Test
    void shouldReturnNullForNonNumericValue() {
      assertNull(ColumnsLayoutItemContribution.parseInt("abc"));
    }

    @Test
    void shouldParseValidInteger() {
      assertEquals(5, ColumnsLayoutItemContribution.parseInt("5"));
    }
  }

  @Nested
  class ToIntTests {

    @Test
    void shouldConvertNumberValue() {
      assertEquals(3, ColumnsLayoutItemContribution.toInt(3.0));
    }

    @Test
    void shouldConvertStringValue() {
      assertEquals(7, ColumnsLayoutItemContribution.toInt("7"));
    }
  }

  @Nested
  class ParseAlignmentTests {

    @Test
    void shouldResolveAlignmentFromFullyQualifiedValue() {
      String value = ColumnsLayout.Alignment.class.getCanonicalName() + ".CENTER";

      assertEquals(ColumnsLayout.Alignment.CENTER,
          ColumnsLayoutItemContribution.parseAlignment(value));
    }
  }

  @Nested
  class ReadAlignmentTests {

    @Test
    void shouldReturnNullWhenStyleUnset() {
      var component = mock(ColumnsLayoutItemTestHelper.StylableComponent.class);
      when(component.getStyle("justify-self")).thenReturn(null);

      assertNull(ColumnsLayoutItemContribution.readAlignment(component, "justify-self"));
    }

    @Test
    void shouldReturnNullWhenStyleDoesNotMatchAnyAlignment() {
      var component = mock(ColumnsLayoutItemTestHelper.StylableComponent.class);
      when(component.getStyle("justify-self")).thenReturn("unknown-value");

      assertNull(ColumnsLayoutItemContribution.readAlignment(component, "justify-self"));
    }

    @Test
    void shouldMapMatchingStyleToFullyQualifiedConstant() {
      var component = mock(ColumnsLayoutItemTestHelper.StylableComponent.class);
      when(component.getStyle("justify-self")).thenReturn("center");

      assertEquals(ColumnsLayout.Alignment.class.getCanonicalName() + ".CENTER",
          ColumnsLayoutItemContribution.readAlignment(component, "justify-self"));
    }
  }

  private static class TestColumnsLayoutItemContribution extends ColumnsLayoutItemContribution {

    TestColumnsLayoutItemContribution() {
      super(HasStyle.class, "Test", "setTest");
    }

    @Override
    protected ValueExpression toSourceExpression(FeatureProperty property) {
      return scalarExpression(property);
    }
  }
}
