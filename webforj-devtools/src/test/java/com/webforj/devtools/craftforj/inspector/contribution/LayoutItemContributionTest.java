package com.webforj.devtools.craftforj.inspector.contribution;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.concern.HasStyle;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import java.util.Optional;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class LayoutItemContributionTest {

  private final TestLayoutItemContribution contribution =
      new TestLayoutItemContribution(SourceChange.ItemPosition.LAST);

  @Nested
  class SupportsTests {

    @Test
    void shouldSupportChildImplementingConcern() {
      TestChildComponent child = mock(TestChildComponent.class);

      assertTrue(contribution.supports(child, true));
      assertTrue(contribution.supports(child, false));
    }

    @Test
    void shouldNotSupportChildNotImplementingConcern() {
      Component other = mock(Component.class);

      assertFalse(contribution.supports(other, true));
      assertFalse(contribution.supports(other, false));
    }
  }

  @Nested
  class SupportsParentTests {

    @Test
    void shouldSupportExactParentClass() {
      assertTrue(contribution.supportsParent(TestParent.class.getName()));
    }

    @Test
    void shouldSupportSubclassOfParent() {
      assertTrue(contribution.supportsParent(TestParentSubclass.class.getName()));
    }

    @Test
    void shouldNotSupportNullParentType() {
      assertFalse(contribution.supportsParent(null));
    }

    @Test
    void shouldNotSupportEmptyParentType() {
      assertFalse(contribution.supportsParent(""));
    }

    @Test
    void shouldNotSupportUnrelatedParentType() {
      assertFalse(contribution.supportsParent("com.example.OtherLayout"));
    }

    @Test
    void shouldNotSupportUnloadableParentType() {
      assertFalse(contribution.supportsParent("com.example.DoesNotExist"));
    }
  }

  @Nested
  class SetTests {

    @Test
    void shouldAlwaysReturnFalseForTwoArgSet() {
      TestChildComponent child = mock(TestChildComponent.class);

      assertFalse(contribution.set(child, "value"));
    }

    @Test
    void shouldReturnFalseWhenParentIsNull() {
      TestChildComponent child = mock(TestChildComponent.class);

      assertFalse(contribution.set(child, null, "value"));
    }

    @Test
    void shouldReturnFalseWhenParentIsWrongType() {
      TestChildComponent child = mock(TestChildComponent.class);
      Component wrongParent = mock(Component.class);

      assertFalse(contribution.set(child, wrongParent, "value"));
    }

    @Test
    void shouldReturnFalseWhenChildDoesNotImplementConcern() {
      Component child = mock(Component.class);
      TestParent parent = mock(TestParent.class);

      assertFalse(contribution.set(child, parent, "value"));
    }

    @Test
    void shouldApplyThroughDirectParent() {
      TestChildComponent child = mock(TestChildComponent.class);
      TestParent parent = mock(TestParent.class);

      assertTrue(contribution.set(child, parent, "newValue"));
      verify(child).setStyle("test-key", "newValue");
    }

    @Test
    void shouldApplyThroughCompositeBoundParent() {
      TestChildComponent child = mock(TestChildComponent.class);
      TestParent parent = mock(TestParent.class);
      TestCompositeParent composite = mock(TestCompositeParent.class);

      try (MockedStatic<ComponentUtil> componentUtil = mockStatic(ComponentUtil.class)) {
        componentUtil.when(() -> ComponentUtil.getBoundComponent(composite)).thenReturn(parent);

        assertTrue(contribution.set(child, composite, "newValue"));
        verify(child).setStyle("test-key", "newValue");
      }
    }

    @Test
    void shouldReturnFalseWhenCompositeBoundComponentIsWrongType() {
      TestChildComponent child = mock(TestChildComponent.class);
      Component unrelatedBound = mock(Component.class);
      TestCompositeParent composite = mock(TestCompositeParent.class);

      try (MockedStatic<ComponentUtil> componentUtil = mockStatic(ComponentUtil.class)) {
        componentUtil.when(() -> ComponentUtil.getBoundComponent(composite))
            .thenReturn(unrelatedBound);

        assertFalse(contribution.set(child, composite, "newValue"));
      }
    }

    @Test
    void shouldInvokeResetterForEmptyValue() {
      TestChildComponent child = mock(TestChildComponent.class);
      TestParent parent = mock(TestParent.class);

      assertTrue(contribution.set(child, parent, null));
      verify(child).setStyle("test-key", "");
    }

    @Test
    void shouldReturnFalseWhenSetterThrows() {
      TestChildComponent child = mock(TestChildComponent.class);
      TestParent parent = mock(TestParent.class);
      doThrow(new RuntimeException("boom")).when(child).setStyle("test-key", "newValue");

      assertFalse(contribution.set(child, parent, "newValue"));
    }
  }

  @Nested
  class GetTests {

    @Test
    void shouldMarkPropertyAsParentScoped() {
      TestChildComponent child = mock(TestChildComponent.class);
      when(child.getStyle("test-key")).thenReturn("value");

      Optional<FeatureProperty> result = contribution.get(child);

      assertTrue(result.isPresent());
      assertTrue(result.get().isParentScoped());
      assertEquals("value", result.get().getValue());
    }

    @Test
    void shouldReturnEmptyWhenChildDoesNotImplementConcern() {
      Component other = mock(Component.class);

      assertTrue(contribution.get(other).isEmpty());
    }

    @Test
    void shouldReturnEmptyWhenGetterThrows() {
      TestChildComponent child = mock(TestChildComponent.class);
      when(child.getStyle("test-key")).thenThrow(new RuntimeException("boom"));

      assertTrue(contribution.get(child).isEmpty());
    }
  }

  @Nested
  class MiscTests {

    @Test
    void shouldBeParentScoped() {
      assertTrue(contribution.isParentScoped());
    }

    @Test
    void shouldBelongToLayoutCategory() {
      assertEquals(FeatureCategory.LAYOUT, contribution.getCategory());
    }

    @Test
    void shouldExposeConcernAsFeatureInterface() {
      assertEquals(HasStyle.class, contribution.getFeatureInterface());
    }

    @Test
    void shouldUseConfiguredMethodNameForSourceGeneration() {
      assertEquals("setTest", contribution.getSourceMethodName("anything"));
    }

    @Test
    void shouldReportTwoArgumentItemCallCount() {
      assertEquals(2, contribution.getItemCallArgumentCount());
    }
  }

  @Nested
  class BuildItemSourceChangeTests {

    @Test
    void shouldReturnNullForNullValue() {
      FeatureProperty property =
          FeatureProperty.builder("Test", "Test").integer().value(null).build();

      assertNull(contribution.buildItemSourceChange(property, "item"));
    }

    @Test
    void shouldReturnNullForEmptyStringValue() {
      FeatureProperty property = FeatureProperty.builder("Test", "Test").text().value("").build();

      assertNull(contribution.buildItemSourceChange(property, "item"));
    }

    @Test
    void shouldPlaceItemLastWhenPositionIsLast() {
      FeatureProperty property = FeatureProperty.builder("Test", "Test").integer().value(5).build();

      SourceChange change = contribution.buildItemSourceChange(property, "myItem");

      assertEquals("setTest", change.getMethodName());
      assertEquals(2, change.getArguments().size());

      Expression first = change.getArguments().get(0);
      Expression second = change.getArguments().get(1);
      assertTrue(first instanceof IntegerLiteralExpr);
      assertEquals("5", first.toString());
      assertTrue(second instanceof NameExpr);
      assertEquals("myItem", second.toString());

      assertEquals("myItem", change.getItemRef());
      assertEquals(SourceChange.ItemPosition.LAST, change.getItemPosition());
    }

    @Test
    void shouldPlaceItemFirstWhenPositionIsFirst() {
      TestLayoutItemContribution firstPositioned =
          new TestLayoutItemContribution(SourceChange.ItemPosition.FIRST);
      FeatureProperty property = FeatureProperty.builder("Test", "Test").integer().value(5).build();

      SourceChange change = firstPositioned.buildItemSourceChange(property, "myItem");

      Expression first = change.getArguments().get(0);
      Expression second = change.getArguments().get(1);
      assertTrue(first instanceof NameExpr);
      assertEquals("myItem", first.toString());
      assertTrue(second instanceof IntegerLiteralExpr);
      assertEquals("5", second.toString());

      assertEquals("myItem", change.getItemRef());
      assertEquals(SourceChange.ItemPosition.FIRST, change.getItemPosition());
    }

    @Test
    void shouldThrowForInvalidEnumConstant() {
      EnumTestContribution enumContribution = new EnumTestContribution();
      FeatureProperty property = FeatureProperty.builder("Test", "Test").text()
          .value("any.prefix.TestEnum.NOT_A_CONSTANT").build();

      assertThrows(SourceModificationException.class,
          () -> enumContribution.buildItemSourceChange(property, "item"));
    }

    @Test
    void shouldBuildFieldAccessForValidEnumConstant() {
      EnumTestContribution enumContribution = new EnumTestContribution();
      FeatureProperty property = FeatureProperty.builder("Test", "Test").text()
          .value(TestEnum.class.getName() + ".TWO").build();

      SourceChange change = enumContribution.buildItemSourceChange(property, "item");

      Expression value = change.getArguments().get(1);
      assertTrue(value instanceof FieldAccessExpr);
      assertEquals("TestEnum.TWO", value.toString());
      assertEquals(1, change.getImports().size());
      assertEquals(TestEnum.class.getCanonicalName(), change.getImports().get(0));
    }

    @Test
    void shouldReturnNullWhenValueExpressionIsNull() {
      NullExpressionContribution nullExpressionContribution = new NullExpressionContribution();
      FeatureProperty property =
          FeatureProperty.builder("Test", "Test").text().value("non-empty").build();

      assertNull(nullExpressionContribution.buildItemSourceChange(property, "item"));
    }
  }

  private enum TestEnum {
    ONE, TWO
  }

  private abstract static class TestChildComponent extends Component
      implements HasStyle<TestChildComponent> {
  }

  private abstract static class TestParent extends Component {
  }

  private abstract static class TestParentSubclass extends TestParent {
  }

  private abstract static class TestCompositeParent extends Composite<TestParent> {
  }

  private static class TestLayoutItemContribution extends LayoutItemContribution<TestParent> {

    TestLayoutItemContribution(SourceChange.ItemPosition itemPosition) {
      super(TestParent.class, HasStyle.class, "Test", "setTest", itemPosition);

      setGetter(c -> ((HasStyle<?>) c).getStyle("test-key"));
      setSetter((parent, item, value) -> ((HasStyle<?>) item).setStyle("test-key",
          String.valueOf(value)));
      setResetter((parent, item) -> ((HasStyle<?>) item).setStyle("test-key", ""));
    }

    @Override
    protected ValueExpression toSourceExpression(FeatureProperty property) {
      return scalarExpression(property);
    }
  }

  private static class EnumTestContribution extends LayoutItemContribution<TestParent> {

    EnumTestContribution() {
      super(TestParent.class, HasStyle.class, "Test", "setTest", SourceChange.ItemPosition.FIRST);
    }

    @Override
    protected ValueExpression toSourceExpression(FeatureProperty property) {
      return enumExpression(property, TestEnum.class);
    }
  }

  private static class NullExpressionContribution extends LayoutItemContribution<TestParent> {

    NullExpressionContribution() {
      super(TestParent.class, HasStyle.class, "Test", "setTest", SourceChange.ItemPosition.LAST);
    }

    @Override
    protected ValueExpression toSourceExpression(FeatureProperty property) {
      return null;
    }
  }
}
