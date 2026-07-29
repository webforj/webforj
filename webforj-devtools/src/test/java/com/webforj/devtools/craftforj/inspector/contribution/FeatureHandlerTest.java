package com.webforj.devtools.craftforj.inspector.contribution;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.concern.HasText;
import com.webforj.concern.HasVisibility;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import java.util.Optional;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class FeatureHandlerTest {

  @Nested
  class GetTargetComponentTests {

    @Test
    void shouldReturnComponentWhenDirectlyImplementsInterface() {
      FeatureHandler handler = createHandler(HasText.class);
      TestHasTextComponent component = mock(TestHasTextComponent.class);

      Component result = handler.getTargetComponent(component, true);

      assertEquals(component, result);
    }

    @Test
    void shouldReturnComponentWhenDirectlyImplementsInterfaceEvenWithoutBoundFallback() {
      FeatureHandler handler = createHandler(HasText.class);
      TestHasTextComponent component = mock(TestHasTextComponent.class);

      Component result = handler.getTargetComponent(component, false);

      assertEquals(component, result);
    }

    @Test
    void shouldReturnBoundComponentWhenAllowBoundFallbackIsTrue() {
      FeatureHandler handler = createHandler(HasVisibility.class);
      TestCompositeWithVisibleBound composite = mock(TestCompositeWithVisibleBound.class);
      TestVisibleComponent boundComponent = mock(TestVisibleComponent.class);

      try (MockedStatic<ComponentUtil> componentUtil = mockStatic(ComponentUtil.class)) {
        componentUtil.when(() -> ComponentUtil.getBoundComponent(composite))
            .thenReturn(boundComponent);

        Component result = handler.getTargetComponent(composite, true);

        assertEquals(boundComponent, result);
      }
    }

    @Test
    void shouldNotReturnBoundComponentWhenAllowBoundFallbackIsFalse() {
      FeatureHandler handler = createHandler(HasVisibility.class);
      TestCompositeWithVisibleBound composite = mock(TestCompositeWithVisibleBound.class);
      TestVisibleComponent boundComponent = mock(TestVisibleComponent.class);

      try (MockedStatic<ComponentUtil> componentUtil = mockStatic(ComponentUtil.class)) {
        componentUtil.when(() -> ComponentUtil.getBoundComponent(composite))
            .thenReturn(boundComponent);

        Component result = handler.getTargetComponent(composite, false);

        assertNull(result, "Should not fall back to bound component when allowBoundFallback=false");
      }
    }

    @Test
    void shouldReturnNullWhenComponentDoesNotImplementInterface() {
      FeatureHandler handler = createHandler(HasVisibility.class);
      TestHasTextComponent component = mock(TestHasTextComponent.class);

      Component result = handler.getTargetComponent(component, true);

      assertNull(result);
    }
  }

  @Nested
  class SupportsTests {

    @Test
    void shouldSupportWhenDirectlyImplementsInterface() {
      FeatureHandler handler = createHandler(HasText.class);
      TestHasTextComponent component = mock(TestHasTextComponent.class);

      assertTrue(handler.supports(component, true));
      assertTrue(handler.supports(component, false));
    }

    @Test
    void shouldSupportBoundComponentOnlyWhenAllowBoundFallbackIsTrue() {
      FeatureHandler handler = createHandler(HasVisibility.class);
      TestCompositeWithVisibleBound composite = mock(TestCompositeWithVisibleBound.class);
      TestVisibleComponent boundComponent = mock(TestVisibleComponent.class);

      try (MockedStatic<ComponentUtil> componentUtil = mockStatic(ComponentUtil.class)) {
        componentUtil.when(() -> ComponentUtil.getBoundComponent(composite))
            .thenReturn(boundComponent);

        assertTrue(handler.supports(composite, true));
        assertFalse(handler.supports(composite, false));
      }
    }

    @Test
    void shouldDefaultToAllowingBoundFallback() {
      FeatureHandler handler = createHandler(HasVisibility.class);
      TestCompositeWithVisibleBound composite = mock(TestCompositeWithVisibleBound.class);
      TestVisibleComponent boundComponent = mock(TestVisibleComponent.class);

      try (MockedStatic<ComponentUtil> componentUtil = mockStatic(ComponentUtil.class)) {
        componentUtil.when(() -> ComponentUtil.getBoundComponent(composite))
            .thenReturn(boundComponent);

        // The no-arg supports() should default to allowing bound fallback
        assertTrue(handler.supports(composite));
      }
    }
  }

  @Nested
  class TranslationBundleTests {

    @Test
    void shouldDefaultToNoTranslationBundle() {
      FeatureHandler handler = createHandler(HasText.class);

      assertNull(handler.getTranslationBundle());
    }
  }

  private FeatureHandler createHandler(Class<?> featureInterface) {
    return new FeatureHandler() {
      @Override
      public Class<?> getFeatureInterface() {
        return featureInterface;
      }

      @Override
      public Optional<FeatureProperty> get(Component component) {
        return Optional.empty();
      }

      @Override
      public boolean set(Component component, Object value) {
        return false;
      }

      @Override
      public FeatureCategory getCategory() {
        return FeatureCategory.STATE;
      }
    };
  }

  abstract static class TestHasTextComponent extends Component implements HasText<Component> {
  }

  abstract static class TestVisibleComponent extends Component implements HasVisibility<Component> {
  }

  abstract static class TestCompositeWithVisibleBound extends Composite<TestVisibleComponent> {
  }
}
