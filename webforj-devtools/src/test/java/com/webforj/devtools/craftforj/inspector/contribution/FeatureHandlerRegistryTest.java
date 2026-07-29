package com.webforj.devtools.craftforj.inspector.contribution;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.component.ComponentSourceRegistry;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.concern.HasText;
import com.webforj.concern.HasVisibility;
import com.webforj.devtools.craftforj.inspector.model.FeatureGroup;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class FeatureHandlerRegistryTest {

  private FeatureHandlerRegistry registry;

  @BeforeEach
  void setUp() {
    registry = new FeatureHandlerRegistry();
  }

  @Test
  void shouldHaveHandlersLoaded() {

    assertTrue(registry.getHandler("HasText").isPresent());
  }

  @Test
  void shouldExtractFeaturesFromComponent() {
    TestHasTextComponent mockComponent = mock(TestHasTextComponent.class);
    when(mockComponent.getText()).thenReturn("Hello World");

    List<FeatureGroup> groups = registry.getFeatureGroups(mockComponent);

    assertNotNull(groups);
    assertFalse(groups.isEmpty());

    // Find the content group which should contain the text property
    FeatureGroup contentGroup =
        groups.stream().filter(g -> "content".equals(g.getId())).findFirst().orElse(null);

    assertNotNull(contentGroup);
    assertTrue(contentGroup.getProperties().stream().anyMatch(p -> "Text".equals(p.getName())));
  }

  @Test
  void shouldApplyChangeToComponent() {
    TestHasTextComponent mockComponent = mock(TestHasTextComponent.class);

    boolean success = registry.applyChange(mockComponent, "HasText", "New Text");

    assertTrue(success);
    verify(mockComponent).setText("New Text");
  }

  @Test
  void shouldReturnFalseForUnknownFeatureType() {
    Component mockComponent = mock(Component.class);

    boolean success = registry.applyChange(mockComponent, "NonExistentFeature", "value");

    assertFalse(success);
  }

  @Test
  void shouldReturnEmptyOptionalForUnknownHandler() {
    assertFalse(registry.getHandler("NonExistentHandler").isPresent());
  }

  @Test
  void shouldExposeAllRegisteredHandlers() {
    List<FeatureHandler> handlers = new ArrayList<>(registry.getHandlers());

    assertFalse(handlers.isEmpty());
    assertTrue(handlers.stream().anyMatch(h -> "HasText".equals(h.getFeatureType())));
  }

  @Test
  void shouldGroupPropertiesCorrectly() {
    TestHasTextComponent mockComponent = mock(TestHasTextComponent.class);
    when(mockComponent.getText()).thenReturn("Test");

    List<FeatureGroup> groups = registry.getFeatureGroups(mockComponent);

    boolean hasContentGroup = groups.stream().anyMatch(g -> "content".equals(g.getId()));
    assertTrue(hasContentGroup);
  }

  @Nested
  class RootVsNestedComponentTests {

    @Test
    void shouldShowBoundFeaturesForRootComponent() {
      // A Composite where component class matches the declaring class (root component)
      TestCompositeWithVisibleBound composite = mock(TestCompositeWithVisibleBound.class);
      TestVisibleComponent boundComponent = mock(TestVisibleComponent.class);
      when(boundComponent.isVisible()).thenReturn(true);

      SourcePoint sourcePoint =
          new SourcePoint(composite.getClass().getName(), "TestFile.java", 10);

      try (
          MockedStatic<ComponentSourceRegistry> sourceRegistry =
              mockStatic(ComponentSourceRegistry.class);
          MockedStatic<ComponentUtil> componentUtil = mockStatic(ComponentUtil.class)) {

        sourceRegistry.when(() -> ComponentSourceRegistry.getSourcePoint(composite))
            .thenReturn(sourcePoint);
        componentUtil.when(() -> ComponentUtil.getBoundComponent(composite))
            .thenReturn(boundComponent);

        List<FeatureGroup> groups = registry.getFeatureGroups(composite);

        // Should find HasVisibility feature from bound component
        boolean hasVisibility = groups.stream().flatMap(g -> g.getProperties().stream())
            .anyMatch(p -> "Visible".equals(p.getName()));
        assertTrue(hasVisibility, "Root composite should show bound component's Visible feature");
      }
    }

    @Test
    void shouldNotShowBoundFeaturesForNestedComponent() {
      // A Composite where component class differs from declaring class (nested component)
      TestCompositeWithVisibleBound composite = mock(TestCompositeWithVisibleBound.class);
      TestVisibleComponent boundComponent = mock(TestVisibleComponent.class);

      // Declaring class is different - this component is used inside another class
      SourcePoint sourcePoint = new SourcePoint("com.example.SomeOtherClass", "OtherFile.java", 10);

      try (
          MockedStatic<ComponentSourceRegistry> sourceRegistry =
              mockStatic(ComponentSourceRegistry.class);
          MockedStatic<ComponentUtil> componentUtil = mockStatic(ComponentUtil.class)) {

        sourceRegistry.when(() -> ComponentSourceRegistry.getSourcePoint(composite))
            .thenReturn(sourcePoint);
        componentUtil.when(() -> ComponentUtil.getBoundComponent(composite))
            .thenReturn(boundComponent);

        List<FeatureGroup> groups = registry.getFeatureGroups(composite);

        // Should NOT find HasVisibility feature - it only exists on bound component
        boolean hasVisibility = groups.stream().flatMap(g -> g.getProperties().stream())
            .anyMatch(p -> "Visible".equals(p.getName()));
        assertFalse(hasVisibility,
            "Nested composite should NOT show bound component's Visible feature");
      }
    }

    @Test
    void shouldShowDirectFeaturesForNestedComponent() {
      // A component that directly implements HasText, used as nested
      TestHasTextComponent component = mock(TestHasTextComponent.class);
      when(component.getText()).thenReturn("Hello");

      // Declaring class is different - this component is nested
      SourcePoint sourcePoint = new SourcePoint("com.example.SomeOtherClass", "OtherFile.java", 10);

      try (MockedStatic<ComponentSourceRegistry> sourceRegistry =
          mockStatic(ComponentSourceRegistry.class)) {

        sourceRegistry.when(() -> ComponentSourceRegistry.getSourcePoint(component))
            .thenReturn(sourcePoint);

        List<FeatureGroup> groups = registry.getFeatureGroups(component);

        // Should still find HasText feature - it's directly implemented
        boolean hasText = groups.stream().flatMap(g -> g.getProperties().stream())
            .anyMatch(p -> "Text".equals(p.getName()));
        assertTrue(hasText, "Nested component should still show its directly implemented features");
      }
    }

    @Test
    void shouldTreatAsNestedWhenNoSourcePoint() {
      // When source point is null, treat as nested (safer - shows fewer features)
      TestCompositeWithVisibleBound composite = mock(TestCompositeWithVisibleBound.class);
      TestVisibleComponent boundComponent = mock(TestVisibleComponent.class);

      try (
          MockedStatic<ComponentSourceRegistry> sourceRegistry =
              mockStatic(ComponentSourceRegistry.class);
          MockedStatic<ComponentUtil> componentUtil = mockStatic(ComponentUtil.class)) {

        sourceRegistry.when(() -> ComponentSourceRegistry.getSourcePoint(composite))
            .thenReturn(null);
        componentUtil.when(() -> ComponentUtil.getBoundComponent(composite))
            .thenReturn(boundComponent);

        List<FeatureGroup> groups = registry.getFeatureGroups(composite);

        // Should NOT find HasVisibility feature
        boolean hasVisibility = groups.stream().flatMap(g -> g.getProperties().stream())
            .anyMatch(p -> "Visible".equals(p.getName()));
        assertFalse(hasVisibility, "Component without source point should be treated as nested");
      }
    }
  }

  abstract static class TestHasTextComponent extends Component implements HasText<Component> {
  }

  abstract static class TestVisibleComponent extends Component implements HasVisibility<Component> {
  }

  abstract static class TestCompositeWithVisibleBound extends Composite<TestVisibleComponent> {
  }
}
