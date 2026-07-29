package com.webforj.devtools.craftforj.inspector.contribution;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.concern.HasStyle;
import com.webforj.devtools.craftforj.inspector.model.FeatureCategory;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.PropertyType;
import com.webforj.devtools.craftforj.inspector.model.SelectOption;
import java.util.List;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class KeyValueConcernContributionTest {

  @Nested
  class Get {

    @Test
    void shouldGet() {
      var contribution = new TestKeyValueContribution();
      var component = mock(StylableComponent.class);
      when(component.getStyle("test-key")).thenReturn("test-value");

      var result = contribution.get(component);

      assertTrue(result.isPresent());
      assertEquals("Test Key", result.get().getName());
      assertEquals("TestKeyValue", result.get().getFeatureType());
      assertEquals(PropertyType.TEXT, result.get().getEditorType());
      assertEquals("test-value", result.get().getValue());
    }

    @Test
    void shouldGetNull() {
      var contribution = new TestKeyValueContribution();
      var component = mock(StylableComponent.class);
      when(component.getStyle("test-key")).thenReturn(null);

      var result = contribution.get(component);

      assertTrue(result.isPresent());
      assertNull(result.get().getValue());
    }

    @Test
    void shouldGetOptionsWhenConfigured() {
      var contribution = new TestKeyValueContributionWithOptions();
      var component = mock(StylableComponent.class);
      when(component.getStyle("select-key")).thenReturn("option1");

      var result = contribution.get(component);

      assertTrue(result.isPresent());
      assertEquals(PropertyType.SELECT, result.get().getEditorType());
      assertEquals(2, ((java.util.List<?>) result.get().getEditorConfig().get("options")).size());
    }
  }

  @Nested
  class Set {

    @Test
    void shouldSet() {
      var contribution = new TestKeyValueContribution();
      var component = mock(StylableComponent.class);

      boolean result = contribution.set(component, "new-value");

      assertTrue(result);
      verify(component).setStyle("test-key", "new-value");
    }

    @Test
    void shouldNotSetNonMatchingComponent() {
      var contribution = new TestKeyValueContribution();
      var component = mock(Component.class);

      boolean result = contribution.set(component, "value");

      assertFalse(result);
    }

    @Test
    void shouldNotSetWhenNoSetterConfigured() {
      var contribution = new TestKeyValueContributionNoSetter();
      var component = mock(StylableComponent.class);

      boolean result = contribution.set(component, "value");

      assertFalse(result);
    }
  }

  @Nested
  class GetSourceValue {

    @Test
    void shouldBuildList() {
      var contribution = new TestKeyValueContribution();
      var property =
          FeatureProperty.builder("Test", "TestKeyValue").text().value("my-value").build();

      Object result = contribution.getSourceValue(property);

      assertTrue(result instanceof List);
      List<?> list = (List<?>) result;
      assertEquals(2, list.size());
      assertEquals("test-key", list.get(0));
      assertEquals("my-value", list.get(1));
    }

    @Test
    void shouldConvertIntegerToString() {
      var contribution = new TestKeyValueContribution();
      var property = FeatureProperty.builder("Test", "TestKeyValue").integer().value(42.0).build();

      Object result = contribution.getSourceValue(property);

      assertTrue(result instanceof List);
      List<?> list = (List<?>) result;
      assertEquals(2, list.size());
      assertEquals("test-key", list.get(0));
      assertEquals("42", list.get(1));
    }

    @Test
    void shouldGetNullValue() {
      var contribution = new TestKeyValueContribution();
      var property = FeatureProperty.builder("Test", "TestKeyValue").text().value(null).build();

      Object result = contribution.getSourceValue(property);

      assertTrue(result instanceof List);
      List<?> list = (List<?>) result;
      assertEquals(2, list.size());
      assertEquals("test-key", list.get(0));
      assertEquals("", list.get(1));
    }
  }

  @Nested
  class Metadata {

    @Test
    void shouldGetCorrectFeatureInterface() {
      var contribution = new TestKeyValueContribution();

      assertEquals(HasStyle.class, contribution.getFeatureInterface());
    }

    @Test
    void shouldGetCorrectFeatureType() {
      var contribution = new TestKeyValueContribution();

      assertEquals("TestKeyValue", contribution.getFeatureType());
    }
  }

  // Test implementations

  abstract static class StylableComponent extends Component implements HasStyle<StylableComponent> {
  }

  static class TestKeyValueContribution extends KeyValueConcernContribution<HasStyle<?>> {
    TestKeyValueContribution() {
      super(HasStyle.class, "test-key", "Test Key", FeatureCategory.LAYOUT);
      setBuilderConfig(FeatureProperty.Builder::text);
      setGetter(c -> c.getStyle("test-key"));
      setSetter((c, v) -> c.setStyle("test-key", (String) v));
    }
  }

  static class TestKeyValueContributionNoSetter extends KeyValueConcernContribution<HasStyle<?>> {
    TestKeyValueContributionNoSetter() {
      super(HasStyle.class, "test-key", "Test Key", FeatureCategory.LAYOUT);
      setGetter(c -> c.getStyle("test-key"));
    }
  }

  static class TestKeyValueContributionWithOptions
      extends KeyValueConcernContribution<HasStyle<?>> {

    private static final List<SelectOption> OPTIONS =
        List.of(new SelectOption("option1", "Option 1"), new SelectOption("option2", "Option 2"));

    TestKeyValueContributionWithOptions() {
      super(HasStyle.class, "select-key", "Select Key", FeatureCategory.LAYOUT);
      setBuilderConfig(b -> b.select(OPTIONS));
      setGetter(c -> c.getStyle("select-key"));
      setSetter((c, v) -> c.setStyle("select-key", (String) v));
    }
  }
}
