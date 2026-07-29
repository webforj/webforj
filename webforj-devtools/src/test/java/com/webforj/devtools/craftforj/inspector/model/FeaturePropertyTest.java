package com.webforj.devtools.craftforj.inspector.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class FeaturePropertyTest {

  enum TestEnum {
    VALUE_A, VALUE_B, VALUE_C
  }

  @Test
  void shouldBuildTextProperty() {
    FeatureProperty prop = FeatureProperty.builder("Text", "HasText").text().value("hello").build();

    assertEquals("Text", prop.getName());
    assertEquals("HasText", prop.getFeatureType());
    assertEquals(PropertyType.TEXT, prop.getEditorType());
    assertNull(prop.getEditorConfig());
    assertEquals(String.class, prop.getJavaType());
    assertEquals("hello", prop.getValue());
  }

  @Test
  void shouldBuildTextPropertyWithConfig() {
    Map<String, Object> config = Map.of("maxLength", 50, "pattern", "[A-Za-z]+");
    FeatureProperty prop =
        FeatureProperty.builder("Name", "HasName").text(config).value("test").build();

    assertEquals(PropertyType.TEXT, prop.getEditorType());
    assertEquals(config, prop.getEditorConfig());
    assertEquals(String.class, prop.getJavaType());
  }

  @Test
  void shouldBuildIntegerProperty() {
    FeatureProperty prop =
        FeatureProperty.builder("MaxRowCount", "HasMaxRowCount").integer().value(5).build();

    assertEquals("MaxRowCount", prop.getName());
    assertEquals("HasMaxRowCount", prop.getFeatureType());
    assertEquals(PropertyType.NUMBER, prop.getEditorType());
    assertEquals(Map.of("step", 1), prop.getEditorConfig());
    assertEquals(Integer.class, prop.getJavaType());
    assertEquals(5, prop.getValue());
  }

  @Test
  void shouldBuildIntegerPropertyWithBounds() {
    FeatureProperty prop =
        FeatureProperty.builder("Count", "HasCount").integer(0, 100).value(50).build();

    assertEquals(PropertyType.NUMBER, prop.getEditorType());
    Map<String, Object> config = prop.getEditorConfig();
    assertEquals(1, config.get("step"));
    assertEquals(0, config.get("min"));
    assertEquals(100, config.get("max"));
    assertEquals(Integer.class, prop.getJavaType());
  }

  @Test
  void shouldBuildDecimalProperty() {
    FeatureProperty prop =
        FeatureProperty.builder("Opacity", "HasOpacity").decimal().value(0.5).build();

    assertEquals("Opacity", prop.getName());
    assertEquals(PropertyType.NUMBER, prop.getEditorType());
    assertEquals(Map.of("step", "any"), prop.getEditorConfig());
    assertEquals(Double.class, prop.getJavaType());
    assertEquals(0.5, prop.getValue());
  }

  @Test
  void shouldBuildDecimalPropertyWithStep() {
    FeatureProperty prop =
        FeatureProperty.builder("Value", "HasValue").decimal(0.1).value(1.5).build();

    assertEquals(PropertyType.NUMBER, prop.getEditorType());
    assertEquals(Map.of("step", 0.1), prop.getEditorConfig());
    assertEquals(Double.class, prop.getJavaType());
  }

  @Test
  void shouldBuildDecimalPropertyWithStepAndBounds() {
    FeatureProperty prop = FeatureProperty.builder("Percent", "HasPercent").decimal(0.01, 0.0, 1.0)
        .value(0.75).build();

    assertEquals(PropertyType.NUMBER, prop.getEditorType());
    Map<String, Object> config = prop.getEditorConfig();
    assertEquals(0.01, config.get("step"));
    assertEquals(0.0, config.get("min"));
    assertEquals(1.0, config.get("max"));
    assertEquals(Double.class, prop.getJavaType());
  }

  @Test
  void shouldBuildBooleanProperty() {
    FeatureProperty prop =
        FeatureProperty.builder("Visible", "HasVisibility").bool().value(true).build();

    assertEquals("Visible", prop.getName());
    assertEquals(PropertyType.BOOLEAN, prop.getEditorType());
    assertNull(prop.getEditorConfig());
    assertEquals(Boolean.class, prop.getJavaType());
    assertEquals(true, prop.getValue());
  }

  @Test
  void shouldBuildSelectProperty() {
    List<SelectOption> options =
        List.of(new SelectOption("a", "Option A"), new SelectOption("b", "Option B"));

    FeatureProperty prop =
        FeatureProperty.builder("Choice", "HasChoice").select(options).value("a").build();

    assertEquals("Choice", prop.getName());
    assertEquals(PropertyType.SELECT, prop.getEditorType());
    assertEquals(String.class, prop.getJavaType());
    assertEquals("a", prop.getValue());

    // Options are in editorConfig
    Map<String, Object> config = prop.getEditorConfig();
    assertNotNull(config);
    assertEquals(options, config.get("options"));
  }

  @Test
  void shouldBuildEnumProperty() {
    FeatureProperty prop = FeatureProperty.builder("Status", "HasStatus").enumOf(TestEnum.class)
        .value(TestEnum.VALUE_B).build();

    assertEquals("Status", prop.getName());
    assertEquals(PropertyType.SELECT, prop.getEditorType());
    assertEquals(TestEnum.class, prop.getJavaType());
    assertEquals(TestEnum.VALUE_B, prop.getValue());

    // Options are in editorConfig
    Map<String, Object> config = prop.getEditorConfig();
    assertNotNull(config);
    @SuppressWarnings("unchecked")
    List<SelectOption> options = (List<SelectOption>) config.get("options");
    assertNotNull(options);
    assertEquals(3, options.size());
    assertEquals("VALUE_A", options.get(0).getValue());
    assertEquals("VALUE_A", options.get(0).getLabel());
    assertEquals("VALUE_B", options.get(1).getValue());
    assertEquals("VALUE_C", options.get(2).getValue());
  }

  @Test
  void shouldBuildListProperty() {
    List<String> items = List.of("class1", "class2");
    FeatureProperty prop =
        FeatureProperty.builder("ClassNames", "HasClassName").list().value(items).build();

    assertEquals("ClassNames", prop.getName());
    assertEquals(PropertyType.LIST, prop.getEditorType());
    assertEquals(Map.of("itemType", "string"), prop.getEditorConfig());
    assertEquals(List.class, prop.getJavaType());
    assertEquals(items, prop.getValue());
  }

  @Test
  void shouldBuildListOfProperty() {
    FeatureProperty prop = FeatureProperty.builder("Items", "HasItems").listOf(Integer.class)
        .value(List.of(1, 2, 3)).build();

    assertEquals(PropertyType.LIST, prop.getEditorType());
    assertEquals(Map.of("itemType", "integer"), prop.getEditorConfig());
    assertEquals(List.class, prop.getJavaType());
  }

  @Test
  void shouldBuildSizeProperty() {
    FeatureProperty prop =
        FeatureProperty.builder("Width", "HasWidth").size().value("100px").build();

    assertEquals("Width", prop.getName());
    assertEquals(PropertyType.SIZE, prop.getEditorType());
    assertNull(prop.getEditorConfig());
    assertEquals(String.class, prop.getJavaType());
    assertEquals("100px", prop.getValue());
  }

  @Test
  void shouldDefaultToTextType() {
    FeatureProperty prop = FeatureProperty.builder("Default", "HasDefault").value("test").build();

    assertEquals(PropertyType.TEXT, prop.getEditorType());
    assertEquals(String.class, prop.getJavaType());
  }

  @Test
  void shouldAllowNullValue() {
    FeatureProperty prop =
        FeatureProperty.builder("Nullable", "HasNullable").text().value(null).build();

    assertNull(prop.getValue());
  }

  @Test
  void shouldPreserveIntegerTypeForSourceGeneration() {
    FeatureProperty prop =
        FeatureProperty.builder("MaxRowCount", "HasMaxRowCount").integer().value(2).build();

    assertEquals(Integer.class, prop.getJavaType());
    assertEquals(1, prop.getEditorConfig().get("step"));
  }

  @Test
  void shouldPreserveDoubleTypeForSourceGeneration() {
    FeatureProperty prop =
        FeatureProperty.builder("Opacity", "HasOpacity").decimal().value(2.0).build();

    assertEquals(Double.class, prop.getJavaType());
    assertEquals("any", prop.getEditorConfig().get("step"));
  }
}
