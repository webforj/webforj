package com.webforj.devtools.craftforj.inspector.source.generator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import com.webforj.devtools.craftforj.inspector.source.generator.IconExpressionGenerator.IconExpression;
import com.webforj.devtools.craftforj.inspector.source.generator.IconExpressionGenerator.IconValue;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("IconExpressionGenerator")
class IconExpressionGeneratorTest {

  private static final String ICONS_PACKAGE = "com.webforj.component.icons.";

  @Nested
  class ParseValue {

    @Test
    @DisplayName("parses pool and name")
    void shouldParsePoolAndName() {
      IconValue value = IconExpressionGenerator.parseValue("tabler:home");

      assertEquals("tabler", value.pool());
      assertEquals("home", value.name());
    }

    @Test
    @DisplayName("rejects values without pool or name")
    void shouldRejectInvalidValues() {
      assertThrows(SourceModificationException.class,
          () -> IconExpressionGenerator.parseValue("home"));
      assertThrows(SourceModificationException.class,
          () -> IconExpressionGenerator.parseValue(":home"));
      assertThrows(SourceModificationException.class,
          () -> IconExpressionGenerator.parseValue("tabler:"));
      assertThrows(SourceModificationException.class,
          () -> IconExpressionGenerator.parseValue(null));
    }
  }

  @Nested
  class Generate {

    @Test
    @DisplayName("generates TablerIcon factory call")
    void shouldGenerateTablerFactory() {
      IconExpression result = IconExpressionGenerator.generate("tabler:home");

      assertEquals("TablerIcon.create(\"home\")", result.expression().toString());
      assertEquals(List.of(ICONS_PACKAGE + "TablerIcon"), result.imports());
    }

    @Test
    @DisplayName("generates FontAwesomeIcon factory call")
    void shouldGenerateFontAwesomeFactory() {
      IconExpression result = IconExpressionGenerator.generate("fa:star");

      assertEquals("FontAwesomeIcon.create(\"star\")", result.expression().toString());
      assertEquals(List.of(ICONS_PACKAGE + "FontAwesomeIcon"), result.imports());
    }

    @Test
    @DisplayName("generates FeatherIcon enum call with dashes mapped to underscores")
    void shouldGenerateFeatherEnumCall() {
      IconExpression result = IconExpressionGenerator.generate("feather:arrow-up");

      assertEquals("FeatherIcon.ARROW_UP.create()", result.expression().toString());
      assertEquals(List.of(ICONS_PACKAGE + "FeatherIcon"), result.imports());
    }

    @Test
    @DisplayName("generates DwcIcon enum call")
    void shouldGenerateDwcEnumCall() {
      IconExpression result = IconExpressionGenerator.generate("dwc:chevron-down");

      assertEquals("DwcIcon.CHEVRON_DOWN.create()", result.expression().toString());
      assertEquals(List.of(ICONS_PACKAGE + "DwcIcon"), result.imports());
    }

    @Test
    @DisplayName("falls back to generic Icon creation for unknown enum names")
    void shouldFallBackForUnknownEnumName() {
      IconExpression result = IconExpressionGenerator.generate("feather:does-not-exist");

      assertEquals("new Icon(\"does-not-exist\", \"feather\")", result.expression().toString());
      assertEquals(List.of(ICONS_PACKAGE + "Icon"), result.imports());
    }

    @Test
    @DisplayName("falls back to generic Icon creation for custom pools")
    void shouldFallBackForCustomPools() {
      IconExpression result = IconExpressionGenerator.generate("my-pool:logo");

      assertEquals("new Icon(\"logo\", \"my-pool\")", result.expression().toString());
      assertEquals(List.of(ICONS_PACKAGE + "Icon"), result.imports());
    }

    @Test
    @DisplayName("rejects values without a pool")
    void shouldRejectInvalidValue() {
      assertThrows(SourceModificationException.class,
          () -> IconExpressionGenerator.generate("home"));
    }
  }
}
