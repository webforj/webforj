package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class BrowserConsoleTest {

  BrowserConsole console;

  @BeforeEach
  void setUp() {
    console = new BrowserConsole();
  }

  @Nested
  class FontStyleBuilderTests {

    @Test
    void shouldSetFontStyleToItalic() {
      console.style().italic();
      assertEquals("italic", console.getStyles().get("font-style"));
    }

    @Test
    void shouldSetFontStyleToNormal() {
      console.style().normal();
      assertEquals("normal", console.getStyles().get("font-style"));
    }
  }

  @Nested
  class FontWeightBuilderTests {

    @Test
    void shouldSetFontWeightToBold() {
      console.weight().bold();
      assertEquals("bold", console.getStyles().get("font-weight"));
    }

    @Test
    void shouldSetFontWeightToNormal() {
      console.weight().normal();
      assertEquals("normal", console.getStyles().get("font-weight"));
    }

    @Test
    void shouldSetFontWeightToBolder() {
      console.weight().bolder();
      assertEquals("bolder", console.getStyles().get("font-weight"));
    }

    @Test
    void shouldSetFontWeightToLighter() {
      console.weight().lighter();
      assertEquals("lighter", console.getStyles().get("font-weight"));
    }
  }

  @Nested
  class FontSizeBuilderTests {

    @Test
    void shouldSetFontSizeToSmall() {
      console.size().small();
      assertEquals("small", console.getStyles().get("font-size"));
    }

    @Test
    void shouldSetFontSizeToMedium() {
      console.size().medium();
      assertEquals("medium", console.getStyles().get("font-size"));
    }

    @Test
    void shouldSetFontSizeToLarge() {
      console.size().large();
      assertEquals("large", console.getStyles().get("font-size"));
    }

    @Test
    void shouldSetFontSizeToSmaller() {
      console.size().smaller();
      assertEquals("x-small", console.getStyles().get("font-size"));
    }

    @Test
    void shouldSetFontSizeToLarger() {
      console.size().larger();
      assertEquals("x-large", console.getStyles().get("font-size"));
    }

    @Test
    void shouldSetFontSizeToCustomValue() {
      console.size().from("16px");
      assertEquals("16px", console.getStyles().get("font-size"));
    }
  }

  @Nested
  class TextTransformBuilderTests {

    @Test
    void shouldSetTextTransformToNone() {
      console.transform().none();
      assertEquals("none", console.getStyles().get("text-transform"));
    }

    @Test
    void shouldSetTextTransformToCapitalize() {
      console.transform().capitalize();
      assertEquals("capitalize", console.getStyles().get("text-transform"));
    }

    @Test
    void shouldSetTextTransformToUppercase() {
      console.transform().uppercase();
      assertEquals("uppercase", console.getStyles().get("text-transform"));
    }

    @Test
    void shouldSetTextTransformToLowercase() {
      console.transform().lowercase();
      assertEquals("lowercase", console.getStyles().get("text-transform"));
    }
  }

  @Nested
  class ColorBuilderTests {

    @Test
    void shouldSetTextColorToRed() {
      console.color().red();
      assertEquals("#E86C5D", console.getStyles().get("color"));
    }

    @Test
    void shouldSetTextColorToCustomColor() {
      console.color().colored("#123456");
      assertEquals("#123456", console.getStyles().get("color"));
    }
  }

  @Nested
  class BackgroundColorBuilderTests {

    @Test
    void shouldSetBackgroundColorToYellow() {
      console.background().yellow();
      assertEquals("yellow", console.getStyles().get("background-color"));
    }

    @Test
    void shouldSetBackgroundColorToCustomColor() {
      console.background().colored("#654321");
      assertEquals("#654321", console.getStyles().get("background-color"));
    }
  }
}

