package com.webforj.utilities;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.typesafe.config.Config;
import com.webforj.Environment;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockedStatic;

class HtmlTextTest {

  @Nested
  class IsWrappedWithHtmlTag {

    @ParameterizedTest
    @ValueSource(strings = {"<html>hi</html>", "  <html>hi</html>", "<html><b>hi</b></html>"})
    void shouldBeTrueWhenWrapped(String input) {
      assertEquals(true, HtmlText.isWrappedWithHtmlTag(input));
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"hi", "<b>hi</b>", "text <html>"})
    void shouldBeFalseWhenNotWrapped(String input) {
      assertEquals(false, HtmlText.isWrappedWithHtmlTag(input));
    }
  }

  @Nested
  class ForHtmlSink {

    @Test
    void shouldEncodeReservedCharacters() {
      assertEquals("&lt;b&gt;hi&lt;/b&gt; world", HtmlText.forHtmlSink("<b>hi</b> world"));
      assertEquals("Tom &amp; Jerry", HtmlText.forHtmlSink("Tom & Jerry"));
      assertEquals("5 &lt; 3 and 9 &gt; 2", HtmlText.forHtmlSink("5 < 3 and 9 > 2"));
    }

    @Test
    void shouldNeutralizeScriptPayloadInPlainText() {
      assertEquals("&lt;script&gt;alert(1)&lt;/script&gt;",
          HtmlText.forHtmlSink("<script>alert(1)</script>"));
    }

    @Test
    void shouldPassHtmlWrappedThroughRaw() {
      assertEquals("<html><b>hi</b></html>", HtmlText.forHtmlSink("<html><b>hi</b></html>"));
      assertEquals("<html><b>hi</b><script>alert(1)</script></html>",
          HtmlText.forHtmlSink("<html><b>hi</b><script>alert(1)</script></html>"));
      assertEquals("<html><img src=\"x\" onerror=\"alert(1)\"></html>",
          HtmlText.forHtmlSink("<html><img src=\"x\" onerror=\"alert(1)\"></html>"));
    }

    @Test
    void shouldStripMarkerAndEscapeInsideWhenLegacyDisabled() {
      withLegacyHtmlInText(false, () -> assertEquals("&lt;b&gt;hi&lt;/b&gt;",
          HtmlText.forHtmlSink("<html><b>hi</b></html>")));
    }

    @Test
    void shouldLeavePlainTextUntouched() {
      assertEquals("Click Me", HtmlText.forHtmlSink("Click Me"));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void shouldReturnEmptyForNullOrEmpty(String input) {
      assertEquals("", HtmlText.forHtmlSink(input));
    }
  }

  @Nested
  class ToText {

    @Test
    void shouldReturnVisibleTextOfMarkup() {
      assertEquals("hello", HtmlText.toText("<b>hello</b>"));
      assertEquals("Nothing", HtmlText.toText("<html><strong>Nothing</strong></html>"));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void shouldReturnEmptyForNullOrEmpty(String input) {
      assertEquals("", HtmlText.toText(input));
    }

    @ParameterizedTest
    @ValueSource(strings = {"Click Me", "Tom & Jerry", "5 < 3", "<b>hi</b> world",
        "<script>alert(1)</script>"})
    void shouldRoundTripThroughForHtmlSink(String value) {
      assertEquals(value, HtmlText.toText(HtmlText.forHtmlSink(value)));
    }
  }

  private static void withLegacyHtmlInText(boolean enabled, Runnable assertion) {
    try (MockedStatic<Environment> mocked = mockStatic(Environment.class)) {
      Environment environment = mock(Environment.class);
      Config config = mock(Config.class);
      mocked.when(Environment::getCurrent).thenReturn(environment);
      when(environment.getConfig()).thenReturn(config);
      when(config.hasPath(HtmlText.LEGACY_HTML_IN_TEXT)).thenReturn(true);
      when(config.getIsNull(HtmlText.LEGACY_HTML_IN_TEXT)).thenReturn(false);
      when(config.getBoolean(HtmlText.LEGACY_HTML_IN_TEXT)).thenReturn(enabled);

      assertion.run();
    }
  }
}
