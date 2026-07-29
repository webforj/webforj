package com.webforj.devtools.craftforj.docs.index;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class HtmlElementsTest {

  @Test
  void shouldRecognizeCommonHtmlElements() {
    assertTrue(HtmlElements.isHtmlElement("div"));
    assertTrue(HtmlElements.isHtmlElement("span"));
    assertTrue(HtmlElements.isHtmlElement("button"));
    assertTrue(HtmlElements.isHtmlElement("input"));
    assertTrue(HtmlElements.isHtmlElement("a"));
  }

  @Test
  void shouldBeCaseInsensitive() {
    assertTrue(HtmlElements.isHtmlElement("DIV"));
    assertTrue(HtmlElements.isHtmlElement("Span"));
    assertTrue(HtmlElements.isHtmlElement("BUTTON"));
  }

  @Test
  void shouldRejectCustomElements() {
    assertFalse(HtmlElements.isHtmlElement("dwc-button"));
    assertFalse(HtmlElements.isHtmlElement("my-component"));
    assertFalse(HtmlElements.isHtmlElement("custom-element"));
  }

  @Test
  void shouldRejectNullAndEmpty() {
    assertFalse(HtmlElements.isHtmlElement(null));
    assertFalse(HtmlElements.isHtmlElement(""));
  }
}
