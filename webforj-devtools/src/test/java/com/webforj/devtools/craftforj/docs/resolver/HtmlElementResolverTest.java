package com.webforj.devtools.craftforj.docs.resolver;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.devtools.craftforj.docs.model.DocsQuery;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HtmlElementResolverTest {

  private HtmlElementResolver resolver;

  @BeforeEach
  void setUp() {
    resolver = new HtmlElementResolver();
  }

  @Test
  void shouldResolveHtmlElement() {
    DocsQuery request = new DocsQuery(null, "div");
    String result = resolver.resolve(request);

    assertNotNull(result);
    assertTrue(result.contains("title: <div>"));
    assertTrue(result.contains("developer.mozilla.org"));
  }

  @Test
  void shouldReturnNullForNonHtmlElement() {
    DocsQuery request = new DocsQuery(null, "dwc-button");
    String result = resolver.resolve(request);

    assertNull(result);
  }

  @Test
  void shouldReturnNullForCustomElement() {
    DocsQuery request = new DocsQuery(null, "my-component");
    String result = resolver.resolve(request);

    assertNull(result);
  }

  @Test
  void shouldReturnNullForNullClientComponent() {
    DocsQuery request = new DocsQuery("com.example.Test", null);
    String result = resolver.resolve(request);

    assertNull(result);
  }

  @Test
  void shouldReturnNullForEmptyClientComponent() {
    DocsQuery request = new DocsQuery("com.example.Test", "");
    String result = resolver.resolve(request);

    assertNull(result);
  }
}
