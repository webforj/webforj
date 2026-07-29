package com.webforj.devtools.craftforj.docs.resolver;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.devtools.craftforj.docs.fetcher.DwcFetcher;
import com.webforj.devtools.craftforj.docs.model.DocsQuery;
import com.webforj.devtools.craftforj.docs.model.DwcStylingData;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DwcOnlyResolverTest {

  private DwcFetcher dwcFetcher;
  private DwcOnlyResolver resolver;

  @BeforeEach
  void setUp() {
    dwcFetcher = mock(DwcFetcher.class);
    resolver = new DwcOnlyResolver(dwcFetcher);
  }

  @Test
  void shouldResolveDwcComponent() {
    DwcStylingData.Part part = new DwcStylingData.Part("control", "The wrapper");
    DwcStylingData styling = new DwcStylingData(List.of(part), null, null, null, null);

    when(dwcFetcher.fetch("dwc-button")).thenReturn(styling);

    DocsQuery request = new DocsQuery(null, "dwc-button");
    String result = resolver.resolve(request);

    assertNotNull(result);
    assertTrue(result.contains("dwc-button"));
    assertTrue(result.contains("Shadow Parts"));
    verify(dwcFetcher).fetch("dwc-button");
  }

  @Test
  void shouldReturnNullForNonDwcComponent() {
    DocsQuery request = new DocsQuery(null, "my-component");
    String result = resolver.resolve(request);

    assertNull(result);
    verify(dwcFetcher, never()).fetch(anyString());
  }

  @Test
  void shouldReturnNullForHtmlElement() {
    DocsQuery request = new DocsQuery(null, "div");
    String result = resolver.resolve(request);

    assertNull(result);
    verify(dwcFetcher, never()).fetch(anyString());
  }

  @Test
  void shouldReturnNullWhenNoStylingData() {
    when(dwcFetcher.fetch("dwc-unknown")).thenReturn(null);

    DocsQuery request = new DocsQuery(null, "dwc-unknown");
    String result = resolver.resolve(request);

    assertNull(result);
  }

  @Test
  void shouldReturnNullWhenStylingHasNoData() {
    DwcStylingData emptyData = new DwcStylingData(null, null, null, null, null);
    when(dwcFetcher.fetch("dwc-empty")).thenReturn(emptyData);

    DocsQuery request = new DocsQuery(null, "dwc-empty");
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
