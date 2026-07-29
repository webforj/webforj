package com.webforj.devtools.craftforj.docs.resolver;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.devtools.craftforj.docs.fetcher.DwcFetcher;
import com.webforj.devtools.craftforj.docs.index.DocsIndex;
import com.webforj.devtools.craftforj.docs.model.DocsEntry;
import com.webforj.devtools.craftforj.docs.model.DocsQuery;
import com.webforj.devtools.craftforj.docs.model.DwcStylingData;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class IndexDocsResolverTest {

  private DocsIndex docsIndex;
  private DwcFetcher dwcFetcher;
  private IndexDocsResolver resolver;

  @BeforeEach
  void setUp() {
    docsIndex = mock(DocsIndex.class);
    dwcFetcher = mock(DwcFetcher.class);
    resolver = new IndexDocsResolver(docsIndex, dwcFetcher);
  }

  @Test
  void shouldResolveByServerComponent() {
    DocsEntry entry = new DocsEntry();
    entry.setTitle("Button");
    entry.setSince("23.02");
    entry.setClientComponent("dwc-button");
    entry.setContent("A clickable button.");

    when(docsIndex.findByServerComponent("com.webforj.component.button.Button")).thenReturn(entry);

    DocsQuery request = new DocsQuery("com.webforj.component.button.Button", "dwc-button");
    String result = resolver.resolve(request);

    assertNotNull(result);
    assertTrue(result.contains("title: Button"));
    assertTrue(result.contains("since: 23.02"));
    assertTrue(result.contains("A clickable button."));
  }

  @Test
  void shouldResolveByClientComponentIfServerNotFound() {
    DocsEntry entry = new DocsEntry();
    entry.setTitle("Button");
    entry.setClientComponent("dwc-button");
    entry.setContent("A clickable button.");

    when(docsIndex.findByServerComponent("com.unknown.Button")).thenReturn(null);
    when(docsIndex.findByClientComponent("dwc-button")).thenReturn(entry);

    DocsQuery request = new DocsQuery("com.unknown.Button", "dwc-button");
    String result = resolver.resolve(request);

    assertNotNull(result);
    assertTrue(result.contains("title: Button"));
  }

  @Test
  void shouldFetchDwcStylingForComponent() {
    DocsEntry entry = new DocsEntry();
    entry.setTitle("Button");
    entry.setClientComponent("dwc-button");
    entry.setContent("A clickable button.");

    DwcStylingData.Part part = new DwcStylingData.Part("control", "The wrapper");
    DwcStylingData styling = new DwcStylingData(List.of(part), null, null, null, null);

    when(docsIndex.findByServerComponent("com.webforj.component.button.Button")).thenReturn(entry);
    when(dwcFetcher.fetch("dwc-button")).thenReturn(styling);

    DocsQuery request = new DocsQuery("com.webforj.component.button.Button", "dwc-button");
    String result = resolver.resolve(request);

    assertNotNull(result);
    assertTrue(result.contains("Shadow Parts"));
    assertTrue(result.contains("control"));
    verify(dwcFetcher).fetch("dwc-button");
  }

  @Test
  void shouldReturnNullWhenNotFound() {
    when(docsIndex.findByServerComponent("com.unknown.Component")).thenReturn(null);
    when(docsIndex.findByClientComponent("unknown-tag")).thenReturn(null);

    DocsQuery request = new DocsQuery("com.unknown.Component", "unknown-tag");
    String result = resolver.resolve(request);

    assertNull(result);
  }
}
