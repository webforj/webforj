package com.webforj.devtools.craftforj.docs.resolver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.devtools.craftforj.docs.model.DocsQuery;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;

class ChainedDocsResolverTest {

  @Test
  void shouldReturnFirstNonNullResult() {
    DocsResolver resolver1 = mock(DocsResolver.class);
    DocsResolver resolver2 = mock(DocsResolver.class);
    DocsResolver resolver3 = mock(DocsResolver.class);

    DocsQuery request = new DocsQuery("com.example.Test", "test-component");

    when(resolver1.resolve(request)).thenReturn(null);
    when(resolver2.resolve(request)).thenReturn("# Docs from resolver 2");
    when(resolver3.resolve(request)).thenReturn("# Docs from resolver 3");

    ChainedDocsResolver chained =
        new ChainedDocsResolver(Arrays.asList(resolver1, resolver2, resolver3));
    String result = chained.resolve(request);

    assertEquals("# Docs from resolver 2", result);
    verify(resolver1).resolve(request);
    verify(resolver2).resolve(request);
    verify(resolver3, never()).resolve(request); // Should not be called
  }

  @Test
  void shouldReturnNullWhenAllResolversReturnNull() {
    DocsResolver resolver1 = mock(DocsResolver.class);
    DocsResolver resolver2 = mock(DocsResolver.class);

    DocsQuery request = new DocsQuery("com.example.Unknown", "unknown");

    when(resolver1.resolve(request)).thenReturn(null);
    when(resolver2.resolve(request)).thenReturn(null);

    ChainedDocsResolver chained = new ChainedDocsResolver(Arrays.asList(resolver1, resolver2));
    String result = chained.resolve(request);

    assertNull(result);
    verify(resolver1).resolve(request);
    verify(resolver2).resolve(request);
  }

  @Test
  void shouldReturnNullForEmptyResolverList() {
    ChainedDocsResolver chained = new ChainedDocsResolver(Collections.emptyList());
    DocsQuery request = new DocsQuery("com.example.Test", "test");

    String result = chained.resolve(request);

    assertNull(result);
  }

  @Test
  void shouldWorkWithSingleResolver() {
    DocsResolver resolver = mock(DocsResolver.class);
    DocsQuery request = new DocsQuery("com.example.Test", "test");

    when(resolver.resolve(request)).thenReturn("# Single resolver");

    ChainedDocsResolver chained = new ChainedDocsResolver(List.of(resolver));
    String result = chained.resolve(request);

    assertEquals("# Single resolver", result);
  }
}
