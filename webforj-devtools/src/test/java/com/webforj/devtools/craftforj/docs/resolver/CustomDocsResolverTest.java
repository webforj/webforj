package com.webforj.devtools.craftforj.docs.resolver;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.devtools.craftforj.docs.model.DocsQuery;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class CustomDocsResolverTest {

  private CustomDocsResolver resolver;

  @BeforeEach
  void setUp() {
    resolver = new CustomDocsResolver();
  }

  @Test
  void shouldLoadCustomDocsByServerComponent() {
    DocsQuery request = new DocsQuery("com.example.TestComponent", null);
    String result = resolver.resolve(request);

    assertNotNull(result);
    assertTrue(result.contains("title: Test Component"));
    assertTrue(result.contains("This is a custom documentation file for testing."));
  }

  @Test
  void shouldLoadCustomDocsByClientComponent() {
    DocsQuery request = new DocsQuery(null, "test-client-component");
    String result = resolver.resolve(request);

    assertNotNull(result);
    assertTrue(result.contains("title: Test Client Component"));
    assertTrue(result.contains("Custom docs loaded by client component name."));
  }

  @Test
  void shouldPreferServerComponentOverClientComponent() {
    // Both exist, but server component should be tried first
    DocsQuery request = new DocsQuery("com.example.TestComponent", "test-client-component");
    String result = resolver.resolve(request);

    assertNotNull(result);
    // Should get server component docs
    assertTrue(result.contains("title: Test Component"));
  }

  @Test
  void shouldFallbackToClientComponentWhenServerNotFound() {
    DocsQuery request = new DocsQuery("com.example.NonExistent", "test-client-component");
    String result = resolver.resolve(request);

    assertNotNull(result);
    // Should get client component docs
    assertTrue(result.contains("title: Test Client Component"));
  }

  @Test
  void shouldReturnNullWhenNoCustomDocsExist() {
    DocsQuery request = new DocsQuery("com.example.NonExistent", "non-existent");
    String result = resolver.resolve(request);

    assertNull(result);
  }

  @Test
  void shouldReturnNullForNullServerComponent() {
    DocsQuery request = new DocsQuery(null, "non-existent");
    String result = resolver.resolve(request);

    assertNull(result);
  }

  @Test
  void shouldReturnNullForEmptyServerComponent() {
    DocsQuery request = new DocsQuery("", "non-existent");
    String result = resolver.resolve(request);

    assertNull(result);
  }

  @Test
  void shouldReturnNullForBothNull() {
    DocsQuery request = new DocsQuery(null, null);
    String result = resolver.resolve(request);

    assertNull(result);
  }
}
