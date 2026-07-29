package com.webforj.devtools.craftforj.docs.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.docs.model.DocsQuery;
import com.webforj.devtools.craftforj.docs.resolver.DocsResolver;
import org.junit.jupiter.api.Test;

class GetDocsActionTest {

  @Test
  void shouldReturnCorrectActionName() {
    GetDocsAction action = new GetDocsAction();
    assertEquals("docs.get", action.getAction());
  }

  @Test
  void shouldReturnMarkdownWhenResolverFindsDoc() {
    DocsResolver resolver = mock(DocsResolver.class);
    when(resolver.resolve(any(DocsQuery.class))).thenReturn("# Button\nA clickable button.");

    GetDocsAction action = new GetDocsAction(resolver);

    JsonObject params = new JsonObject();
    params.addProperty("serverComponent", "com.webforj.component.button.Button");
    params.addProperty("clientComponent", "dwc-button");

    GetDocsAction.Response response = action.handle(params);

    assertTrue(response.hasContent());
    assertEquals("# Button\nA clickable button.", response.getMarkdown());
  }

  @Test
  void shouldReturnEmptyResponseWhenResolverReturnsNull() {
    DocsResolver resolver = mock(DocsResolver.class);
    when(resolver.resolve(any(DocsQuery.class))).thenReturn(null);

    GetDocsAction action = new GetDocsAction(resolver);

    JsonObject params = new JsonObject();
    params.addProperty("serverComponent", "com.unknown.Component");

    GetDocsAction.Response response = action.handle(params);

    assertFalse(response.hasContent());
    assertNull(response.getMarkdown());
  }

  @Test
  void shouldPropagateExceptionWhenResolverThrows() {
    DocsResolver resolver = mock(DocsResolver.class);
    when(resolver.resolve(any(DocsQuery.class)))
        .thenThrow(new RuntimeException("Test error message"));

    GetDocsAction action = new GetDocsAction(resolver);

    JsonObject params = new JsonObject();
    params.addProperty("serverComponent", "com.example.Test");

    RuntimeException ex = assertThrows(RuntimeException.class, () -> action.handle(params));
    assertTrue(ex.getMessage().contains("Test error message"));
  }

  @Test
  void shouldHandleNullParams() {
    DocsResolver resolver = mock(DocsResolver.class);
    when(resolver.resolve(any(DocsQuery.class))).thenReturn(null);

    GetDocsAction action = new GetDocsAction(resolver);

    GetDocsAction.Response response = action.handle(null);

    assertFalse(response.hasContent());
  }

  @Test
  void shouldHandleEmptyParams() {
    DocsResolver resolver = mock(DocsResolver.class);
    when(resolver.resolve(any(DocsQuery.class))).thenReturn(null);

    GetDocsAction action = new GetDocsAction(resolver);

    GetDocsAction.Response response = action.handle(new JsonObject());

    assertFalse(response.hasContent());
  }
}
