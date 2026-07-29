package com.webforj.devtools.craftforj.inspector.source.generator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.ast.expr.StringLiteralExpr;
import org.junit.jupiter.api.Test;

class SourceChangeTest {

  @Test
  void shouldCreateValidSourceChangeWithBuilder() {
    SourceChange change =
        SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello"))
            .addImport("com.example.MyClass").build();

    assertEquals("setText", change.getMethodName());
    assertEquals("\"Hello\"", change.getArgument().toString());
    assertEquals(1, change.getImports().size());
    assertEquals("com.example.MyClass", change.getImports().get(0));
  }

  @Test
  void shouldSupportMultipleImports() {
    SourceChange change = SourceChange.builder()
        .methodCall("configure", new StringLiteralExpr("test")).addImport("com.example.A")
        .addImport("com.example.B").addImport("com.example.C").build();

    assertEquals(3, change.getImports().size());
    assertTrue(change.getImports().contains("com.example.A"));
    assertTrue(change.getImports().contains("com.example.B"));
    assertTrue(change.getImports().contains("com.example.C"));
  }

  @Test
  void shouldThrowWhenMethodNameIsMissing() {
    assertThrows(IllegalStateException.class, () -> {
      SourceChange.builder().methodCall(null, new StringLiteralExpr("Hello")).build();
    });
  }

  @Test
  void shouldThrowWhenMethodNameIsBlank() {
    assertThrows(IllegalStateException.class, () -> {
      SourceChange.builder().methodCall("  ", new StringLiteralExpr("Hello")).build();
    });
  }

  @Test
  void shouldDefaultAccessorToNull() {
    SourceChange change =
        SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();

    assertEquals(null, change.getAccessor());
  }

  @Test
  void shouldCopyWithAccessor() {
    SourceChange change =
        SourceChange.builder().methodCall("setPlaceholder", new StringLiteralExpr("Find"))
            .matchKey("key").addImport("com.example.A").build();

    SourceChange scoped = change.withAccessor("getSearch");

    assertEquals("getSearch", scoped.getAccessor());
    assertEquals("setPlaceholder", scoped.getMethodName());
    assertEquals("key", scoped.getMatchKey());
    assertEquals(change.getArguments(), scoped.getArguments());
    assertEquals(change.getImports(), scoped.getImports());
  }
}
