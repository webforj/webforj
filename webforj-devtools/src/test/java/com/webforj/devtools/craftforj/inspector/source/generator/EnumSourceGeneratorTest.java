package com.webforj.devtools.craftforj.inspector.source.generator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerator.GeneratorContext;
import org.junit.jupiter.api.Test;

class EnumSourceGeneratorTest {

  private final SourceGenerator generator = new EnumSourceGenerator();

  private GeneratorContext ctx(String methodName, Object value) {
    FeatureProperty prop =
        FeatureProperty.builder("Theme", "HasTheme").enumOf(TestTheme.class).value(value).build();
    return new GeneratorContext(methodName, prop);
  }

  @Test
  void shouldGenerateFieldAccessForEnumValue() {
    String fqn = TestTheme.class.getName() + ".PRIMARY";
    SourceChange change = generator.generate(ctx("setTheme", fqn));

    assertEquals("setTheme", change.getMethodName());
    assertTrue(change.getArgument() instanceof FieldAccessExpr);
    assertEquals("TestTheme.PRIMARY", change.getArgument().toString());
    assertEquals(1, change.getImports().size());
    assertTrue(change.getImports().get(0).contains("TestTheme"));
  }

  @Test
  void shouldThrowForEmptyValue() {
    assertThrows(SourceModificationException.class, () -> {
      generator.generate(ctx("setTheme", ""));
    });
  }

  @Test
  void shouldThrowForNullValue() {
    assertThrows(SourceModificationException.class, () -> {
      generator.generate(ctx("setTheme", null));
    });
  }

  @Test
  void shouldThrowForInvalidEnumFormat() {
    assertThrows(SourceModificationException.class, () -> {
      generator.generate(ctx("setTheme", "PRIMARY"));
    });
  }

  @Test
  void shouldThrowForNonExistentEnumClass() {
    assertThrows(SourceModificationException.class, () -> {
      generator.generate(ctx("setTheme", "com.nonexistent.Theme.PRIMARY"));
    });
  }

  @Test
  void shouldThrowForNonEnumClass() {
    assertThrows(SourceModificationException.class, () -> {
      generator.generate(ctx("setTheme", "java.lang.String.PRIMARY"));
    });
  }

  @Test
  void shouldThrowForInvalidEnumConstant() {
    String fqn = TestTheme.class.getName() + ".NONEXISTENT";
    assertThrows(SourceModificationException.class, () -> {
      generator.generate(ctx("setTheme", fqn));
    });
  }

  @Test
  void shouldUseDotsNotDollarsForNestedClassImports() {
    // TestTheme is a nested class, so Class.getName() returns with $
    // but imports must use . for nested classes
    String fqn = TestTheme.class.getCanonicalName() + ".PRIMARY";
    SourceChange change = generator.generate(ctx("setTheme", fqn));

    // Verify import uses dots, not dollars
    String importName = change.getImports().get(0);
    assertTrue(importName.contains(".TestTheme"), "Import should use dots for nested classes");
    assertTrue(!importName.contains("$"), "Import should not contain $ for nested classes");
    assertEquals(TestTheme.class.getCanonicalName(), importName);
  }

  enum TestTheme {
    PRIMARY, SECONDARY, DANGER
  }
}
