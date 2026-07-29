package com.webforj.devtools.craftforj.inspector.source.generator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.ast.expr.BooleanLiteralExpr;
import com.github.javaparser.ast.expr.DoubleLiteralExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.NullLiteralExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.github.javaparser.ast.expr.TextBlockLiteralExpr;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.source.SourceModificationException;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceGenerator.GeneratorContext;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ScalarSourceGeneratorTest {

  private final SourceGenerator generator = new ScalarSourceGenerator();

  private GeneratorContext textCtx(String methodName, Object value) {
    FeatureProperty prop = FeatureProperty.builder("Prop", "Feature").text().value(value).build();
    return new GeneratorContext(methodName, prop);
  }

  private GeneratorContext boolCtx(String methodName, Object value) {
    FeatureProperty prop = FeatureProperty.builder("Prop", "Feature").bool().value(value).build();
    return new GeneratorContext(methodName, prop);
  }

  private GeneratorContext intCtx(String methodName, Object value) {
    FeatureProperty prop =
        FeatureProperty.builder("Prop", "Feature").integer().value(value).build();

    return new GeneratorContext(methodName, prop);
  }

  private GeneratorContext decimalCtx(String methodName, Object value) {
    FeatureProperty prop =
        FeatureProperty.builder("Prop", "Feature").decimal().value(value).build();

    return new GeneratorContext(methodName, prop);
  }

  @Test
  void shouldGenerateStringLiteralForStringValue() {
    SourceChange change = generator.generate(textCtx("setText", "Hello World"));

    assertEquals("setText", change.getMethodName());
    assertTrue(change.getArgument() instanceof StringLiteralExpr);
    assertEquals("\"Hello World\"", change.getArgument().toString());
    assertTrue(change.getImports().isEmpty());
  }

  @Test
  void shouldEscapeQuotesAndBackslashesInStringValue() {
    SourceChange change = generator.generate(textCtx("setText", "He said \"hi\" \\ done"));

    assertTrue(change.getArgument() instanceof StringLiteralExpr);
    assertEquals("\"He said \\\"hi\\\" \\\\ done\"", change.getArgument().toString());
  }

  @Test
  void shouldGenerateBooleanLiteralForBooleanValue() {
    SourceChange change = generator.generate(boolCtx("setEnabled", true));

    assertEquals("setEnabled", change.getMethodName());
    assertTrue(change.getArgument() instanceof BooleanLiteralExpr);
    assertEquals("true", change.getArgument().toString());
    assertTrue(change.getImports().isEmpty());
  }

  @Test
  void shouldGenerateIntegerLiteralForIntegerValue() {
    SourceChange change = generator.generate(intCtx("setMinLength", 5));

    assertEquals("setMinLength", change.getMethodName());
    assertTrue(change.getArgument() instanceof IntegerLiteralExpr);
    assertEquals("5", change.getArgument().toString());
    assertTrue(change.getImports().isEmpty());
  }

  @Test
  void shouldGenerateIntegerLiteralForDoubleValueWithIntegerJavaType() {
    // This is the key bug fix: JavaScript sends 2.0, but javaType=Integer generates "2"
    SourceChange change = generator.generate(intCtx("setMaxRowCount", 2.0));

    assertEquals("setMaxRowCount", change.getMethodName());
    assertTrue(change.getArgument() instanceof IntegerLiteralExpr);
    assertEquals("2", change.getArgument().toString());
  }

  @Test
  void shouldGenerateDoubleLiteralForDoubleValue() {
    SourceChange change = generator.generate(decimalCtx("setOpacity", 0.5));

    assertEquals("setOpacity", change.getMethodName());
    assertTrue(change.getArgument() instanceof DoubleLiteralExpr);
    assertTrue(change.getImports().isEmpty());
  }

  @Test
  void shouldReturnNullForNullValue() {
    SourceChange change = generator.generate(textCtx("setText", null));
    assertNull(change);
  }

  @Test
  void shouldReturnNullForEmptyStringValue() {
    SourceChange change = generator.generate(textCtx("setText", ""));
    assertNull(change);
  }

  @Test
  void shouldConvertNullToNullLiteralExpression() {
    Expression expr = ScalarSourceGenerator.toExpression(null);
    assertTrue(expr instanceof NullLiteralExpr);
    assertEquals("null", expr.toString());
  }

  @Test
  void shouldThrowForUnsupportedType() {
    assertThrows(SourceModificationException.class,
        () -> ScalarSourceGenerator.toExpression(new Object()));
  }

  @Test
  void shouldConvertLongToIntegerLiteral() {
    Expression expr = ScalarSourceGenerator.toExpression(100L);

    assertTrue(expr instanceof IntegerLiteralExpr);
    assertEquals("100L", expr.toString());
  }

  @Test
  void shouldConvertFloatToDoubleLiteralWithSuffix() {
    Expression expr = ScalarSourceGenerator.toExpression(1.5f);

    assertTrue(expr instanceof DoubleLiteralExpr);
    assertTrue(expr.toString().contains("f"));
  }

  @Test
  void shouldGenerateTextBlockForMultilineStringValue() {
    SourceChange change = generator.generate(textCtx("setText", "line1\nline2"));

    assertEquals("setText", change.getMethodName());
    assertTrue(change.getArgument() instanceof TextBlockLiteralExpr);
    assertEquals("        line1\n        line2\\\n        ",
        ((TextBlockLiteralExpr) change.getArgument()).getValue());
    assertTrue(change.getImports().isEmpty());
  }

  @Test
  void shouldKeepTrailingNewlineInTextBlockContent() {
    SourceChange change = generator.generate(textCtx("setText", "line1\nline2\n"));

    assertTrue(change.getArgument() instanceof TextBlockLiteralExpr);
    assertEquals("        line1\n        line2\n        ",
        ((TextBlockLiteralExpr) change.getArgument()).getValue());
  }

  @Test
  void shouldEscapeBackslashesAndTripleQuotesInTextBlock() {
    SourceChange change = generator.generate(textCtx("setText", "a \\ b\n\"\"\" inside\n"));

    assertEquals("        a \\\\ b\n        \\\"\\\"\\\" inside\n        ",
        ((TextBlockLiteralExpr) change.getArgument()).getValue());
  }

  @Test
  void shouldProtectTrailingWhitespaceInTextBlock() {
    SourceChange change = generator.generate(textCtx("setText", "word \nsp  \ntab\t\nend"));

    assertEquals("        word\\s\n        sp \\s\n        tab\\t\n        end\\\n        ",
        ((TextBlockLiteralExpr) change.getArgument()).getValue());
  }

  @Test
  void shouldFallBackToStringLiteralForCarriageReturns() {
    SourceChange change = generator.generate(textCtx("setText", "a\r\nb"));

    assertTrue(change.getArgument() instanceof StringLiteralExpr);
    assertEquals("\"a\\r\\nb\"", change.getArgument().toString());
  }

  @ParameterizedTest
  @ValueSource(strings = {"line1\nline2", "line1\nline2\n", "multi\n\n\nblank\n",
      "trail \nspaces  \n", "a \nb ", "tab\t\nend", "quote \" and \"\"\" run\n",
      "back\\slash\nmore\\", "\n", "  indented\n  lines", "ends\ninner\"", "\"\"\"\"\nx"})
  void shouldCompileTextBlockBackToOriginalValue(String value, @TempDir Path tempDir)
      throws Exception {
    Expression expr = ScalarSourceGenerator.toExpression(value, String.class);
    assertTrue(expr instanceof TextBlockLiteralExpr);

    String source = "public class Tb { public static final String S = " + expr + "; }";
    Path file = tempDir.resolve("Tb.java");
    Files.writeString(file, source);
    JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
    assertEquals(0, compiler.run(null, null, null, file.toString()), source);

    try (URLClassLoader loader = new URLClassLoader(new URL[] {tempDir.toUri().toURL()})) {
      Object compiled = loader.loadClass("Tb").getField("S").get(null);

      assertEquals(value, compiled);
    }
  }
}
