package com.webforj.devtools.craftforj.inspector.source.strategy;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import com.webforj.devtools.craftforj.inspector.source.model.ModificationContext;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class FieldDeclarationStrategyTest {

  private FieldDeclarationStrategy strategy;

  @BeforeEach
  void setUp() {
    strategy = new FieldDeclarationStrategy();
  }

  @Test
  void shouldHandleFieldDeclaration() {
    String code = """
        class Test {
          private Button button = new Button();
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(2, "Button"));

    assertTrue(canHandle);
  }

  @Test
  void shouldNotHandleLocalVariable() {
    String code = """
        class Test {
          void method() {
            Button button = new Button();
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(3, "Button"));

    assertFalse(canHandle);
  }

  @Test
  void shouldAddSetterToConstructor() {
    String code = """
        class Test {
          private Button button = new Button();
          Test() {
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change =
        SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();
    TargetContext target = new TargetContext(2, "Button");

    strategy.apply(cu, new ModificationContext(target, "button", List.of(change)));

    String result = cu.toString();
    assertTrue(result.contains("button.setText"));
  }
}
