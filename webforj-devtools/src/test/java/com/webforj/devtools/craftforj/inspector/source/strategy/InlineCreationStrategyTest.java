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

class InlineCreationStrategyTest {

  private InlineCreationStrategy strategy;

  @BeforeEach
  void setUp() {
    strategy = new InlineCreationStrategy();
  }

  @Test
  void shouldHandleInlineCreation() {
    String code = """
        class Test {
          void method() {
            add(new Button());
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(3, "Button"));

    assertTrue(canHandle);
  }

  @Test
  void shouldNotHandleAssignedCreation() {
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
  void shouldNotHandleFieldDeclaration() {
    String code = """
        class Test {
          private Button button = new Button();
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(2, "Button"));

    assertFalse(canHandle);
  }

  @Test
  void shouldExtractToVariableAndAddSetter() {
    String code = """
        class Test {
          void method() {
            add(new Button());
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change =
        SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();
    TargetContext target = new TargetContext(3, "Button");

    strategy.apply(cu, new ModificationContext(target, null, List.of(change)));

    String result = cu.toString();
    assertTrue(result.contains("Button button = new Button()"));
    assertTrue(result.contains("button.setText"));
    assertTrue(result.contains("add(button)"));
  }

  @Test
  void shouldGenerateUniqueVariableName() {
    String code = """
        class Test {
          void method() {
            Button button = new Button();
            add(new Button());
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change =
        SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();
    TargetContext target = new TargetContext(4, "Button");

    strategy.apply(cu, new ModificationContext(target, null, List.of(change)));

    String result = cu.toString();
    assertTrue(result.contains("button2"));
  }

  @Test
  void shouldApplyMultipleChangesAtOnce() {
    String code = """
        class Test {
          void method() {
            add(new Button());
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change1 =
        SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();
    SourceChange change2 = SourceChange.builder()
        .methodCall("setEnabled", new com.github.javaparser.ast.expr.BooleanLiteralExpr(true))
        .build();
    TargetContext target = new TargetContext(3, "Button");

    strategy.apply(cu, new ModificationContext(target, null, List.of(change1, change2)));

    String result = cu.toString();
    assertTrue(result.contains("Button button = new Button()"));
    assertTrue(result.contains("button.setText"));
    assertTrue(result.contains("button.setEnabled"));
    assertTrue(result.contains("add(button)"));
  }
}
