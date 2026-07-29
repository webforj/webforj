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

class LocalVariableStrategyTest {

  private LocalVariableStrategy strategy;

  @BeforeEach
  void setUp() {
    strategy = new LocalVariableStrategy();
  }

  @Test
  void shouldHandleLocalVariable() {
    String code = """
        class Test {
          void method() {
            Button button = new Button();
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(3, "Button"));

    assertTrue(canHandle);
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
  void shouldAddSetterAfterDeclaration() {
    String code = """
        class Test {
          void method() {
            Button button = new Button();
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change =
        SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();
    TargetContext target = new TargetContext(3, "Button");

    strategy.apply(cu, new ModificationContext(target, "button", List.of(change)));

    String result = cu.toString();
    assertTrue(result.contains("button.setText"));
  }

  @Test
  void shouldUpdateExistingSetterCall() {
    String code = """
        class Test {
          void method() {
            Button button = new Button();
            button.setText("Old");
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change =
        SourceChange.builder().methodCall("setText", new StringLiteralExpr("New")).build();
    TargetContext target = new TargetContext(3, "Button");

    strategy.apply(cu, new ModificationContext(target, "button", List.of(change)));

    String result = cu.toString();
    assertTrue(result.contains("\"New\""));
    assertFalse(result.contains("\"Old\""));
  }

  @Test
  void shouldHandleLocalVariableInCompositeClass() {
    String code = """
        class TableView extends Composite<Div> {
          public TableView() {
            Table table = new Table();
            getBoundComponent().add(table);
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(3, "Table"));
    assertTrue(canHandle);
  }

  @Test
  void shouldAddSetterToLocalVariableInCompositeClass() {
    String code = """
        class TableView extends Composite<Div> {
          public TableView() {
            Table table = new Table();
            getBoundComponent().add(table);
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change = SourceChange.builder()
        .methodCall("setStriped", new com.github.javaparser.ast.expr.BooleanLiteralExpr(true))
        .build();
    TargetContext target = new TargetContext(3, "Table");

    strategy.apply(cu, new ModificationContext(target, "table", List.of(change)));

    String result = cu.toString();
    assertTrue(result.contains("table.setStriped(true)"));
    assertFalse(result.contains("getBoundComponent().setStriped"));
  }
}
