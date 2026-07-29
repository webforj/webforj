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

class BoundComponentStrategyTest {

  private BoundComponentStrategy strategy;

  @BeforeEach
  void setUp() {
    strategy = new BoundComponentStrategy();
  }

  @Test
  void shouldHandleCompositeWithGetBoundComponent() {
    String code = """
        class DrawerHeader extends Composite<FlexLayout> {
          public DrawerHeader() {
            getBoundComponent().setDirection(FlexDirection.COLUMN);
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(2, "FlexLayout"));

    assertTrue(canHandle);
  }

  @Test
  void shouldNotHandleNonCompositeClass() {
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
  void shouldNotHandleCompositeWithVariableAssignment() {
    String code = """
        class DrawerHeader extends Composite<FlexLayout> {
          private FlexLayout self = getBoundComponent();

          public DrawerHeader() {
            self.setDirection(FlexDirection.COLUMN);
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(2, "FlexLayout"));

    assertFalse(canHandle);
  }

  @Test
  void shouldAddSetterUsingGetBoundComponent() {
    String code = """
        class DrawerHeader extends Composite<FlexLayout> {
          public DrawerHeader() {
            getBoundComponent().setDirection(FlexDirection.COLUMN);
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change =
        SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();
    TargetContext target = new TargetContext(2, "FlexLayout");

    strategy.apply(cu, new ModificationContext(target, null, List.of(change)));

    String result = cu.toString();
    assertTrue(result.contains("getBoundComponent().setText"));
    assertTrue(result.contains("\"Hello\""));
  }

  @Test
  void shouldUpdateExistingGetBoundComponentCall() {
    String code = """
        class DrawerHeader extends Composite<FlexLayout> {
          public DrawerHeader() {
            getBoundComponent().setText("Old");
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change =
        SourceChange.builder().methodCall("setText", new StringLiteralExpr("New")).build();
    TargetContext target = new TargetContext(2, "FlexLayout");

    strategy.apply(cu, new ModificationContext(target, null, List.of(change)));

    String result = cu.toString();
    assertTrue(result.contains("\"New\""));
    assertFalse(result.contains("\"Old\""));
  }

  @Test
  void shouldAddSetterAfterExistingGetBoundComponentCalls() {
    String code = """
        class DrawerHeader extends Composite<FlexLayout> {
          public DrawerHeader() {
            getBoundComponent().setDirection(FlexDirection.COLUMN);
            getBoundComponent().setSpacing("0px");
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change =
        SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();
    TargetContext target = new TargetContext(2, "FlexLayout");

    strategy.apply(cu, new ModificationContext(target, null, List.of(change)));

    String result = cu.toString();
    assertTrue(result.contains("getBoundComponent().setText"));

    int setSpacingIndex = result.indexOf("setSpacing");
    int setTextIndex = result.indexOf("setText");
    assertTrue(setTextIndex > setSpacingIndex);
  }

  @Test
  void shouldHandleCompositeWithNoConstructor() {
    String code = """
        class DrawerHeader extends Composite<FlexLayout> {
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(1, "FlexLayout"));

    assertTrue(canHandle);
  }

  @Test
  void shouldHandleCompositeWithEmptyConstructor() {
    String code = """
        class DrawerHeader extends Composite<FlexLayout> {
          DrawerHeader() {
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandle = strategy.canHandle(cu, new TargetContext(2, "FlexLayout"));

    assertTrue(canHandle);
  }

  @Test
  void shouldAddSetterToEmptyConstructor() {
    String code = """
        class DrawerHeader extends Composite<FlexLayout> {
          DrawerHeader() {
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change = SourceChange.builder()
        .methodCall("setVisible", new com.github.javaparser.ast.expr.BooleanLiteralExpr(false))
        .build();
    TargetContext target = new TargetContext(2, "FlexLayout");

    strategy.apply(cu, new ModificationContext(target, null, List.of(change)));

    String result = cu.toString();
    assertTrue(result.contains("getBoundComponent().setVisible(false)"));
  }

  @Test
  void shouldCreateConstructorIfNoneExists() {
    String code = """
        class DrawerHeader extends Composite<FlexLayout> {
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);
    SourceChange change =
        SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();
    TargetContext target = new TargetContext(1, "FlexLayout");

    strategy.apply(cu, new ModificationContext(target, null, List.of(change)));

    String result = cu.toString();
    assertTrue(result.contains("DrawerHeader()"));
    assertTrue(result.contains("getBoundComponent().setText"));
  }

  @Test
  void shouldHandleCompositeClassForBoundComponentLine() {
    String code = """
        class TableView extends Composite<Div> {
          public TableView() {
            Table table = new Table();
            getBoundComponent().add(table);
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    boolean canHandleLine4 = strategy.canHandle(cu, new TargetContext(4, "Div"));
    assertTrue(canHandleLine4);
  }
}
