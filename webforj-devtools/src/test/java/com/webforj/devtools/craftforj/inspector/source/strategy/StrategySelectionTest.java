package com.webforj.devtools.craftforj.inspector.source.strategy;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.expr.BooleanLiteralExpr;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import com.webforj.devtools.craftforj.inspector.source.model.ModificationContext;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests strategy selection order to ensure the correct strategy handles each code pattern.
 *
 * <p>
 * Strategy order (most specific first):
 * <ol>
 * <li>FieldDeclarationStrategy - class fields</li>
 * <li>InlineCreationStrategy - new X() passed as argument</li>
 * <li>FactoryMethodStrategy - static factory methods</li>
 * <li>LocalVariableStrategy - local variables in methods/constructors</li>
 * <li>BoundComponentStrategy - Composite getBoundComponent() pattern</li>
 * </ol>
 * </p>
 */
class StrategySelectionTest {

  private List<ModificationStrategy> strategies;

  @BeforeEach
  void setUp() {
    // Same order as SourceCodeModifier.createStrategies()
    strategies = List.of(new FieldDeclarationStrategy(), new InlineCreationStrategy(),
        new FactoryMethodStrategy(), new LocalVariableStrategy(), new BoundComponentStrategy());
  }

  private ModificationStrategy findStrategy(CompilationUnit cu, int line, String type) {
    TargetContext target = new TargetContext(line, type);
    for (ModificationStrategy strategy : strategies) {
      if (strategy.canHandle(cu, target)) {
        return strategy;
      }
    }
    return null;
  }

  @Nested
  @DisplayName("Field Declaration Pattern")
  class FieldDeclarationPattern {

    @Test
    @DisplayName("should select FieldDeclarationStrategy for class field")
    void shouldSelectFieldDeclarationStrategyForClassField() {
      String code = """
          class MyView {
            private Button button = new Button();
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      ModificationStrategy selected = findStrategy(cu, 2, "Button");

      assertNotNull(selected);
      assertEquals(FieldDeclarationStrategy.class, selected.getClass());
    }

    @Test
    @DisplayName("should select FieldDeclarationStrategy for field in Composite class")
    void shouldSelectFieldDeclarationStrategyForFieldInCompositeClass() {
      String code = """
          class MyView extends Composite<Div> {
            private Button button = new Button();
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      ModificationStrategy selected = findStrategy(cu, 2, "Button");

      assertNotNull(selected);
      assertEquals(FieldDeclarationStrategy.class, selected.getClass());
    }
  }

  @Nested
  @DisplayName("Inline Creation Pattern")
  class InlineCreationPattern {

    @Test
    @DisplayName("should select InlineCreationStrategy for new X() in method argument")
    void shouldSelectInlineCreationStrategyForNewInMethodArgument() {
      String code = """
          class MyView {
            void setup() {
              add(new Button());
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      ModificationStrategy selected = findStrategy(cu, 3, "Button");

      assertNotNull(selected);
      assertEquals(InlineCreationStrategy.class, selected.getClass());
    }

    @Test
    @DisplayName("should select InlineCreationStrategy for new X() in chained method")
    void shouldSelectInlineCreationStrategyForNewInChainedMethod() {
      String code = """
          class MyView {
            void setup() {
              add(new FlexLayout().add(new Button()));
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      // The Button is inline (no variable assignment)
      ModificationStrategy selected = findStrategy(cu, 3, "Button");

      assertNotNull(selected);
      assertEquals(InlineCreationStrategy.class, selected.getClass());
    }
  }

  @Nested
  @DisplayName("Factory Method Pattern")
  class FactoryMethodPattern {

    @Test
    @DisplayName("should select FactoryMethodStrategy for static create() method")
    void shouldSelectFactoryMethodStrategyForStaticCreateMethod() {
      String code = """
          class MyView {
            void setup() {
              FlexLayout.create().horizontal().build();
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      ModificationStrategy selected = findStrategy(cu, 3, "FlexLayout");

      assertNotNull(selected);
      assertEquals(FactoryMethodStrategy.class, selected.getClass());
    }

    @Test
    @DisplayName("should select FactoryMethodStrategy for enum constant factory method")
    void shouldSelectFactoryMethodStrategyForEnumConstantFactory() {
      String code = """
          class MyView {
            void setup() {
              btn.setPrefixComponent(FeatherIcon.BELL.create());
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      ModificationStrategy selected = findStrategy(cu, 3, "Icon");

      assertNotNull(selected);
      assertEquals(FactoryMethodStrategy.class, selected.getClass());
    }
  }

  @Nested
  @DisplayName("Local Variable Pattern")
  class LocalVariablePattern {

    static Stream<Arguments> localVariableCases() {
      return Stream.of(Arguments.of("variable in method", """
          class MyView {
            void setup() {
              Button button = new Button();
            }
          }
          """, 3, "Button"), Arguments.of("variable in constructor", """
          class MyView {
            MyView() {
              Button button = new Button();
            }
          }
          """, 3, "Button"), Arguments.of("variable in Composite constructor", """
          class TableView extends Composite<Div> {
            public TableView() {
              Table table = new Table();
              getBoundComponent().add(table);
            }
          }
          """, 3, "Table"));
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("localVariableCases")
    @DisplayName("should select LocalVariableStrategy for")
    void shouldSelectLocalVariableStrategy(String scenario, String code, int line, String type) {
      CompilationUnit cu = StaticJavaParser.parse(code);

      ModificationStrategy selected = findStrategy(cu, line, type);

      assertNotNull(selected);
      assertEquals(LocalVariableStrategy.class, selected.getClass());
    }

    @Test
    @DisplayName("should generate correct code for local variable in Composite class")
    void shouldGenerateCorrectCodeForLocalVariableInCompositeClass() {
      String code = """
          class TableView extends Composite<Div> {
            public TableView() {
              Table table = new Table();
              getBoundComponent().add(table);
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      SourceChange change =
          SourceChange.builder().methodCall("setStriped", new BooleanLiteralExpr(true)).build();
      TargetContext target = new TargetContext(3, "Table");

      ModificationStrategy selected = findStrategy(cu, 3, "Table");
      selected.apply(cu, new ModificationContext(target, "table", List.of(change)));

      String result = cu.toString();
      assertTrue(result.contains("table.setStriped(true)"), "Should use variable name 'table'");
      assertTrue(!result.contains("getBoundComponent().setStriped"),
          "Should NOT use getBoundComponent()");
    }
  }

  @Nested
  @DisplayName("Bound Component Pattern")
  class BoundComponentPattern {

    @Test
    @DisplayName("should select BoundComponentStrategy for Composite with direct getBoundComponent")
    void shouldSelectBoundComponentStrategyForCompositeWithDirectCalls() {
      String code = """
          class DrawerHeader extends Composite<FlexLayout> {
            public DrawerHeader() {
              getBoundComponent().setDirection(FlexDirection.COLUMN);
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      ModificationStrategy selected = findStrategy(cu, 3, "FlexLayout");

      assertNotNull(selected);
      assertEquals(BoundComponentStrategy.class, selected.getClass());
    }

    @Test
    @DisplayName("should select BoundComponentStrategy for empty Composite constructor")
    void shouldSelectBoundComponentStrategyForEmptyCompositeConstructor() {
      String code = """
          class DrawerHeader extends Composite<FlexLayout> {
            public DrawerHeader() {
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      ModificationStrategy selected = findStrategy(cu, 2, "FlexLayout");

      assertNotNull(selected);
      assertEquals(BoundComponentStrategy.class, selected.getClass());
    }

    @Test
    @DisplayName("should generate getBoundComponent() calls for Composite")
    void shouldGenerateGetBoundComponentCallsForComposite() {
      String code = """
          class DrawerHeader extends Composite<FlexLayout> {
            public DrawerHeader() {
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      SourceChange change =
          SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();
      TargetContext target = new TargetContext(2, "FlexLayout");

      ModificationStrategy selected = findStrategy(cu, 2, "FlexLayout");
      selected.apply(cu, new ModificationContext(target, null, List.of(change)));

      String result = cu.toString();
      assertTrue(result.contains("getBoundComponent().setText(\"Hello\")"));
    }

    @Test
    @DisplayName("should NOT use BoundComponentStrategy if variable assigned to getBoundComponent")
    void shouldNotSelectBoundComponentStrategyWhenVariableAssigned() {
      String code = """
          class DrawerHeader extends Composite<FlexLayout> {
            private FlexLayout self = getBoundComponent();
            public DrawerHeader() {
              self.setDirection(FlexDirection.COLUMN);
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      // Line 2 is the field - should use FieldDeclarationStrategy
      ModificationStrategy selected = findStrategy(cu, 2, "FlexLayout");

      assertNotNull(selected);
      assertEquals(FieldDeclarationStrategy.class, selected.getClass());
    }
  }

  @Nested
  @DisplayName("Tricky Scenarios")
  class ComplexScenarios {

    @Test
    @DisplayName("should handle multiple components in same Composite class")
    void shouldHandleMultipleComponentsInSameCompositeClass() {
      String code = """
          class MyView extends Composite<FlexLayout> {
            private Button headerButton = new Button();

            public MyView() {
              Table table = new Table();
              Slider slider = new Slider();
              getBoundComponent().add(table);
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      // Field at line 2 - FieldDeclarationStrategy
      assertEquals(FieldDeclarationStrategy.class, findStrategy(cu, 2, "Button").getClass());

      // Local variable at line 5 - LocalVariableStrategy
      assertEquals(LocalVariableStrategy.class, findStrategy(cu, 5, "Table").getClass());

      // Local variable at line 6 - LocalVariableStrategy
      assertEquals(LocalVariableStrategy.class, findStrategy(cu, 6, "Slider").getClass());
    }

    @Test
    @DisplayName("should handle nested Composite classes")
    void shouldHandleNestedCompositeClasses() {
      String code = """
          class OuterView extends Composite<Div> {
            public OuterView() {
              InnerView inner = new InnerView();
              getBoundComponent().add(inner);
            }

            class InnerView extends Composite<FlexLayout> {
              public InnerView() {
                Button btn = new Button();
                getBoundComponent().add(btn);
              }
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      // Outer: local variable at line 3
      assertEquals(LocalVariableStrategy.class, findStrategy(cu, 3, "InnerView").getClass());

      // Inner: local variable at line 9
      assertEquals(LocalVariableStrategy.class, findStrategy(cu, 9, "Button").getClass());
    }

    @Test
    @DisplayName("should apply changes correctly to field and local variable")
    void shouldApplyChangesToFieldAndLocalVariable() {
      String code = """
          class MyView extends Composite<FlexLayout> {
            private Button headerButton = new Button();

            public MyView() {
              Table table = new Table();
              getBoundComponent().add(table);
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      // Apply to field
      SourceChange fieldChange =
          SourceChange.builder().methodCall("setText", new StringLiteralExpr("Header")).build();
      TargetContext fieldTarget = new TargetContext(2, "Button");
      findStrategy(cu, 2, "Button").apply(cu,
          new ModificationContext(fieldTarget, "headerButton", List.of(fieldChange)));

      // Apply to local variable
      SourceChange varChange =
          SourceChange.builder().methodCall("setRowHeight", new IntegerLiteralExpr("50")).build();
      TargetContext varTarget = new TargetContext(5, "Table");
      findStrategy(cu, 5, "Table").apply(cu,
          new ModificationContext(varTarget, "table", List.of(varChange)));

      String result = cu.toString();
      assertTrue(result.contains("headerButton.setText(\"Header\")"));
      assertTrue(result.contains("table.setRowHeight(50)"));
    }

    @Test
    @DisplayName("should apply changes to bound component in empty Composite")
    void shouldApplyChangesToBoundComponent() {
      String code = """
          class MyView extends Composite<FlexLayout> {
            public MyView() {
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      SourceChange boundChange =
          SourceChange.builder().methodCall("setSpacing", new StringLiteralExpr("10px")).build();
      TargetContext target = new TargetContext(2, "FlexLayout");
      findStrategy(cu, 2, "FlexLayout").apply(cu,
          new ModificationContext(target, null, List.of(boundChange)));

      String result = cu.toString();
      assertTrue(result.contains("getBoundComponent().setSpacing(\"10px\")"));
    }
  }
}
