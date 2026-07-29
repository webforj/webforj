package com.webforj.devtools.craftforj.inspector.source.parser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.type.Type;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class AstFinderTest {

  @Nested
  class MatchesType {

    @Test
    void shouldMatchDeclaredTypeWhenNoInitializer() {
      String code = """
          class Test {
            private Button button;
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      VariableDeclarator varDecl = cu.findAll(VariableDeclarator.class).get(0);

      assertTrue(AstFinder.matchesType(varDecl.getType(), varDecl.getInitializer().orElse(null),
          "Button"));
    }

    @Test
    void shouldMatchInitializerTypeEvenWhenDeclaredTypeIsASupertype() {
      String code = """
          class Test {
            void method() {
              FlexLayout self = new ColumnsLayout();
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      VariableDeclarator varDecl = cu.findAll(VariableDeclarator.class).get(0);

      assertTrue(AstFinder.matchesType(varDecl.getType(), varDecl.getInitializer().orElse(null),
          "ColumnsLayout"));
    }

    @Test
    void shouldRejectMismatchedType() {
      String code = """
          class Test {
            private Button button;
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      VariableDeclarator varDecl = cu.findAll(VariableDeclarator.class).get(0);

      assertFalse(AstFinder.matchesType(varDecl.getType(), varDecl.getInitializer().orElse(null),
          "TextField"));
    }

    @Test
    void shouldRejectNullDeclaredTypeWithNoInitializer() {
      assertFalse(AstFinder.matchesType((Type) null, null, "Button"));
    }

    @Test
    void shouldSkipCheckWhenExpectedTypeNameIsNull() {
      assertTrue(AstFinder.matchesType((Type) null, null, (String) null));
    }

    @Test
    void shouldSkipCheckWhenExpectedTypeNameIsEmpty() {
      assertTrue(AstFinder.matchesType((Type) null, null, ""));
    }

    @Test
    void shouldSkipCheckWhenExpectedTypeNamesCollectionIsNull() {
      assertTrue(AstFinder.matchesType((Type) null, null, (Set<String>) null));
    }

    @Test
    void shouldSkipCheckWhenExpectedTypeNamesCollectionIsEmpty() {
      assertTrue(AstFinder.matchesType((Type) null, null, Set.of()));
    }

    @Test
    void shouldMatchAnyOfMultipleExpectedTypeNames() {
      String code = """
          class Test {
            private Button button;
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      VariableDeclarator varDecl = cu.findAll(VariableDeclarator.class).get(0);

      assertTrue(AstFinder.matchesType(varDecl.getType(), varDecl.getInitializer().orElse(null),
          Set.of("TextField", "Button")));
    }
  }

  @Nested
  class FindFieldAt {

    @Test
    void shouldFindFieldDeclaration() {
      String code = """
          class Test {
            private Button button = new Button();
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      Optional<FieldDeclaration> field = AstFinder.findFieldAt(cu, new TargetContext(2, "Button"));

      assertTrue(field.isPresent());
      assertEquals("button", field.get().getVariable(0).getNameAsString());
    }

    @Test
    void shouldReturnEmptyWhenLineContainsLocalVariable() {
      String code = """
          class Test {
            void method() {
              Button button = new Button();
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      Optional<FieldDeclaration> field = AstFinder.findFieldAt(cu, new TargetContext(3, "Button"));

      assertFalse(field.isPresent());
    }

    @Test
    void shouldRejectFieldAtRightLineWithWrongAcceptableType() {
      String code = """
          class Test {
            private TextField confirmPassword = new TextField();
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      TargetContext target = new TargetContext(2, "");
      target.setAcceptableTypes(Set.of("ColumnsLayout"));

      Optional<FieldDeclaration> field = AstFinder.findFieldAt(cu, target);

      assertFalse(field.isPresent());
    }

    @Test
    void shouldAcceptFieldAtRightLineWithMatchingAcceptableType() {
      String code = """
          class Test {
            private TextField confirmPassword = new TextField();
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      TargetContext target = new TargetContext(2, "");
      target.setAcceptableTypes(Set.of("TextField"));

      Optional<FieldDeclaration> field = AstFinder.findFieldAt(cu, target);

      assertTrue(field.isPresent());
      assertEquals("confirmPassword", field.get().getVariable(0).getNameAsString());
    }
  }

  @Nested
  class FindNodeAt {

    @Test
    void shouldFindNodeInsideConstructor() {
      String code = """
          class Test {
            Test() {
              Button button = new Button();
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      Optional<Node> node = AstFinder.findNodeAt(cu, new TargetContext(3, "Button"));

      assertTrue(node.isPresent());
    }

    @Test
    void shouldReturnEmptyWhenLineHasNoInitializer() {
      String code = """
          class Test {
            private Button button;
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      Optional<Node> node = AstFinder.findNodeAt(cu, new TargetContext(2, "Button"));

      assertFalse(node.isPresent());
    }

    @Test
    void shouldFindStatementInsideLambdaBody() {
      String code = """
          class Test {
            Test() {
              Button showToast = new Button();
              showToast.onClick(e -> {
                Toast toast = new Toast("Hello");
                toast.open();
              });
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      Optional<Node> node = AstFinder.findNodeAt(cu, new TargetContext(5, "Toast"));

      assertTrue(node.isPresent());
      String nodeText = node.get().toString();
      assertTrue(nodeText.contains("Toast toast"),
          "Expected to find 'Toast toast' statement, but found: " + nodeText);
      assertFalse(nodeText.contains("showToast.onClick"),
          "Should not return the outer lambda statement");
    }

    @Test
    void shouldFindDeepestNestedStatement() {
      String code = """
          class Test {
            void method() {
              if (true) {
                for (int i = 0; i < 10; i++) {
                  Button button = new Button();
                }
              }
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      Optional<Node> node = AstFinder.findNodeAt(cu, new TargetContext(5, "Button"));

      assertTrue(node.isPresent());
      String nodeText = node.get().toString();
      assertTrue(nodeText.contains("Button button"),
          "Expected to find 'Button button' statement, but found: " + nodeText);
    }
  }

  @Nested
  class FindInlineCreationAt {

    @Test
    void shouldFindObjectCreationPassedAsArgument() {
      String code = """
          class Test {
            void method() {
              add(new Button());
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      Optional<ObjectCreationExpr> creation =
          AstFinder.findInlineCreationAt(cu, new TargetContext(3, "Button"));

      assertTrue(creation.isPresent());
      assertEquals("Button", creation.get().getType().getNameAsString());
    }

    @Test
    void shouldReturnEmptyWhenCreationIsAssignedToVariable() {
      String code = """
          class Test {
            void method() {
              Button button = new Button();
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      Optional<ObjectCreationExpr> creation =
          AstFinder.findInlineCreationAt(cu, new TargetContext(3, "Button"));

      assertFalse(creation.isPresent());
    }
  }

  @Nested
  class FindFactoryMethodAt {

    @Test
    void shouldFindStaticFactoryMethodCall() {
      String code = """
          class Test {
            void method() {
              add(Icon.create("test"));
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      Optional<MethodCallExpr> factory =
          AstFinder.findFactoryMethodAt(cu, new TargetContext(3, "Icon"));

      assertTrue(factory.isPresent());
      assertEquals("create", factory.get().getNameAsString());
    }

    @Test
    void shouldFindEnumConstantFactoryMethodCall() {
      String code = """
          class Test {
            void method() {
              add(FeatherIcon.BELL.create());
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      Optional<MethodCallExpr> factory =
          AstFinder.findFactoryMethodAt(cu, new TargetContext(3, "Icon"));

      assertTrue(factory.isPresent());
      assertEquals("create", factory.get().getNameAsString());
    }

    @Test
    void shouldReturnEmptyForInstanceMethodCall() {
      String code = """
          class Test {
            void method() {
              button.setText("Hello");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      Optional<MethodCallExpr> factory =
          AstFinder.findFactoryMethodAt(cu, new TargetContext(3, "Icon"));

      assertFalse(factory.isPresent());
    }
  }

  @Nested
  class ExtractVariableName {

    @Test
    void shouldExtractNameFromFieldDeclaration() {
      String code = """
          class Test {
            private Button myButton = new Button();
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      FieldDeclaration field = cu.findAll(FieldDeclaration.class).get(0);

      String name = AstFinder.extractVariableName(field);

      assertEquals("myButton", name);
    }
  }

  @Nested
  class ExtractVariableNameAt {

    @Test
    void shouldFindLocalVariableAtLine() {
      String code = """
          class Test {
            void method() {
              Button button = new Button();
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      String name = AstFinder.extractVariableNameAt(cu, new TargetContext(3, "Button"));

      assertEquals("button", name);
    }

    @Test
    void shouldFindFieldDeclarationAtLine() {
      String code = """
          class Test {
            private Button button = new Button();
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      String name = AstFinder.extractVariableNameAt(cu, new TargetContext(2, "Button"));

      assertEquals("button", name);
    }

    @Test
    void shouldReturnNullWhenLineHasNoVariable() {
      String code = """
          class Test {
            void method() {
              System.out.println("Hello");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      String name = AstFinder.extractVariableNameAt(cu, new TargetContext(3, "String"));

      assertNull(name);
    }

    @Test
    void shouldRejectFuzzyFieldFallbackWhenAcceptableTypesExcludeIt() {
      String code = """
          class RegisterForm {
            private TextField confirmPassword = new TextField();



            // ColumnsLayout columns = new ColumnsLayout();
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      TargetContext target = new TargetContext(6, "");
      target.setAcceptableTypes(Set.of("ColumnsLayout"));

      String name = AstFinder.extractVariableNameAt(cu, target);

      assertNull(name);
    }

    @Test
    void shouldDocumentFuzzyFieldFallbackWithoutTypeNames() {
      String code = """
          class RegisterForm {
            private TextField confirmPassword = new TextField();



            // ColumnsLayout columns = new ColumnsLayout();
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      String name = AstFinder.extractVariableNameAt(cu, new TargetContext(6, ""));

      assertEquals("confirmPassword", name);
    }
  }

  @Nested
  class IsCompositeClass {

    @Test
    void shouldReturnTrueForCompositeClass() {
      String code = """
          class DrawerHeader extends Composite<FlexLayout> {
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      assertTrue(AstFinder.isCompositeClass(cu));
    }

    @Test
    void shouldReturnFalseForNonCompositeClass() {
      String code = """
          class Test {
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      assertFalse(AstFinder.isCompositeClass(cu));
    }

    @Test
    void shouldReturnFalseForClassExtendingOther() {
      String code = """
          class Test extends Button {
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      assertFalse(AstFinder.isCompositeClass(cu));
    }
  }

  @Nested
  class BoundComponentTypeMatches {

    @Test
    void shouldMatchWhenExpectedTypesContainTheClassOwnName() {
      String code = """
          class DrawerHeader extends Composite<FlexLayout> {
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      assertTrue(AstFinder.boundComponentTypeMatches(cu, Set.of("DrawerHeader")));
    }

    @Test
    void shouldMatchWhenExpectedTypesContainTheCompositeTypeArgument() {
      String code = """
          class DrawerHeader extends Composite<FlexLayout> {
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      assertTrue(AstFinder.boundComponentTypeMatches(cu, Set.of("FlexLayout")));
    }

    @Test
    void shouldAcceptRawCompositeWithNoTypeArgument() {
      String code = """
          class DrawerHeader extends Composite {
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      assertTrue(AstFinder.boundComponentTypeMatches(cu, Set.of("AnythingElse")));
    }

    @Test
    void shouldRejectWhenNeitherClassNameNorTypeArgumentMatch() {
      String code = """
          class DrawerHeader extends Composite<FlexLayout> {
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      assertFalse(AstFinder.boundComponentTypeMatches(cu, Set.of("SomethingElse")));
    }
  }

  @Nested
  class UsesBoundComponentPattern {

    @Test
    void shouldReturnTrueWhenUsingGetBoundComponentDirectly() {
      String code = """
          class DrawerHeader extends Composite<FlexLayout> {
            public DrawerHeader() {
              getBoundComponent().setDirection(FlexDirection.COLUMN);
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      assertTrue(AstFinder.usesBoundComponentPattern(cu, new TargetContext(2, "FlexLayout")));
    }

    @Test
    void shouldReturnFalseWhenGetBoundComponentAssignedToVariable() {
      String code = """
          class DrawerHeader extends Composite<FlexLayout> {
            private FlexLayout self = getBoundComponent();

            public DrawerHeader() {
              self.setDirection(FlexDirection.COLUMN);
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      assertFalse(AstFinder.usesBoundComponentPattern(cu, new TargetContext(2, "FlexLayout")));
    }

    @Test
    void shouldReturnFalseForNonCompositeClass() {
      String code = """
          class Test {
            public Test() {
              Button button = new Button();
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      assertFalse(AstFinder.usesBoundComponentPattern(cu, new TargetContext(2, "Button")));
    }

    @Test
    void shouldReturnTrueForCompositeWithNoConstructor() {
      String code = """
          class DrawerHeader extends Composite<FlexLayout> {
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      assertTrue(AstFinder.usesBoundComponentPattern(cu, new TargetContext(1, "FlexLayout")));
    }

    @Test
    void shouldReturnTrueForCompositeWithEmptyConstructor() {
      String code = """
          class DrawerHeader extends Composite<FlexLayout> {
            public DrawerHeader() {
              // Empty constructor without getBoundComponent() calls yet
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      assertTrue(AstFinder.usesBoundComponentPattern(cu, new TargetContext(2, "FlexLayout")));
    }

    @Test
    void shouldRejectCompositeOfUnrelatedTypeWhenAcceptableTypesAreAHierarchy() {
      String code = """
          class DrawerHeader extends Composite<Div> {
            public DrawerHeader() {
              getBoundComponent().setText("test");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      TargetContext target = new TargetContext(2, "");
      target.setAcceptableTypes(Set.of("ColumnsLayout", "FlexLayout"));

      assertFalse(AstFinder.usesBoundComponentPattern(cu, target));
    }
  }
}
