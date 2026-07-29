package com.webforj.devtools.craftforj.inspector.source.parser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.expr.BooleanLiteralExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import java.util.List;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class AstModifierTest {

  @Nested
  class CreateSetterStatement {

    @Test
    void shouldCreateValidStatement() {
      SourceChange change =
          SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();

      Statement stmt = AstModifier.createSetterStatement("button", change);

      assertTrue(stmt.toString().contains("button.setText(\"Hello\")"));
    }
  }

  @Nested
  class UpdateExistingSetterCall {

    @Test
    void shouldUpdateLastMatchingCall() {
      String code = """
          class Test {
            void method() {
              button.setText("Old");
              button.setText("Older");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      SourceChange change =
          SourceChange.builder().methodCall("setText", new StringLiteralExpr("New")).build();

      boolean updated = AstModifier.updateExistingSetterCall(cu, "button", change);

      assertTrue(updated);
      String result = cu.toString();
      assertTrue(result.contains("button.setText(\"New\")"));
    }

    @Test
    void shouldReturnFalseWhenNoMatchingCallFound() {
      String code = """
          class Test {
            void method() {
              button.setEnabled(true);
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      SourceChange change =
          SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();

      boolean updated = AstModifier.updateExistingSetterCall(cu, "button", change);

      assertFalse(updated);
    }

    @Test
    void shouldUpdateThisQualifiedCall() {
      String code = """
          class Test {
            Test(String text) {
              this.button.setText(text);
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      SourceChange change =
          SourceChange.builder().methodCall("setText", new StringLiteralExpr("New")).build();

      boolean updated = AstModifier.updateExistingSetterCall(cu, "button", change);

      assertTrue(updated);
      assertTrue(cu.toString().contains("this.button.setText(\"New\")"));
      assertEquals("text", change.getReplacedComputedExpression());
    }

    @Test
    void shouldSearchOnlyWithinTheGivenRoot() {
      String code = """
          class Test {
            Test() {
            }
            void api() {
              button.setText("Dead");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      SourceChange change =
          SourceChange.builder().methodCall("setText", new StringLiteralExpr("New")).build();
      var ctorBlock = cu.findFirst(ConstructorDeclaration.class).orElseThrow().getBody();

      boolean updated = AstModifier.updateExistingSetterCall(ctorBlock, "button", change);

      assertFalse(updated);
      assertTrue(cu.toString().contains("button.setText(\"Dead\")"));
    }

    @Test
    void shouldUpdateOnlyCallWithMatchingKey() {
      String code = """
          class Test {
            void method() {
              self.setStyle("margin", "1em");
              self.setStyle("padding", "2em");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      SourceChange change = SourceChange.builder()
          .methodCall("setStyle",
              List.of(new StringLiteralExpr("margin"), new StringLiteralExpr("3em")))
          .matchKey("margin").build();

      boolean updated = AstModifier.updateExistingSetterCall(cu, "self", change);

      assertTrue(updated);
      String result = cu.toString();
      assertTrue(result.contains("setStyle(\"margin\", \"3em\")"));
      assertTrue(result.contains("setStyle(\"padding\", \"2em\")"));
    }

    @Test
    void shouldNotUpdateCallWithDifferentKey() {
      String code = """
          class Test {
            void method() {
              self.setStyle("margin", "1em");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      SourceChange change = SourceChange.builder()
          .methodCall("setStyle",
              List.of(new StringLiteralExpr("padding"), new StringLiteralExpr("2em")))
          .matchKey("padding").build();

      boolean updated = AstModifier.updateExistingSetterCall(cu, "self", change);

      assertFalse(updated);
      String result = cu.toString();
      assertTrue(result.contains("setStyle(\"margin\", \"1em\")"));
    }
  }

  @Nested
  class IsMethodCallOnVariable {

    @Test
    void shouldReturnTrueForDirectCall() {
      String code = """
          class Test {
            void method() {
              button.setText("Hello");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      MethodCallExpr methodCall = cu.findAll(MethodCallExpr.class).get(0);

      boolean result = AstModifier.isMethodCallOnVariable(methodCall, "button");

      assertTrue(result);
    }

    @Test
    void shouldReturnTrueForChainedCall() {
      String code = """
          class Test {
            void method() {
              button.setEnabled(true).setText("Hello");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      MethodCallExpr setTextCall = cu.findAll(MethodCallExpr.class).stream()
          .filter(m -> m.getNameAsString().equals("setText")).findFirst().get();

      boolean result = AstModifier.isMethodCallOnVariable(setTextCall, "button");

      assertTrue(result);
    }

    @Test
    void shouldReturnFalseForDifferentVariable() {
      String code = """
          class Test {
            void method() {
              other.setText("Hello");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      MethodCallExpr methodCall = cu.findAll(MethodCallExpr.class).get(0);

      boolean result = AstModifier.isMethodCallOnVariable(methodCall, "button");

      assertFalse(result);
    }
  }

  @Nested
  class FindInsertionPointForVariable {

    @Test
    void shouldFindIndexAfterLastConsecutiveCall() {
      String code = """
          class Test {
            void method() {
              button.setText("Hello");
              button.setEnabled(true);
              other.doSomething();
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      BlockStmt block =
          cu.findFirst(ClassOrInterfaceDeclaration.class).get().getMethods().get(0).getBody().get();

      int insertPoint = AstModifier.findInsertionPointForVariable(block, "button");

      assertEquals(1, insertPoint);
    }

    @Test
    void shouldReturnMinusOneWhenNoCallsFound() {
      String code = """
          class Test {
            void method() {
              other.setText("Hello");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      BlockStmt block =
          cu.findFirst(ClassOrInterfaceDeclaration.class).get().getMethods().get(0).getBody().get();

      int insertPoint = AstModifier.findInsertionPointForVariable(block, "button");

      assertEquals(-1, insertPoint);
    }
  }

  @Nested
  class AddSetterWithSmartPosition {

    @Test
    void shouldInsertAfterLastConsecutiveCallOnVariable() {
      String code = """
          class Test {
            Test() {
              button.setText("Hello");
              button.setTooltip("Tip");
              other.doSomething();
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      ClassOrInterfaceDeclaration classDecl = cu.findFirst(ClassOrInterfaceDeclaration.class).get();
      SourceChange change =
          SourceChange.builder().methodCall("setEnabled", new BooleanLiteralExpr(true)).build();
      Statement setterStmt = AstModifier.createSetterStatement("button", change);

      AstModifier.addSetterWithSmartPosition(classDecl, "button", setterStmt);

      BlockStmt body = classDecl.getConstructors().get(0).getBody();
      assertEquals(4, body.getStatements().size());
      assertTrue(body.getStatement(0).toString().contains("setText"));
      assertTrue(body.getStatement(1).toString().contains("setTooltip"));
      assertTrue(body.getStatement(2).toString().contains("setEnabled"));
      assertTrue(body.getStatement(3).toString().contains("doSomething"));
    }

    @Test
    void shouldAppendToEndWhenNoCallsOnVariable() {
      String code = """
          class Test {
            Test() {
              other.doSomething();
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      ClassOrInterfaceDeclaration classDecl = cu.findFirst(ClassOrInterfaceDeclaration.class).get();
      SourceChange change =
          SourceChange.builder().methodCall("setEnabled", new BooleanLiteralExpr(true)).build();
      Statement setterStmt = AstModifier.createSetterStatement("button", change);

      AstModifier.addSetterWithSmartPosition(classDecl, "button", setterStmt);

      BlockStmt body = classDecl.getConstructors().get(0).getBody();
      assertEquals(2, body.getStatements().size());
      assertTrue(body.getStatement(1).toString().contains("setEnabled"));
    }

    @Test
    void shouldCreateConstructorWhenMissing() {
      String code = """
          class Test {
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      ClassOrInterfaceDeclaration classDecl = cu.findFirst(ClassOrInterfaceDeclaration.class).get();
      SourceChange change =
          SourceChange.builder().methodCall("setEnabled", new BooleanLiteralExpr(true)).build();
      Statement setterStmt = AstModifier.createSetterStatement("button", change);

      AstModifier.addSetterWithSmartPosition(classDecl, "button", setterStmt);

      assertEquals(1, classDecl.getConstructors().size());
      assertTrue(classDecl.getConstructors().get(0).getBody().toString().contains("setEnabled"));
    }
  }

  @Nested
  class AddSetterToConstructor {

    @Test
    void shouldCreateConstructorIfMissing() {
      String code = """
          class Test {
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      ClassOrInterfaceDeclaration classDecl = cu.findFirst(ClassOrInterfaceDeclaration.class).get();
      SourceChange change =
          SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();
      Statement setterStmt = AstModifier.createSetterStatement("button", change);

      AstModifier.addSetterToConstructor(classDecl, setterStmt);

      assertEquals(1, classDecl.getConstructors().size());
      assertTrue(classDecl.getConstructors().get(0).getBody().toString().contains("setText"));
    }
  }

  @Nested
  class GenerateFreeVariableName {

    @Test
    void shouldReturnBaseNameWhenAvailable() {
      String code = """
          class Test {
            void method() {
              String other = "test";
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      BlockStmt block =
          cu.findFirst(ClassOrInterfaceDeclaration.class).get().getMethods().get(0).getBody().get();

      String name = AstModifier.generateFreeVariableName("button", block);

      assertEquals("button", name);
    }

    @Test
    void shouldAppendSuffixWhenNameTaken() {
      String code = """
          class Test {
            void method() {
              Button button = new Button();
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      BlockStmt block =
          cu.findFirst(ClassOrInterfaceDeclaration.class).get().getMethods().get(0).getBody().get();

      String name = AstModifier.generateFreeVariableName("button", block);

      assertEquals("button2", name);
    }

    @Test
    void shouldIncrementSuffixUntilAvailable() {
      String code = """
          class Test {
            void method() {
              Button button = new Button();
              Button button2 = new Button();
              Button button3 = new Button();
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      BlockStmt block =
          cu.findFirst(ClassOrInterfaceDeclaration.class).get().getMethods().get(0).getBody().get();

      String name = AstModifier.generateFreeVariableName("button", block);

      assertEquals("button4", name);
    }
  }

  @Nested
  class AddImportIfNotExists {

    @Test
    void shouldAddNewImport() {
      String code = """
          package com.example;

          class Test {
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      AstModifier.addImportIfNotExists(cu, "com.webforj.component.Button");

      assertTrue(cu.getImports().stream()
          .anyMatch(imp -> imp.getNameAsString().equals("com.webforj.component.Button")));
    }

    @Test
    void shouldNotDuplicateExistingImport() {
      String code = """
          package com.example;

          import com.webforj.component.Button;

          class Test {
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      AstModifier.addImportIfNotExists(cu, "com.webforj.component.Button");

      assertEquals(1, cu.getImports().stream()
          .filter(imp -> imp.getNameAsString().equals("com.webforj.component.Button")).count());
    }
  }

  @Nested
  class ExtractToVariableAndAddSetters {

    @Test
    void shouldExtractObjectCreationExpr() {
      String code = """
          class Test {
            void method() {
              add(new Button());
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      ObjectCreationExpr creation = cu.findFirst(ObjectCreationExpr.class).get();
      SourceChange change =
          SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();

      boolean result =
          AstModifier.extractToVariableAndAddSetters(creation, List.of(change), "Button");

      assertTrue(result);
      String output = cu.toString();
      assertTrue(output.contains("Button button = new Button()"));
      assertTrue(output.contains("button.setText(\"Hello\")"));
      assertTrue(output.contains("add(button)"));
    }

    @Test
    void shouldExtractFactoryMethodCall() {
      String code = """
          class Test {
            void method() {
              add(Icon.create("x"));
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      MethodCallExpr factoryCall = cu.findAll(MethodCallExpr.class).stream()
          .filter(m -> m.getNameAsString().equals("create")).findFirst().get();
      SourceChange change =
          SourceChange.builder().methodCall("setSize", new StringLiteralExpr("lg")).build();

      boolean result =
          AstModifier.extractToVariableAndAddSetters(factoryCall, List.of(change), "Icon");

      assertTrue(result);
      String output = cu.toString();
      assertTrue(output.contains("Icon icon = Icon.create(\"x\")"));
      assertTrue(output.contains("icon.setSize(\"lg\")"));
      assertTrue(output.contains("add(icon)"));
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
      ObjectCreationExpr creation = cu.findAll(ObjectCreationExpr.class).stream()
          .filter(o -> o.findAncestor(MethodCallExpr.class).isPresent()).findFirst().get();
      SourceChange change =
          SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();

      boolean result =
          AstModifier.extractToVariableAndAddSetters(creation, List.of(change), "Button");

      assertTrue(result);
      String output = cu.toString();
      assertTrue(output.contains("Button button2 = new Button()"));
      assertTrue(output.contains("button2.setText(\"Hello\")"));
    }

    @Test
    void shouldReturnFalseWhenNoBlockStmtAncestor() {
      String code = """
          class Test {
            Button button = new Button();
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      ObjectCreationExpr creation = cu.findFirst(ObjectCreationExpr.class).get();
      SourceChange change =
          SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();

      boolean result =
          AstModifier.extractToVariableAndAddSetters(creation, List.of(change), "Button");

      assertFalse(result);
    }

    @Test
    void shouldAddMultipleSetters() {
      String code = """
          class Test {
            void method() {
              add(new Button());
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      ObjectCreationExpr creation = cu.findFirst(ObjectCreationExpr.class).get();
      SourceChange change1 =
          SourceChange.builder().methodCall("setText", new StringLiteralExpr("Hello")).build();
      SourceChange change2 =
          SourceChange.builder().methodCall("setEnabled", new BooleanLiteralExpr(true)).build();

      boolean result =
          AstModifier.extractToVariableAndAddSetters(creation, List.of(change1, change2), "Button");

      assertTrue(result);
      String output = cu.toString();
      assertTrue(output.contains("Button button = new Button()"));
      assertTrue(output.contains("button.setText(\"Hello\")"));
      assertTrue(output.contains("button.setEnabled(true)"));
      assertTrue(output.contains("add(button)"));
    }
  }

  @Nested
  class AccessorScopedCalls {

    @Test
    void shouldCreateSetterStatementWithAccessor() {
      SourceChange change =
          SourceChange.builder().methodCall("setPlaceholder", new StringLiteralExpr("Find"))
              .accessor("getSearch").build();

      Statement stmt = AstModifier.createSetterStatement("nav", change);

      assertEquals("nav.getSearch().setPlaceholder(\"Find\");", stmt.toString());
    }

    @Test
    void shouldCreateBoundComponentSetterStatementWithAccessor() {
      SourceChange change =
          SourceChange.builder().methodCall("setPlaceholder", new StringLiteralExpr("Find"))
              .accessor("getSearch").build();

      Statement stmt = AstModifier.createBoundComponentSetterStatement(change);

      assertEquals("getBoundComponent().getSearch().setPlaceholder(\"Find\");", stmt.toString());
    }

    @Test
    void shouldUpdateAccessorScopedCallOnly() {
      String code = """
          class Test {
            void method() {
              combo.setPlaceholder("Top");
              combo.getSearch().setPlaceholder("Old");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      SourceChange change = SourceChange.builder()
          .methodCall("setPlaceholder", new StringLiteralExpr("New")).accessor("getSearch").build();

      boolean updated = AstModifier.updateExistingSetterCall(cu, "combo", change);

      assertTrue(updated);
      String result = cu.toString();
      assertTrue(result.contains("combo.setPlaceholder(\"Top\")"));
      assertTrue(result.contains("combo.getSearch().setPlaceholder(\"New\")"));
    }

    @Test
    void shouldNotUpdateAccessorScopedCallWithoutAccessor() {
      String code = """
          class Test {
            void method() {
              combo.getSearch().setPlaceholder("Old");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      SourceChange change =
          SourceChange.builder().methodCall("setPlaceholder", new StringLiteralExpr("New")).build();

      boolean updated = AstModifier.updateExistingSetterCall(cu, "combo", change);

      assertFalse(updated);
    }

    @Test
    void shouldRemoveAccessorScopedCall() {
      String code = """
          class Test {
            void method() {
              combo.setPlaceholder("Top");
              combo.getSearch().setPlaceholder("Old");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);

      boolean removed = AstModifier.removeMethodCall(cu, "combo", "setPlaceholder", "getSearch");

      assertTrue(removed);
      String result = cu.toString();
      assertTrue(result.contains("combo.setPlaceholder(\"Top\")"));
      assertFalse(result.contains("getSearch()"));
    }

    @Test
    void shouldUpdateAccessorScopedBoundComponentCall() {
      String code = """
          class Test {
            Test() {
              getBoundComponent().setPlaceholder("Top");
              getBoundComponent().getSearch().setPlaceholder("Old");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      SourceChange change = SourceChange.builder()
          .methodCall("setPlaceholder", new StringLiteralExpr("New")).accessor("getSearch").build();

      boolean updated = AstModifier.updateExistingBoundComponentSetterCall(cu, change);

      assertTrue(updated);
      String result = cu.toString();
      assertTrue(result.contains("getBoundComponent().setPlaceholder(\"Top\")"));
      assertTrue(result.contains("getBoundComponent().getSearch().setPlaceholder(\"New\")"));
    }

    @Test
    void shouldIgnoreFluentChainsWhenResolvingAccessor() {
      String code = """
          class Test {
            void method() {
              button.setText("a").setTheme("b");
            }
          }
          """;
      CompilationUnit cu = StaticJavaParser.parse(code);
      MethodCallExpr setTheme = cu.findAll(MethodCallExpr.class).stream()
          .filter(mc -> mc.getNameAsString().equals("setTheme")).findFirst().get();

      assertEquals(null, AstModifier.getDirectAccessor(setTheme));
    }
  }
}
