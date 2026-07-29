package com.webforj.devtools.craftforj.inspector.source.parser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.expr.DoubleLiteralExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.IntegerLiteralExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange.ItemPosition;
import java.util.List;
import java.util.function.Predicate;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("AstModifier parent-scoped item calls")
class AstModifierItemCallTest {

  private static Predicate<MethodCallExpr> onLayout() {
    return mc -> AstModifier.isMethodCallOnVariable(mc, "layout");
  }

  private static SourceChange growChange(double value, String item) {
    return SourceChange.builder()
        .methodCall("setItemGrow", List.of(new DoubleLiteralExpr(value), new NameExpr(item)))
        .itemRef(item, ItemPosition.LAST).build();
  }

  private static SourceChange spanChange(String item, int value) {
    return SourceChange.builder()
        .methodCall("setSpan",
            List.of(new NameExpr(item), new IntegerLiteralExpr(String.valueOf(value))))
        .itemRef(item, ItemPosition.FIRST).build();
  }

  private static BlockStmt methodBody(CompilationUnit cu) {
    return cu.findFirst(ClassOrInterfaceDeclaration.class).get().getMethods().get(0).getBody()
        .get();
  }

  @Nested
  @DisplayName("updateExistingItemCall")
  class UpdateExistingItemCall {

    @Test
    @DisplayName("updates the value keeping the item argument")
    void shouldUpdateValueKeepingItem() {
      CompilationUnit cu = StaticJavaParser.parse("""
          class Test {
            void m() {
              layout.setItemGrow(1.0, btn);
            }
          }
          """);

      boolean updated = AstModifier.updateExistingItemCall(cu, onLayout(), growChange(3.0, "btn"));

      assertTrue(updated);
      assertTrue(cu.toString().contains("layout.setItemGrow(3.0, btn)"));
    }

    @Test
    @DisplayName("does not touch a call referencing a different item")
    void shouldNotTouchDifferentItem() {
      CompilationUnit cu = StaticJavaParser.parse("""
          class Test {
            void m() {
              layout.setItemGrow(1.0, other);
            }
          }
          """);

      boolean updated = AstModifier.updateExistingItemCall(cu, onLayout(), growChange(3.0, "btn"));

      assertFalse(updated);
      assertTrue(cu.toString().contains("layout.setItemGrow(1.0, other)"));
    }

    @Test
    @DisplayName("arity guard: a 3-arg breakpoint call is not matched by a 2-arg FIRST change")
    void shouldNotMatchBreakpointOverloadByArity() {
      CompilationUnit cu = StaticJavaParser.parse("""
          class Test {
            void m() {
              layout.setSpan(btn, "md", 2);
            }
          }
          """);

      boolean updated = AstModifier.updateExistingItemCall(cu, onLayout(), spanChange("btn", 4));

      assertFalse(updated);
      assertTrue(cu.toString().contains("layout.setSpan(btn, \"md\", 2)"));
    }

    @Test
    @DisplayName("container overload with one arg is not matched by a 2-arg FIRST item change")
    void shouldNotMatchContainerOverload() {
      CompilationUnit cu = StaticJavaParser.parse("""
          class Test {
            void m() {
              layout.setHorizontalAlignment(Alignment.CENTER);
            }
          }
          """);

      Expression value = StaticJavaParser.parseExpression("Alignment.CENTER");
      SourceChange change = SourceChange.builder()
          .methodCall("setHorizontalAlignment", List.of(new NameExpr("btn"), value))
          .itemRef("btn", ItemPosition.FIRST).build();

      boolean updated = AstModifier.updateExistingItemCall(cu, onLayout(), change);

      assertFalse(updated);
      assertTrue(cu.toString().contains("layout.setHorizontalAlignment(Alignment.CENTER)"));
    }

    @Test
    @DisplayName("varargs multi-item: detaches the item and returns false for a dedicated call")
    void shouldDetachItemFromVarargsCall() {
      CompilationUnit cu = StaticJavaParser.parse("""
          class Test {
            void m() {
              layout.setItemGrow(1.0, a, b);
            }
          }
          """);

      boolean updated = AstModifier.updateExistingItemCall(cu, onLayout(), growChange(2.0, "a"));

      assertFalse(updated);
      assertTrue(cu.toString().contains("layout.setItemGrow(1.0, b)"));
      assertFalse(cu.toString().contains(", a"));
    }

    @Test
    @DisplayName("bound-component scope variant is updated in place")
    void shouldUpdateBoundComponentScopedItemCall() {
      CompilationUnit cu = StaticJavaParser.parse("""
          class Test {
            void m() {
              getBoundComponent().setItemGrow(1.0, btn);
            }
          }
          """);

      boolean updated = AstModifier.updateExistingItemCall(cu,
          AstModifier::isMethodCallOnBoundComponent, growChange(3.0, "btn"));

      assertTrue(updated);
      assertTrue(cu.toString().contains("getBoundComponent().setItemGrow(3.0, btn)"));
    }
  }

  @Nested
  @DisplayName("removeItemCall")
  class RemoveItemCall {

    @Test
    @DisplayName("removes the whole statement for a single-item call")
    void shouldRemoveSingleItemCall() {
      CompilationUnit cu = StaticJavaParser.parse("""
          class Test {
            void m() {
              layout.setItemGrow(1.0, btn);
            }
          }
          """);

      boolean removed =
          AstModifier.removeItemCall(cu, onLayout(), "setItemGrow", "btn", ItemPosition.LAST, 2);

      assertTrue(removed);
      assertFalse(cu.toString().contains("setItemGrow"));
    }

    @Test
    @DisplayName("detaches only the item from a multi-item varargs call")
    void shouldDetachItemFromVarargsCall() {
      CompilationUnit cu = StaticJavaParser.parse("""
          class Test {
            void m() {
              layout.setItemGrow(1.0, a, b);
            }
          }
          """);

      boolean removed =
          AstModifier.removeItemCall(cu, onLayout(), "setItemGrow", "a", ItemPosition.LAST, 2);

      assertTrue(removed);
      assertTrue(cu.toString().contains("layout.setItemGrow(1.0, b)"));
    }

    @Test
    @DisplayName("removes a FIRST-position call with matching arity")
    void shouldRemoveFirstPositionCall() {
      CompilationUnit cu = StaticJavaParser.parse("""
          class Test {
            void m() {
              layout.setSpan(btn, 2);
            }
          }
          """);

      boolean removed =
          AstModifier.removeItemCall(cu, onLayout(), "setSpan", "btn", ItemPosition.FIRST, 2);

      assertTrue(removed);
      assertFalse(cu.toString().contains("setSpan"));
    }

    @Test
    @DisplayName("leaves a wrong-arity call untouched")
    void shouldLeaveWrongArityCall() {
      CompilationUnit cu = StaticJavaParser.parse("""
          class Test {
            void m() {
              layout.setSpan(btn, "md", 2);
            }
          }
          """);

      boolean removed =
          AstModifier.removeItemCall(cu, onLayout(), "setSpan", "btn", ItemPosition.FIRST, 2);

      assertFalse(removed);
      assertTrue(cu.toString().contains("layout.setSpan(btn, \"md\", 2)"));
    }
  }

  @Nested
  @DisplayName("findInsertionPointForItemCall")
  class FindInsertionPointForItemCall {

    @Test
    @DisplayName("returns the last statement referencing the parent scope or the item")
    void shouldReturnLastRelevantStatement() {
      CompilationUnit cu = StaticJavaParser.parse("""
          class Test {
            void m() {
              FlexLayout layout = new FlexLayout();
              Button btn = new Button();
              layout.add(btn);
              System.out.println("x");
            }
          }
          """);
      BlockStmt block = methodBody(cu);

      int index = AstModifier.findInsertionPointForItemCall(block, onLayout(), "btn");

      assertEquals(2, index);
    }

    @Test
    @DisplayName("returns -1 when neither the parent nor the item is referenced")
    void shouldReturnMinusOneWhenNothingRelevant() {
      CompilationUnit cu = StaticJavaParser.parse("""
          class Test {
            void m() {
              System.out.println("x");
            }
          }
          """);
      BlockStmt block = methodBody(cu);

      int index = AstModifier.findInsertionPointForItemCall(block, onLayout(), "btn");

      assertEquals(-1, index);
    }
  }

  @Nested
  @DisplayName("extractToVariable")
  class ExtractToVariable {

    @Test
    @DisplayName("inserts a declaration and rewrites the call to use the variable")
    void shouldExtractInlineCreation() {
      CompilationUnit cu = StaticJavaParser.parse("""
          class Test {
            void m() {
              add(new Button("x"));
            }
          }
          """);
      ObjectCreationExpr creation = cu.findFirst(ObjectCreationExpr.class).get();

      String name = AstModifier.extractToVariable(creation, "Button");

      assertEquals("button", name);
      String output = cu.toString();
      assertTrue(output.contains("Button button = new Button(\"x\")"));
      assertTrue(output.contains("add(button)"));
    }
  }

  @Nested
  @DisplayName("addSettersForVariable with an item change")
  class AddSettersForVariable {

    private CompilationUnit fixture() {
      return StaticJavaParser.parse("""
          class Test {
            void m() {
              FlexLayout layout = new FlexLayout();
              Button btn = new Button();
              layout.add(btn);
            }
          }
          """);
    }

    @Test
    @DisplayName("inserts the item call after layout.add(btn)")
    void shouldInsertAfterAdd() {
      CompilationUnit cu = fixture();
      BlockStmt block = methodBody(cu);

      AstModifier.addSettersForVariable(cu, block, "layout", List.of(growChange(1.0, "btn")));

      String output = cu.toString();
      int addIndex = output.indexOf("layout.add(btn)");
      int growIndex = output.indexOf("layout.setItemGrow(1.0, btn)");
      assertTrue(addIndex >= 0);
      assertTrue(growIndex > addIndex);
    }

    @Test
    @DisplayName("re-running with a new value updates in place (idempotent upsert)")
    void shouldUpsertIdempotently() {
      CompilationUnit cu = fixture();
      BlockStmt block = methodBody(cu);

      AstModifier.addSettersForVariable(cu, block, "layout", List.of(growChange(1.0, "btn")));
      AstModifier.addSettersForVariable(cu, block, "layout", List.of(growChange(2.0, "btn")));

      String output = cu.toString();
      assertTrue(output.contains("layout.setItemGrow(2.0, btn)"));
      assertFalse(output.contains("setItemGrow(1.0"));
      int first = output.indexOf("setItemGrow");
      assertEquals(first, output.lastIndexOf("setItemGrow"));
    }
  }
}
