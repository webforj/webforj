package com.webforj.devtools.craftforj.source.resolver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("VariableResolver")
class VariableResolverTest {

  @Test
  @DisplayName("should bind a field through every scope and leave the same name of other scopes")
  void shouldFindReferencesOfField() {
    CompilationUnit cu = parse("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import java.io.StringReader;
        import java.util.List;

        public class View {
          private final Button save = new Button("Save");

          public View(FlexLayout self, int mode) throws Exception {
            View.this.save.setText("Store");
            self.add(save);
            Runnable focus = save::focus;
            for (Button save : List.of(new Button())) {
              save.setText("Loop");
            }
            for (Button save = new Button(); save != null; save = null) {
              save.setText("Counter");
            }
            try (StringReader save = new StringReader("x")) {
              save.read();
            } catch (RuntimeException save) {
              save.printStackTrace();
            } finally {
              save.focus();
            }
            switch (mode) {
              case 1:
                StringReader save = new StringReader("y");
                save.ready();
              case 2:
                save = null;
                break;
              default:
                this.save.setText("Default");
                break;
            }
            self.add(new Object() {
              private Button save = new Button();

              Button get() {
                return save;
              }
            }.get());
            List.of(new Button()).forEach(save -> save.setText("Lambda"));
          }

          void show(Button save) {
            save.setText("Parameter");
          }

          void hide() {
            Button save = new Button("Local");
            save.setVisible(false);
          }

          static class Inner {
            void focus(View view) {
              view.save.focus();
            }
          }
        }
        """);

    List<Expression> references = VariableResolver.findReferences(findVariable(cu, "save", 9));

    assertEquals(List.of(12, 13, 14, 26, 36, 60), references.stream().map(this::getLine).toList());
  }

  @Test
  @DisplayName("should bind a local only below its declaration and inside its block")
  void shouldFindReferencesOfLocal() {
    CompilationUnit cu = parse("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final Button cta = new Button("Field");

          public View(FlexLayout self) {
            self.add(cta);
            Button cta = new Button("Local");
            cta.setText(cta.getText() + "!");
            self.add(cta);
          }

          void other(FlexLayout self) {
            self.add(cta);
          }
        }
        """);

    List<Expression> references = VariableResolver.findReferences(findVariable(cu, "cta", 11));

    assertEquals(List.of(12, 12, 13), references.stream().map(this::getLine).toList());
  }

  @Test
  @DisplayName("should leave a name to the class in between that inherits a field of that name")
  void shouldLeaveInheritedName() {
    CompilationUnit cu = parse("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import java.util.ArrayList;

        class View {
          private Button target = new Button("Selected");

          class Known extends Base {
            void attach(FlexLayout layout) {
              layout.add(target);
            }
          }

          class Loaded extends ArrayList<String> {
            void attach(FlexLayout layout) {
              layout.add(target);
            }
          }
        }

        class Base {
          protected Button target = new Button("Inherited");
        }
        """);

    List<Expression> references = VariableResolver.findReferences(findVariable(cu, "target", 8));

    assertEquals(List.of(18), references.stream().map(this::getLine).toList());
  }

  @Test
  @DisplayName("should refuse a name a class in between may inherit from a parent it cannot read")
  void shouldRefuseUnknownParent() {
    CompilationUnit cu = parse("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        class View {
          private Button target = new Button("Selected");

          Object inner = new Missing() {
            void attach(FlexLayout layout) {
              layout.add(target);
            }
          };
        }
        """);
    VariableDeclarator target = findVariable(cu, "target", 7);

    SourceModificationException refused = assertThrows(SourceModificationException.class,
        () -> VariableResolver.findReferences(target));

    assertEquals("target at line 11 may be a field of Missing, which cannot be resolved",
        refused.getMessage());
  }

  @Test
  @DisplayName("should refuse a name that is also matched as a pattern")
  void shouldRefusePattern() {
    CompilationUnit cu = parse("""
        package app;

        import com.webforj.component.button.Button;

        class View {
          private final Button save = new Button("Save");

          View(Object value) {
            if (value instanceof Button save) {
              save.focus();
            }
          }
        }
        """);
    VariableDeclarator save = findVariable(cu, "save", 6);

    SourceModificationException refused = assertThrows(SourceModificationException.class,
        () -> VariableResolver.findReferences(save));

    assertEquals("save is also matched as a pattern at line 9", refused.getMessage());
  }

  @Test
  @DisplayName("should find the parameter and the variable a name is bound to")
  void shouldFindDeclaration() {
    CompilationUnit cu = parse("""
        package app;

        import com.webforj.component.button.Button;

        class View {
          private Button field = new Button();

          void show(Button parameter) {
            parameter.focus();
            (field).focus();
            TablerIcon.create("plus");
          }
        }
        """);

    assertEquals(Parameter.class, findDeclaration(cu, "parameter").getClass());
    assertSame(findVariable(cu, "field", 6), findDeclaration(cu, "field"));
    assertNull(findDeclaration(cu, "TablerIcon"));
  }

  @Test
  @DisplayName("should find the variable a creation is stored in")
  void shouldFindHolder() {
    CompilationUnit cu = parse("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        class View {
          private Button assigned;

          View(FlexLayout layout) {
            Button initialized = new Button("Initialized");
            this.assigned = new Button("Assigned");
            layout.add(new Button("Inline"));
          }
        }
        """);
    List<ObjectCreationExpr> creations = cu.findAll(ObjectCreationExpr.class);

    assertSame(findVariable(cu, "initialized", 10), VariableResolver.findHolder(creations.get(0)));
    assertSame(findVariable(cu, "assigned", 7), VariableResolver.findHolder(creations.get(1)));
    assertNull(VariableResolver.findHolder(creations.get(2)));
  }

  private static CompilationUnit parse(String source) {
    return new SourceParserService().parse(source).orElseThrow();
  }

  private static VariableDeclarator findVariable(CompilationUnit cu, String name, int line) {
    return cu.findAll(VariableDeclarator.class).stream()
        .filter(variable -> variable.getNameAsString().equals(name)
            && variable.getRange().orElseThrow().begin.line == line)
        .findFirst().orElseThrow();
  }

  private static Node findDeclaration(CompilationUnit cu, String name) {
    return VariableResolver.findDeclaration(cu.findAll(NameExpr.class).stream()
        .filter(reference -> reference.getNameAsString().equals(name)).findFirst().orElseThrow());
  }

  private int getLine(Expression expression) {
    return expression.getRange().orElseThrow().begin.line;
  }
}
