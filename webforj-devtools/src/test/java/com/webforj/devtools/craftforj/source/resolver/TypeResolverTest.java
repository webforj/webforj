package com.webforj.devtools.craftforj.source.resolver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.webforj.component.button.Button;
import com.webforj.component.button.ButtonTheme;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("TypeResolver")
class TypeResolverTest {

  @Test
  @DisplayName("should resolve a written type through the imports, the package and java.lang")
  void shouldResolveWrittenTypes() {
    CompilationUnit cu = parse("""
        package com.webforj.component.button;

        import com.webforj.component.layout.flexlayout.FlexLayout;
        import java.util.*;

        class View {
          FlexLayout imported;
          Button samePackage;
          String language;
          Map.Entry<String, String> nested;
          List<String> wildcard;
          com.webforj.component.button.ButtonTheme qualified;
          int[] numbers;
          Missing missing;
        }
        """);

    assertEquals(FlexLayout.class, resolveField(cu, "imported"));
    assertEquals(Button.class, resolveField(cu, "samePackage"));
    assertEquals(String.class, resolveField(cu, "language"));
    assertEquals(Map.Entry.class, resolveField(cu, "nested"));
    assertEquals(List.class, resolveField(cu, "wildcard"));
    assertEquals(ButtonTheme.class, resolveField(cu, "qualified"));
    assertEquals(int[].class, resolveField(cu, "numbers"));
    assertNull(resolveField(cu, "missing"));
  }

  @Test
  @DisplayName("should read a class the application did not load through the class it extends")
  void shouldResolveDeclaredTypeThroughAncestor() {
    CompilationUnit cu = parse("""
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        class View extends FlexLayout {
          class Inner {
          }
        }
        """);
    TypeDeclaration<?> view = cu.getType(0);
    TypeDeclaration<?> inner = (TypeDeclaration<?>) view.getMember(0);

    assertEquals(FlexLayout.class, TypeResolver.resolve(view));
    assertEquals(FlexLayout.class, TypeResolver.resolve("com.example.View", view));
    assertEquals("com.example.View$Inner", TypeResolver.getBinaryName(inner));
    assertNull(TypeResolver.resolve(inner));
  }

  @Test
  @DisplayName("should resolve the type an expression produces")
  void shouldResolveExpressions() {
    CompilationUnit cu = parse("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.icons.TablerIcon;

        class View {
          private final Button field = new Button("Field");

          void run(Button parameter, boolean flag) {
            var text = "text";
            var number = 1;
            var decimal = 1.5;
            var small = 1.5f;
            var letter = 'a';
            var large = 1L;
            var joined = text + number;
            var compared = number > 2;
            var theme = ButtonTheme.PRIMARY;
            var type = Button.class;
            var created = new Button("Created");
            var fluent = new Button("Fluent").setTheme(ButtonTheme.PRIMARY);
            var factory = TablerIcon.create("plus");
            var own = this.field;
            var given = parameter;
            var chosen = flag ? field : parameter;
            var cast = (Object) field;
            var label = field.getText();
            var unknown = missing();
          }
        }
        """);

    assertEquals(String.class, resolveLocal(cu, "text"));
    assertEquals(int.class, resolveLocal(cu, "number"));
    assertEquals(double.class, resolveLocal(cu, "decimal"));
    assertEquals(float.class, resolveLocal(cu, "small"));
    assertEquals(char.class, resolveLocal(cu, "letter"));
    assertEquals(long.class, resolveLocal(cu, "large"));
    assertEquals(String.class, resolveLocal(cu, "joined"));
    assertEquals(boolean.class, resolveLocal(cu, "compared"));
    assertEquals(ButtonTheme.class, resolveLocal(cu, "theme"));
    assertEquals(Class.class, resolveLocal(cu, "type"));
    assertEquals(Button.class, resolveLocal(cu, "created"));
    assertEquals(Button.class, resolveLocal(cu, "fluent"));
    assertEquals("com.webforj.component.icons.Icon", resolveLocal(cu, "factory").getName());
    assertEquals(Button.class, resolveLocal(cu, "own"));
    assertEquals(Button.class, resolveLocal(cu, "given"));
    assertEquals(Button.class, resolveLocal(cu, "chosen"));
    assertEquals(Object.class, resolveLocal(cu, "cast"));
    assertEquals(String.class, resolveLocal(cu, "label"));
    assertNull(resolveLocal(cu, "unknown"));
  }

  @Test
  @DisplayName("should resolve this and super to the class they are written in")
  void shouldResolveThisAndSuper() {
    CompilationUnit cu = parse("""
        package app;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        class View extends FlexLayout {
          void run() {
            var self = this;
            var outer = View.this;
            var parent = super.getComponents();
            var anonymous = new Runnable() {
              public void run() {
                var inner = this;
              }
            };
            var numbers = new int[] {1};
            var assigned = (self = null);
          }
        }
        """);

    assertEquals(FlexLayout.class, resolveLocal(cu, "self"));
    assertEquals(FlexLayout.class, resolveLocal(cu, "outer"));
    assertEquals(List.class, resolveLocal(cu, "parent"));
    assertEquals(Runnable.class, resolveLocal(cu, "anonymous"));
    assertEquals(Runnable.class, resolveLocal(cu, "inner"));
    assertNull(resolveLocal(cu, "numbers"));
    assertNull(resolveLocal(cu, "assigned"));
  }

  @Test
  @DisplayName("should tell a component class from any other class")
  void shouldTellComponents() {
    assertTrue(TypeResolver.isComponent(Button.class));
    assertFalse(TypeResolver.isComponent(String.class));
    assertFalse(TypeResolver.isComponent(null));
    assertEquals(FlexLayout.class,
        TypeResolver.load("com.webforj.component.layout.flexlayout.FlexLayout"));
    assertNull(TypeResolver.load("com.example.Missing"));
    assertNull(TypeResolver.load(""));
  }

  private static CompilationUnit parse(String source) {
    return new SourceParserService().parse(source).orElseThrow();
  }

  private static Class<?> resolveField(CompilationUnit cu, String name) {
    VariableDeclarator variable = findVariable(cu, name);

    return TypeResolver.resolve(variable.getType(), variable);
  }

  private static Class<?> resolveLocal(CompilationUnit cu, String name) {
    return TypeResolver.resolveType(findVariable(cu, name).getInitializer().orElseThrow());
  }

  private static VariableDeclarator findVariable(CompilationUnit cu, String name) {
    return cu.findAll(VariableDeclarator.class).stream()
        .filter(variable -> variable.getNameAsString().equals(name)).findFirst().orElseThrow();
  }
}
