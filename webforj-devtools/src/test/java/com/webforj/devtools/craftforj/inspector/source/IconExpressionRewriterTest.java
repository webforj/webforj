package com.webforj.devtools.craftforj.inspector.source;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.webforj.devtools.craftforj.inspector.source.model.TargetContext;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("IconExpressionRewriter")
class IconExpressionRewriterTest {

  @Test
  @DisplayName("replaces a factory call passed as method argument")
  void shouldReplaceFactoryCallInMethodArgument() {
    String code = """
        class MyView {
          void init() {
            btn.setPrefixComponent(FeatherIcon.BELL.create());
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    List<String> imports =
        IconExpressionRewriter.rewrite(cu, new TargetContext(3, "Icon"), "tabler:home");

    assertTrue(cu.toString().contains("btn.setPrefixComponent(TablerIcon.create(\"home\"))"));
    assertEquals(List.of("com.webforj.component.icons." + "TablerIcon"), imports);
  }

  @Test
  @DisplayName("replaces a factory call passed as constructor argument")
  void shouldReplaceFactoryCallInConstructorArgument() {
    String code = """
        class MyView {
          void init() {
            addItem(new AppNavItem("Inbox", InboxView.class, TablerIcon.create("inbox")));
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    IconExpressionRewriter.rewrite(cu, new TargetContext(3, "Icon"), "feather:bell");

    assertTrue(cu.toString()
        .contains("new AppNavItem(\"Inbox\", InboxView.class, FeatherIcon.BELL.create())"));
  }

  @Test
  @DisplayName("preserves chained calls after the factory call")
  void shouldPreserveChainedCalls() {
    String code = """
        class MyView {
          void init() {
            Icon icon = TablerIcon.create("bell").setTheme(Theme.DANGER);
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    IconExpressionRewriter.rewrite(cu, new TargetContext(3, "Icon"), "dwc:calendar");

    assertTrue(cu.toString().contains("DwcIcon.CALENDAR.create().setTheme(Theme.DANGER)"));
  }

  @Test
  @DisplayName("replaces a factory call in a field declaration")
  void shouldReplaceFactoryCallInField() {
    String code = """
        class MyView {
          private final Icon icon = TablerIcon.create("home");
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    IconExpressionRewriter.rewrite(cu, new TargetContext(2, "Icon"), "feather:bell");

    assertTrue(cu.toString().contains("private final Icon icon = FeatherIcon.BELL.create();"));
  }

  @Test
  @DisplayName("rewrites name and pool literals of an Icon creation")
  void shouldRewriteIconCreationLiterals() {
    String code = """
        class MyView {
          void init() {
            add(new Icon("home", "tabler"));
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    List<String> imports =
        IconExpressionRewriter.rewrite(cu, new TargetContext(3, "Icon"), "feather:bell");

    assertTrue(cu.toString().contains("add(new Icon(\"bell\", \"feather\"))"));
    assertEquals(List.of(), imports);
  }

  @Test
  @DisplayName("keeps the IconButton constructor and rewrites its literals")
  void shouldRewriteIconButtonCreationLiterals() {
    String code = """
        class MyView {
          void init() {
            add(new IconButton("menu-2", "tabler"));
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    IconExpressionRewriter.rewrite(cu, new TargetContext(3, "IconButton"), "feather:bell");

    assertTrue(cu.toString().contains("add(new IconButton(\"bell\", \"feather\"))"));
  }

  @Test
  @DisplayName("targets the inner factory call when a creation wraps it")
  void shouldPreferInnerFactoryCall() {
    String code = """
        class MyView {
          void init() {
            add(new IconButton(TablerIcon.create("menu-2")));
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    IconExpressionRewriter.rewrite(cu, new TargetContext(3, "IconButton"), "feather:bell");

    assertTrue(cu.toString().contains("add(new IconButton(FeatherIcon.BELL.create()))"));
  }

  @Test
  @DisplayName("fails when multiple icon expressions share the line")
  void shouldFailOnAmbiguousLine() {
    String code = """
        class MyView {
          void init() {
            add(TablerIcon.create("a"), TablerIcon.create("b"));
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    SourceModificationException e = assertThrows(SourceModificationException.class,
        () -> IconExpressionRewriter.rewrite(cu, new TargetContext(3, "Icon"), "feather:bell"));

    assertTrue(e.getMessage().contains("Multiple icon expressions"));
  }

  @Test
  @DisplayName("fails when no icon expression exists at the line")
  void shouldFailWhenNoIconExpression() {
    String code = """
        class MyView {
          void init() {
            btn.setText("Hello");
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    SourceModificationException e = assertThrows(SourceModificationException.class,
        () -> IconExpressionRewriter.rewrite(cu, new TargetContext(3, "Icon"), "feather:bell"));

    assertTrue(e.getMessage().contains("No icon expression"));
  }

  @Test
  @DisplayName("fails on a creation without literal name and pool arguments")
  void shouldFailOnCreationWithoutLiterals() {
    String code = """
        class MyView {
          void init(Icon existing) {
            add(new IconButton(existing));
          }
        }
        """;
    CompilationUnit cu = StaticJavaParser.parse(code);

    SourceModificationException e =
        assertThrows(SourceModificationException.class, () -> IconExpressionRewriter.rewrite(cu,
            new TargetContext(3, "IconButton"), "feather:bell"));

    assertTrue(e.getMessage().contains("Cannot rewrite icon creation"));
  }
}
