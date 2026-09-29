package com.webforj.devtools.craftforj.source.resolver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.webforj.component.button.Button;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.layout.splitter.Splitter;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("MethodResolver")
class MethodResolverTest {

  private static final String SOURCE = """
      package app;

      import com.webforj.component.Composite;
      import com.webforj.component.button.Button;
      import com.webforj.component.button.ButtonTheme;
      import com.webforj.component.icons.TablerIcon;
      import com.webforj.component.layout.appnav.AppNavItem;
      import com.webforj.component.layout.flexlayout.FlexLayout;
      import com.webforj.component.layout.splitter.Splitter;

      class View extends Composite<FlexLayout> {
        View(FlexLayout layout, Splitter splitter, Button save, Button cancel) {
          layout.add(save, cancel);
          layout.setItemGrow(1.0, save);
          splitter.setMaster(save);
          String.format("%s", save);
          getBoundComponent().add(save);
          configure(save);
          this.configure(cancel);
          layout.missing(save);
          new FlexLayout(save, cancel);
          new AppNavItem("Home", "/home", TablerIcon.create("home"));
          new Button("Fixed");
          FlexLayout.create(save).vertical().build();
          new Button("Chain").setTheme(ButtonTheme.PRIMARY).getText().length();
        }

        private void configure(Button button) {
          button.setEnabled(false);
        }
      }
      """;

  private final CompilationUnit cu = new SourceParserService().parse(SOURCE).orElseThrow();

  @Test
  @DisplayName("should accept every argument of a parameter that takes any number of components")
  void shouldTellComponentVarargs() {
    assertTrue(MethodResolver.isComponentVarargs(findCall("add", 0), 0));
    assertTrue(MethodResolver.isComponentVarargs(findCall("add", 0), 1));
    assertFalse(MethodResolver.isComponentVarargs(findCall("setItemGrow", 0), 0));
    assertTrue(MethodResolver.isComponentVarargs(findCall("setItemGrow", 0), 1));
    assertFalse(MethodResolver.isComponentVarargs(findCall("setMaster", 0), 0));
    assertFalse(MethodResolver.isComponentVarargs(findCall("format", 0), 1));
    assertFalse(MethodResolver.isComponentVarargs(findCall("missing", 0), 0));
    assertTrue(MethodResolver.isComponentVarargs(findCall("create", 1), 0));
    assertTrue(MethodResolver.isComponentVarargs(findCreation("FlexLayout"), 1));
    assertFalse(MethodResolver.isComponentVarargs(findCreation("Button"), 0));
  }

  @Test
  @DisplayName("should accept the arguments of a class by the name of its method")
  void shouldTellComponentVarargsOfClass() {
    assertTrue(MethodResolver.isComponentVarargs(FlexLayout.class, "add", 3));
    assertFalse(MethodResolver.isComponentVarargs(FlexLayout.class, "setItemGrow", 0));
    assertFalse(MethodResolver.isComponentVarargs(Splitter.class, "setMaster", 0));
    assertFalse(MethodResolver.isComponentVarargs(String.class, "format", 1));
    assertTrue(MethodResolver.isComponentVarargs(FlexLayout.class, null, 0));
    assertFalse(MethodResolver.isComponentVarargs(Splitter.class, null, 0));
    assertFalse(MethodResolver.isComponentVarargs(null, "add", 0));
  }

  @Test
  @DisplayName("should bind a call made on the component a composite is built on")
  void shouldResolveReceiverOfBoundComponent() {
    assertEquals(FlexLayout.class, MethodResolver.resolveReceiver(findCall("add", 1)));
    assertTrue(MethodResolver.isComponentVarargs(findCall("add", 1), 0));
    assertNull(MethodResolver.resolveReturnType(findCall("missing", 0)));
  }

  @Test
  @DisplayName("should resolve what a fluent method returns to the class it is called on")
  void shouldResolveFluentReturnType() {
    assertEquals(Button.class, MethodResolver.resolveReturnType(findCall("setTheme", 0)));
    assertEquals(String.class, MethodResolver.resolveReturnType(findCall("getText", 0)));
    assertEquals("new Button(\"Chain\").setTheme(ButtonTheme.PRIMARY)",
        MethodResolver.findChainEnd(findCreations("Button").get(1)).toString());
  }

  @Test
  @DisplayName("should tell a method of the project from a method of a library")
  void shouldTellProjectMethods() {
    assertTrue(MethodResolver.isProjectMethod(findCall("configure", 0), type -> false));
    assertTrue(MethodResolver.isProjectMethod(findCall("configure", 1), type -> false));
    assertFalse(MethodResolver.isProjectMethod(findCall("getBoundComponent", 0), type -> false));
    assertTrue(MethodResolver.isProjectMethod(findCall("add", 0),
        type -> type.isAssignableFrom(FlexLayout.class)));
    assertFalse(MethodResolver.isProjectMethod(findCall("add", 0), type -> false));
  }

  @Test
  @DisplayName("should tell whether a creation can be written without one of its arguments")
  void shouldFindOverloadWithoutArgument() {
    assertTrue(MethodResolver.hasOverloadWithout(findCreation("AppNavItem"), 2));
    assertFalse(MethodResolver.hasOverloadWithout(findCreation("AppNavItem"), 0));
    assertFalse(MethodResolver.hasOverloadWithout(findCall("setMaster", 0), 0));
    assertFalse(MethodResolver.hasOverloadWithout(findCreation("FlexLayout"), 0));
  }

  @Test
  @DisplayName("should take a number that the parameter of a method is wide enough for")
  void shouldBindBoxedAndWidenedArguments() {
    CompilationUnit numbers = new SourceParserService().parse("""
        package app;

        import java.util.ArrayList;
        import java.util.List;

        class View {
          View(StringBuilder text, List<Object> values) {
            text.append(1).append('c').append(2L).append(true).append(1.5f);
            text.setLength((short) 1);
            values.add(1);
            values.add("text");
            Math.max(1, 2.5);
            Math.abs('c');
            new ArrayList<Object>(16);
            new StringBuilder(text, 3);
          }
        }
        """).orElseThrow();
    List<MethodCallExpr> calls = numbers.findAll(MethodCallExpr.class);
    List<ObjectCreationExpr> creations = numbers.findAll(ObjectCreationExpr.class);

    assertEquals(
        List.of("append:1", "append:1", "append:1", "append:1", "append:1", "setLength:1", "add:1",
            "add:1", "max:1", "abs:1"),
        calls.stream()
            .map(call -> call.getNameAsString() + ":" + MethodResolver.findMethods(call).size())
            .toList());
    assertEquals(double.class, MethodResolver.resolveReturnType(calls.get(8)));
    assertEquals(int.class, MethodResolver.resolveReturnType(calls.get(9)));
    assertTrue(MethodResolver.hasOverloadWithout(creations.get(0), 0));
    assertFalse(MethodResolver.hasOverloadWithout(creations.get(1), 0));
  }

  @Test
  @DisplayName("should tell a call made on a class from a call made on an object")
  void shouldTellStaticCalls() {
    assertTrue(MethodResolver.isStatic(findCall("create", 1)));
    assertFalse(MethodResolver.isStatic(findCall("add", 0)));
    assertFalse(MethodResolver.isStatic(findCall("missing", 0)));
  }

  private MethodCallExpr findCall(String name, int index) {
    return cu.findAll(MethodCallExpr.class).stream()
        .filter(call -> call.getNameAsString().equals(name)).toList().get(index);
  }

  private ObjectCreationExpr findCreation(String type) {
    return findCreations(type).get(0);
  }

  private List<ObjectCreationExpr> findCreations(String type) {
    return cu.findAll(ObjectCreationExpr.class).stream()
        .filter(creation -> creation.getType().getNameAsString().equals(type)).toList();
  }
}
