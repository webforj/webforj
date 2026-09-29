package com.webforj.devtools.craftforj.source.site;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.github.javaparser.ast.CompilationUnit;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.source.site.model.CreationSite;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("SourceSites")
class SourceSitesTest {

  private static final String SOURCE = """
      package app;

      import com.webforj.component.button.Button;
      import java.util.List;

      public class View<T> {
        static final Button SHARED = new Button("Shared");

        static {
          SHARED.setText(String.valueOf(new Button("Block").getText()));
        }

        View(T value, List<T>[] values, int... sizes) {
          new Button("Generic");
        }

        void fill(String text) {
          new Button(text);
        }

        void fill(StringBuilder text) {
          new Button(text.toString());
        }

        enum Kind {
          FIRST(new Button("First")), SECOND(new Button("Second"));

          Kind(Button button) {
          }
        }
      }
      """;

  private static final String HIDDEN = """
      package app;

      import com.webforj.component.button.Button;
      import java.util.function.Function;

      public class Hidden {
        Runnable field = () -> new Button("Field");

        void build() {
          Runnable first = () -> new Button("First");
          Function<String, Button> typed = (String text) -> new Button(text);
          Runnable outer = new Runnable() {
            @Override
            public void run() {
              new Button("Anonymous");
              Runnable inner = () -> new Button("Inner");
            }
          };
          class Local {
            Local() {
              new Button("Local");
            }
          }
        }

        void other() {
          Runnable second = () -> new Button("Second");
          Runnable third = () -> new Button("Third");
        }
      }
      """;

  private final CompilationUnit cu = new SourceParserService().parse(SOURCE).orElseThrow();
  private final CompilationUnit hidden = new SourceParserService().parse(HIDDEN).orElseThrow();

  @Test
  @DisplayName("should count the static initializers of a class in the order they are written")
  void shouldFindInStaticInitializer() {
    assertEquals("new Button(\"Shared\")",
        find(create("app.View", "<clinit>", "()V", 0, 2)).toString());
    assertEquals("new Button(\"Block\")",
        find(create("app.View", "<clinit>", "()V", 1, 2)).toString());
  }

  @Test
  @DisplayName("should count the constants of an enum before its static initializers")
  void shouldFindInEnumConstant() {
    assertEquals("new Button(\"Second\")",
        find(create("app.View$Kind", "<clinit>", "()V", 1, 2)).toString());
  }

  @Test
  @DisplayName("should read a parameter of a type variable as any compiled type")
  void shouldMatchTypeVariables() {
    assertEquals("new Button(\"Generic\")",
        find(create("app.View", "<init>", "(Ljava/lang/Object;[Ljava/util/List;[I)V", 0, 1))
            .toString());
  }

  @Test
  @DisplayName("should pick the method whose parameters the compiled method has")
  void shouldPickMethodByParameters() {
    assertEquals("new Button(text.toString())",
        find(create("app.View", "fill", "(Ljava/lang/StringBuilder;)V", 0, 1)).toString());
  }

  @Test
  @DisplayName("should find a lambda by the member it is written in and by its parameters")
  void shouldFindInLambda() {
    assertEquals("new Button(\"Field\")",
        findHidden(create("app.Hidden", "lambda$new$0", "()V", 0, 1)).toString());
    assertEquals("new Button(\"First\")",
        findHidden(create("app.Hidden", "lambda$build$1", "()V", 0, 1)).toString());
    assertEquals("new Button(\"Inner\")",
        findHidden(create("app.Hidden$1", "lambda$run$0", "()V", 0, 1)).toString());
  }

  @Test
  @DisplayName("should ask the line only where several lambdas fit")
  void shouldTellLambdasApartByLine() {
    String descriptor = "(Ljava/lang/String;)Lcom/webforj/component/button/Button;";

    assertEquals("new Button(text)",
        findHidden(create("app.Hidden", "lambda$build$2", descriptor, 0, 1, 11)).toString());
    assertEquals("new Button(\"Third\")",
        findHidden(create("app.Hidden", "lambda$4", "()V", 0, 1, 28)).toString());
    assertEquals(
        "Hidden holds 2 places that fit the code that created this component, which one cannot be"
            + " told",
        assertThrows(SourceModificationException.class,
            () -> findHidden(create("app.Hidden", "lambda$other$3", "()V", 0, 1))).getMessage());
  }

  @Test
  @DisplayName("should find a class the compiler numbered by what it holds")
  void shouldFindInNumberedClass() {
    assertEquals("new Button(\"Anonymous\")",
        findHidden(create("app.Hidden$1", "run", "()V", 0, 1)).toString());
    assertEquals("new Button(\"Local\")",
        findHidden(create("app.Hidden$1Local", "<init>", "(Lapp/Hidden;)V", 0, 1)).toString());
  }

  @Test
  @DisplayName("should refuse what the file does not hold the way the application ran it")
  void shouldRefuseWhatChanged() {
    assertRefused("The source no longer declares Missing",
        create("app.Missing", "<init>", "()V", 0, 1));
    assertRefused("The source no longer declares View.Gone",
        create("app.View$Gone", "<init>", "()V", 0, 1));
    assertRefused(
        "View.fill() was changed after the application was compiled, save and reload it" + " first",
        create("app.View", "fill", "(I)V", 0, 1));
    assertRefused(
        "View.fill() was changed after the application was compiled, save and reload it" + " first",
        create("app.View", "fill", "(Ljava/lang/String;)V", 0, 2));
    assertRefused("The static initializer of View was changed after the application was compiled,"
        + " save and reload it first", create("app.View", "<clinit>", "()V", 0, 3));
    assertRefused("View no longer holds the class that created this component",
        create("app.View$1", "<init>", "()V", 0, 1));
    assertRefused(
        "A lambda of View was changed after the application was compiled, save and reload it first",
        create("app.View", "lambda$fill$0", "()V", 0, 1));
    assertRefused("A constructor that hands over to another one creates no component by itself",
        CreationSite.builder().setClassName("app.View").setMethod("<init>", "()V")
            .setKind(CreationSite.Kind.DELEGATION).setName("").setPosition(0, 1).build());
    assertRefused("A method the compiler wrote is not a part of the source",
        CreationSite.builder().setClassName("app.View").setMethod("fill", "(Ljava/lang/Object;)V")
            .setKind(CreationSite.Kind.BRIDGE).setName("fill").setPosition(0, 1).build());
  }

  @Test
  @DisplayName("should tell two sites apart by every part of them")
  void shouldCompareSites() {
    CreationSite site = create("app.View", "fill", "(I)V", 0, 1);

    assertEquals(site, create("app.View", "fill", "(I)V", 0, 1));
    assertEquals(site.hashCode(), create("app.View", "fill", "(I)V", 0, 1).hashCode());
    assertNotEquals(site, create("app.View", "fill", "(I)V", 1, 2));
    assertNotEquals(site, create("app.Other", "fill", "(I)V", 0, 1));
    assertNotEquals(site, create("app.View", "other", "(I)V", 0, 1));
    assertNotEquals(site, create("app.View", "fill", "(J)V", 0, 1));
    assertNotEquals(site, create("app.View", "fill", "(I)V", 0, 1, 7));
    assertNotEquals("site", site);
  }

  private Object find(CreationSite site) {
    return SourceSites.find(cu, site);
  }

  private Object findHidden(CreationSite site) {
    return SourceSites.find(hidden, site);
  }

  private void assertRefused(String message, CreationSite site) {
    assertEquals(message,
        assertThrows(SourceModificationException.class, () -> find(site)).getMessage());
  }

  private static CreationSite create(String className, String method, String descriptor,
      int ordinal, int count) {
    return create(className, method, descriptor, ordinal, count, 0);
  }

  private static CreationSite create(String className, String method, String descriptor,
      int ordinal, int count, int line) {
    return CreationSite.builder().setClassName(className).setMethod(method, descriptor)
        .setKind(CreationSite.Kind.CREATION).setName("Button")
        .setProducedType("com.webforj.component.button.Button").setPosition(ordinal, count)
        .setLine(line).build();
  }
}
