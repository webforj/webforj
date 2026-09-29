package com.webforj.devtools.craftforj.source.site;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.github.javaparser.ast.CompilationUnit;
import com.webforj.component.Component;
import com.webforj.component.button.Button;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.source.site.model.CreationSite;
import com.webforj.devtools.craftforj.source.support.RuntimeSourceFixture;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("ComponentSiteResolver")
class ComponentSiteResolverTest {

  @TempDir
  Path directory;

  private RuntimeSourceFixture fixture;

  @BeforeEach
  void setUp() {
    fixture = new RuntimeSourceFixture(directory);
  }

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @Test
  @DisplayName("should tell creations apart that the compiler reports on one line")
  void shouldTellApartCreationsOfOneReportedLine() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(
                new Button(
                    describe("First")),
                new Button("Second"));
          }

          private static String describe(String text) {
            return text;
          }
        }
        """);
    run();

    SourceLocation first = resolve("First");
    SourceLocation second = resolve("Second");

    assertEquals(9, first.getLine());
    assertEquals(11, second.getLine());
    assertEquals("new Button(describe(\"First\"))", read(file, first));
    assertEquals("new Button(\"Second\")", read(file, second));
    assertEquals(CreationSite.builder().setClassName("app.View")
        .setMethod("<init>", "(Lcom/webforj/component/layout/flexlayout/FlexLayout;)V")
        .setKind(CreationSite.Kind.CREATION).setName("Button")
        .setProducedType("com.webforj.component.button.Button").setPosition(1, 2).setLine(10)
        .build(), second.getSite());
  }

  @Test
  @DisplayName("should count the initializers of the class before the body of the constructor")
  void shouldOrderInitializersBeforeConstructorBody() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final Button field = new Button("Field");
          private final Button block;

          {
            block = new Button("Block");
          }

          public View(FlexLayout layout) {
            this(layout, new Button("Argument"));
          }

          private View(FlexLayout layout, Button argument) {
            layout.add(field, block, argument, new Button("Body"));
          }
        }
        """);
    fixture.compile();
    FlexLayout layout = fixture.show(new FlexLayout());
    fixture.create("app.View", layout);

    assertEquals("new Button(\"Field\")", read(file, resolve("Field")));
    assertEquals("new Button(\"Block\")", read(file, resolve("Block")));
    assertEquals("new Button(\"Argument\")", read(file, resolve("Argument")));
    assertEquals("new Button(\"Body\")", read(file, resolve("Body")));
    assertEquals("field", resolve("Field").getVariableName());
    assertEquals("block", resolve("Block").getVariableName());
    assertNull(resolve("Body").getVariableName());
  }

  @Test
  @DisplayName("should find the expression behind switches, wide constants and lambdas")
  void shouldReadEveryInstructionLength() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import java.util.List;
        import java.util.function.Supplier;

        public class View {
          public View(FlexLayout layout) {
            long wide = 9_000_000_000L;
            double precise = 0.125d;
            int dense = 2;
            int sparse = 4000;
            String name = "two";
            switch (dense) {
              case 1:
                dense += 300;
                break;
              case 2:
                dense++;
                break;
              case 3:
                break;
              default:
                break;
            }
            switch (sparse) {
              case 10:
                break;
              case 4000:
                sparse = 1;
                break;
              default:
                break;
            }
            switch (name) {
              case "two":
                layout.add(new Button("Switch"));
                break;
              default:
                break;
            }
            int[][] grid = new int[2][3];
            Supplier<String> text = () -> "Lambda" + wide + precise + grid.length;
            List<String> names = List.of(text.get());
            Object first = names.isEmpty() ? null : names.get(0);
            layout.add(new Button(first instanceof String label ? label : "None"));
          }
        }
        """);
    run();

    assertEquals("new Button(\"Switch\")", read(file, resolve("Switch")));
    assertEquals("new Button(first instanceof String label ? label : \"None\")",
        read(file, fixture.createSiteResolver()
            .resolve(fixture.find(Button.class, button -> button.getText().startsWith("Lambda")))));
  }

  @Test
  @DisplayName("should pick the method that ran among methods of one name")
  void shouldPickOverload() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import java.util.List;

        public class View {
          public View(FlexLayout layout) {
            fill(layout, "Text");
            fill(layout, 7);
            fill(layout, List.of("Generic"));
          }

          private void fill(FlexLayout layout, String text) {
            layout.add(new Button(text));
          }

          private void fill(FlexLayout layout, int number) {
            layout.add(new Button("Number " + number));
          }

          private <T> void fill(FlexLayout layout, List<T> values) {
            layout.add(new Button(String.valueOf(values.get(0))));
          }
        }
        """);
    run();

    assertEquals("new Button(text)", read(file, resolve("Text")));
    assertEquals("new Button(\"Number \" + number)", read(file, resolve("Number 7")));
    assertEquals("new Button(String.valueOf(values.get(0)))", read(file, resolve("Generic")));
  }

  @Test
  @DisplayName("should resolve a component of a nested class and of an enum constant")
  void shouldResolveNestedTypes() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(new Inner().button, Kind.PRIMARY.create());
          }

          class Inner {
            private final Button button = new Button("Inner");
          }

          enum Kind {
            PRIMARY("Primary");

            private final String text;

            Kind(String text) {
              this.text = text;
            }

            Button create() {
              return new Button(text);
            }
          }
        }
        """);
    run();

    SourceLocation inner = resolve("Inner");
    SourceLocation primary = resolve("Primary");

    assertEquals("app.View$Inner", inner.getDeclaringClass());
    assertEquals("new Button(\"Inner\")", read(file, inner));
    assertEquals("app.View", primary.getDeclaringClass());
    assertEquals("Kind.PRIMARY.create()", read(file, primary));
  }

  @Test
  @DisplayName("should resolve the components below a component that the same file creates")
  void shouldResolveTree() {
    fixture.addSource("app.Card", """
        package app;

        import com.webforj.component.Composite;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class Card extends Composite<FlexLayout> {
          public Card() {
            getBoundComponent().add(new Button("Card"));
          }
        }
        """);
    fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            FlexLayout row = new FlexLayout();
            Button save = new Button("Save");
            row.add(save, new Button("Cancel"), new Card());
            layout.add(row);
          }
        }
        """);
    FlexLayout layout = run();
    Component row = layout.getComponents().get(0);

    List<SourceLocation> locations = fixture.createSiteResolver().resolveTree(row);

    assertEquals(List.of("row", "save", "Button at 10", "Card at 10"),
        locations.stream()
            .map(location -> location.getVariableName() != null ? location.getVariableName()
                : location.getSimpleTypeName() + " at " + location.getLine())
            .toList());
  }

  @Test
  @DisplayName("should follow a lambda and an anonymous class to the call that asked for them")
  void shouldFollowHiddenBodies() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import java.util.function.Supplier;

        public class View {
          public View(FlexLayout layout) {
            Supplier<Button> lambda = () -> new Button("Lambda");
            Supplier<Button> anonymous = new Supplier<>() {
              @Override
              public Button get() {
                return new Button("Anonymous");
              }
            };
            layout.add(lambda.get(), anonymous.get());
          }
        }
        """);
    run();

    assertEquals("lambda.get()", read(file, resolve("Lambda")));
    assertEquals("anonymous.get()", read(file, resolve("Anonymous")));
  }

  @Test
  @DisplayName("should find a component a lambda or a class without a name creates for itself")
  void shouldFindInHiddenBodies() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Runnable lambda = () -> layout.add(new Button("Lambda"));
            Runnable anonymous = new Runnable() {
              @Override
              public void run() {
                layout.add(new Button("Anonymous"));
              }
            };
            class Local {
              Local() {
                layout.add(new Button("Local"));
              }
            }
            lambda.run();
            anonymous.run();
            new Local();
          }
        }
        """);
    run();

    assertEquals("new Button(\"Lambda\")", read(file, resolve("Lambda")));
    assertEquals("new Button(\"Anonymous\")", read(file, resolve("Anonymous")));
    assertEquals("new Button(\"Local\")", read(file, resolve("Local")));
  }

  @Test
  @DisplayName("should refuse a component that was created without the application recording it")
  void shouldRefuseUnrecordedComponent() {
    fixture.close();
    Button button = new Button("Unknown");
    fixture = new RuntimeSourceFixture(directory);

    assertEquals("The source of this Button was not recorded",
        assertThrows(SourceModificationException.class,
            () -> fixture.createSiteResolver().resolve(button)).getMessage());
  }

  private FlexLayout run() {
    fixture.compile();
    FlexLayout layout = fixture.show(new FlexLayout());
    fixture.create("app.View", layout);

    return layout;
  }

  private SourceLocation resolve(String text) {
    return fixture.createSiteResolver()
        .resolve(fixture.find(Button.class, button -> text.equals(button.getText())));
  }

  private static String read(Path file, SourceLocation location) throws IOException {
    CompilationUnit cu = new SourceParserService().parse(Files.readString(file)).orElseThrow();

    return SourceSites.find(cu, location.getSite()).toString();
  }
}
