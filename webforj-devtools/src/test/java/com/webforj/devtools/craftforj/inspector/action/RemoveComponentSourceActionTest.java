package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.webforj.component.Component;
import com.webforj.component.button.Button;
import com.webforj.component.html.elements.Paragraph;
import com.webforj.component.icons.Icon;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.layout.toolbar.Toolbar;
import com.webforj.devtools.craftforj.inspector.action.RemoveComponentSourceAction.Response;
import com.webforj.devtools.craftforj.source.SourceFileEditor;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.source.structure.StructureModifier;
import com.webforj.devtools.craftforj.source.support.RuntimeSourceFixture;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("RemoveComponentSourceAction")
class RemoveComponentSourceActionTest {

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
  @DisplayName("should name the action")
  void shouldNameAction() {
    assertEquals("inspector.removeComponentSource", new RemoveComponentSourceAction().getAction());
  }

  @Test
  @DisplayName("should remove a local with its configuration, its listener and its attach call")
  void shouldRemoveLocalComponent() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.setSpacing("1em");
            Button save = new Button("Save");
            save.setTheme(ButtonTheme.PRIMARY);
            save.onClick(event -> {
              save.setEnabled(false);
            });
            Button cancel = new Button("Cancel");
            layout.add(save, cancel);
          }
        }
        """);
    run();

    Response response = remove(findButton("Save"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.setSpacing("1em");
            Button cancel = new Button("Cancel");
            layout.add(cancel);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should remove the second of two creations the compiler reports on one line")
  void shouldRemoveCreationReportedOnAnotherLine() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(
                new Button("First"),
                new Button("Second"),
                new Button("Third"));
          }
        }
        """);
    run();

    Response response = remove(findButton("Second"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(
                new Button("First"),
                new Button("Third"));
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should remove the first of two components one line creates")
  void shouldRemoveOneOfTwoOnOneLine() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.icons.TablerIcon;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(TablerIcon.create("plus"), TablerIcon.create("minus"));
          }
        }
        """);
    run();

    Response response = remove(findIcon("plus"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.icons.TablerIcon;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(TablerIcon.create("minus"));
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should remove the selected component after the lines of the file moved")
  void shouldRemoveAfterLinesMoved() throws IOException {
    String source = """
        package app;

        import com.webforj.component.icons.TablerIcon;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            // Toolbar icons
            layout.add(TablerIcon.create("plus"));
            layout.add(TablerIcon.create("minus"));
          }
        }
        """;
    Path file = fixture.addSource("app.View", source);
    run();
    Files.writeString(file, source.replace("    // Toolbar icons\n", ""));

    Response response = remove(findIcon("plus"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.icons.TablerIcon;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(TablerIcon.create("minus"));
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should refuse a method that creates another number of components than it ran")
  void shouldRefuseChangedMethod() throws IOException {
    String source = """
        package app;

        import com.webforj.component.icons.TablerIcon;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(TablerIcon.create("plus"));
            layout.add(TablerIcon.create("minus"));
          }
        }
        """;
    Path file = fixture.addSource("app.View", source);
    run();
    String changed = source.replace("    layout.add(TablerIcon.create(\"plus\"));\n",
        "    layout.add(TablerIcon.create(\"home\"));\n"
            + "    layout.add(TablerIcon.create(\"plus\"));\n");
    Files.writeString(file, changed);

    Response response = remove(findIcon("plus"));

    assertRefused(response, "The constructor of View was changed after the application was "
        + "compiled, save and reload it first");
    assertEquals(changed, Files.readString(file));
  }

  @Test
  @DisplayName("should refuse a class that was compiled again after the component was created")
  void shouldRefuseClassCompiledAgain() throws IOException {
    String source = """
        package app;

        import com.webforj.component.icons.TablerIcon;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(TablerIcon.create("plus"));
          }
        }
        """;
    Path file = fixture.addSource("app.View", source);
    run();
    String changed =
        source.replace("    layout.add(", "    layout.setSpacing(\"1em\");\n    layout.add(");
    fixture.addSource("app.View", changed);
    fixture.compile();

    Response response = remove(findIcon("plus"));

    assertRefused(response,
        "View was compiled again after this component was created, reload the application");
    assertEquals(changed, Files.readString(file));
  }

  @Test
  @DisplayName("should remove the call that asked a helper for the component and keep the helper")
  void shouldRemoveHelperCall() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(
                build("Search"),
                build("Notifications"));
            layout.add(build("Logout"));
          }

          private Button build(String text) {
            Button button = new Button(text);
            button.setEnabled(false);
            return button;
          }
        }
        """);
    run();

    Response response = remove(findButton("Notifications"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(
                build("Search"));
            layout.add(build("Logout"));
          }

          private Button build(String text) {
            Button button = new Button(text);
            button.setEnabled(false);
            return button;
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should follow a helper that hands on what another helper returned")
  void shouldRemoveCallOfHelperChain() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Button first = primary("First");
            layout.add(first, primary("Second"));
          }

          private static Button primary(String text) {
            return create(text).setEnabled(false);
          }

          private static Button create(String text) {
            return new Button(text);
          }
        }
        """);
    run();

    Response response = remove(findButton("First"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(primary("Second"));
          }

          private static Button primary(String text) {
            return create(text).setEnabled(false);
          }

          private static Button create(String text) {
            return new Button(text);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should refuse a part a helper builds for every component it returns")
  void shouldRefusePartOfSharedHelper() throws IOException {
    String source = """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.icons.TablerIcon;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(build("search"), build("bell"));
          }

          private Button build(String icon) {
            Button button = new Button(icon);
            button.setPrefixComponent(TablerIcon.create(icon));
            return button;
          }
        }
        """;
    Path file = fixture.addSource("app.View", source);
    run();
    fixture.show(findButton("search").getPrefixComponent());
    Icon icon = (Icon) findButton("bell").getPrefixComponent();

    Response response = remove(fixture.show(icon));

    assertRefused(response,
        "Icon is created by code that built 2 components, its source stands for all of them");
    assertEquals(source, Files.readString(file));
  }

  @Test
  @DisplayName("should remove the only component a loop created")
  void shouldRemoveOnlyComponentOfLoop() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import java.util.List;

        public class View {
          public View(FlexLayout layout) {
            for (String name : List.of("Only")) {
              layout.add(new Button(name));
            }
          }
        }
        """);
    run();

    Response response = remove(findButton("Only"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.layout.flexlayout.FlexLayout;
        import java.util.List;

        public class View {
          public View(FlexLayout layout) {
            for (String name : List.of("Only")) {
            }
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should refuse one of the components a loop created")
  void shouldRefuseOneOfLoop() throws IOException {
    String source = """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import java.util.List;

        public class View {
          public View(FlexLayout layout) {
            for (String name : List.of("First", "Second")) {
              layout.add(new Button(name));
            }
          }
        }
        """;
    Path file = fixture.addSource("app.View", source);
    run();

    Response response = remove(findButton("Second"));

    assertRefused(response,
        "Button is created by code that built 2 components, its source stands for all of them");
    assertEquals(source, Files.readString(file));
  }

  @Test
  @DisplayName("should remove a component a lambda declares and leave the lambda")
  void shouldRemoveLocalOfLambda() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Runnable first = () -> layout.add(new Button("First"));
            Runnable second = () -> {
              Button button = new Button("Second");
              button.setEnabled(false);
              layout.add(button);
              layout.setSpacing("1em");
            };
            first.run();
            second.run();
          }
        }
        """);
    run();

    Response response = remove(findButton("Second"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Runnable first = () -> layout.add(new Button("First"));
            Runnable second = () -> {
              layout.setSpacing("1em");
            };
            first.run();
            second.run();
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should leave an empty lambda where the component was all the lambda wrote")
  void shouldRemoveBodyOfLambda() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Runnable build = () -> layout.add(new Button("Only"));
            build.run();
          }
        }
        """);
    run();

    Response response = remove(findButton("Only"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Runnable build = () -> {
            };
            build.run();
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should refuse a component of two lambdas nothing tells apart")
  void shouldRefuseLambdasOfOneLine() throws IOException {
    String source = """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            run(() -> layout.add(new Button("First")), () -> layout.add(new Button("Second")));
          }

          private static void run(Runnable first, Runnable second) {
            first.run();
            second.run();
          }
        }
        """;
    Path file = fixture.addSource("app.View", source);
    run();

    Response response = remove(findButton("Second"));

    assertRefused(response,
        "View holds 2 places that fit the code that created this component, which one cannot be"
            + " told");
    assertEquals(source, Files.readString(file));
  }

  @Test
  @DisplayName("should remove the call that asked a lambda for the component")
  void shouldRemoveCallOfLambda() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import java.util.function.Supplier;

        public class View {
          public View(FlexLayout layout) {
            Supplier<Button> supplier = () -> new Button("Supplied");
            layout.add(new Button("Kept"), supplier.get());
          }
        }
        """);
    run();

    Response response = remove(findButton("Supplied"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import java.util.function.Supplier;

        public class View {
          public View(FlexLayout layout) {
            Supplier<Button> supplier = () -> new Button("Supplied");
            layout.add(new Button("Kept"));
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should remove a component handed to the constructor of the parent class")
  void shouldRemoveArgumentOfParentConstructor() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.html.elements.Div;
        import com.webforj.component.html.elements.Paragraph;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends Div {
          public View(FlexLayout layout) {
            super(new Paragraph("First"), new Paragraph("Second"));
            layout.add(this);
          }
        }
        """);
    run();

    Response response = remove(findParagraph("First"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.html.elements.Div;
        import com.webforj.component.html.elements.Paragraph;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends Div {
          public View(FlexLayout layout) {
            super(new Paragraph("Second"));
            layout.add(this);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should tell the body of a loop from its update, which runs after the body")
  void shouldRemoveCallOfLoopBody() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            int count = 0;
            for (Button kept = create("Init"); count < 1; kept = create("Update")) {
              layout.add(create("Body"));
              count++;
            }
          }

          private static Button create(String text) {
            return new Button(text);
          }
        }
        """);
    run();

    Response response = remove(findButton("Body"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            int count = 0;
            for (Button kept = create("Init"); count < 1; kept = create("Update")) {
              count++;
            }
          }

          private static Button create(String text) {
            return new Button(text);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should remove a component a class without a name creates")
  void shouldRemoveComponentOfAnonymousClass() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Runnable build = new Runnable() {
              @Override
              public void run() {
                layout.setSpacing("1em");
                layout.add(new Button("Inner"));
              }
            };
            build.run();
          }
        }
        """);
    run();

    Response response = remove(findButton("Inner"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Runnable build = new Runnable() {
              @Override
              public void run() {
                layout.setSpacing("1em");
              }
            };
            build.run();
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should remove the use of a project component and leave its class alone")
  void shouldRemoveUseOfProjectComponent() throws IOException {
    String card = """
        package app;

        import com.webforj.component.Composite;
        import com.webforj.component.html.elements.Paragraph;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class Card extends Composite<FlexLayout> {
          private final FlexLayout self = getBoundComponent();

          public Card(String text) {
            self.add(new Paragraph(text));
          }
        }
        """;
    Path cardFile = fixture.addSource("app.Card", card);
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(
                new Button("Keep"),
                new Card("Selected"));
          }
        }
        """);
    FlexLayout layout = run();
    Component selected = layout.getComponents().get(1);

    Response response = remove(selected);

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(
                new Button("Keep"));
          }
        }
        """, Files.readString(file));
    assertEquals(card, Files.readString(cardFile));
  }

  @Test
  @DisplayName("should refuse the component a project component is built on")
  void shouldRefuseRootOfProjectComponent() throws IOException {
    String card = """
        package app;

        import com.webforj.component.Composite;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class Card extends Composite<FlexLayout> {
          public Card() {
            getBoundComponent().setSpacing("1em");
          }
        }
        """;
    Path file = fixture.addSource("app.Card", card);
    fixture.compile();
    Component composite = fixture.create("app.Card");
    FlexLayout root = fixture.find(FlexLayout.class, layout -> layout != composite);

    Response response = remove(root);

    assertRefused(response, "FlexLayout is a part Card builds for itself, remove that instead");
    assertEquals(card, Files.readString(file));
  }

  @Test
  @DisplayName("should detach a component a listener of another component still reads")
  void shouldKeepReferencedComponent() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Paragraph;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Paragraph clicked = new Paragraph();
            clicked.setAttribute("data-case", "bean-on-click");
            Button ask = new Button("Ask");
            ask.onClick(event -> clicked.setText("Asked"));
            layout.add(ask, clicked);
          }
        }
        """);
    run();

    Response response = remove(fixture.find(Paragraph.class, paragraph -> true));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Paragraph;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Paragraph clicked = new Paragraph();
            clicked.setAttribute("data-case", "bean-on-click");
            Button ask = new Button("Ask");
            ask.onClick(event -> clicked.setText("Asked"));
            layout.add(ask);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should refuse a component that is handed on and attached nowhere")
  void shouldRefuseReferencedComponentWithoutAttachment() throws IOException {
    String source = """
        package app;

        import com.webforj.component.button.Button;
        import java.util.List;

        public class View {
          public View(List<Object> sink) {
            Button save = new Button("Save");
            sink.add(save);
          }
        }
        """;
    Path file = fixture.addSource("app.View", source);
    fixture.compile();
    List<Object> sink = new ArrayList<>();
    fixture.create("app.View", sink);

    Response response = remove(fixture.show((Button) sink.get(0)));

    assertRefused(response, "Button save is still used at line 9, sink.add(save);");
    assertEquals(source, Files.readString(file));
  }

  @Test
  @DisplayName("should detach a field other classes can reach and keep it declared")
  void shouldKeepReachableField() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          protected final Button save = new Button("Save");

          public View(FlexLayout layout) {
            save.setEnabled(false);
            layout.add(save);
          }
        }
        """);
    run();

    Response response = remove(findButton("Save"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          protected final Button save = new Button("Save");

          public View(FlexLayout layout) {
            save.setEnabled(false);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should keep a component a method of the project is handed")
  void shouldKeepComponentHandedToProjectMethod() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Div;

        public class View extends Div {
          public View() {
            Button save = new Button("Save");
            add(save);
            configure(save);
          }

          private void configure(Button button) {
            button.setEnabled(false);
          }
        }
        """);
    fixture.compile();
    fixture.create("app.View");

    Response response = remove(findButton("Save"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Div;

        public class View extends Div {
          public View() {
            Button save = new Button("Save");
            configure(save);
          }

          private void configure(Button button) {
            button.setEnabled(false);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should take a component out of the chain that sets it as a prefix")
  void shouldRemoveLinkOfChain() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.icons.TablerIcon;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Button create = new Button("Create")
                .setPrefixComponent(TablerIcon.create("plus"))
                .setTheme(ButtonTheme.PRIMARY);
            layout.add(create);
          }
        }
        """);
    run();
    Icon icon = (Icon) findButton("Create").getPrefixComponent();

    Response response = remove(fixture.show(icon));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Button create = new Button("Create")
                .setTheme(ButtonTheme.PRIMARY);
            layout.add(create);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should remove the statement that sets a declared component as a prefix")
  void shouldRemoveSlotStatement() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.icons.Icon;
        import com.webforj.component.icons.TablerIcon;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Button create = new Button("Create");
            Icon icon = TablerIcon.create("plus");
            icon.setStyle("font-size", "2em");
            create.setPrefixComponent(icon);
            layout.add(create);
          }
        }
        """);
    run();
    Icon icon = (Icon) findButton("Create").getPrefixComponent();

    Response response = remove(fixture.show(icon));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Button create = new Button("Create");
            layout.add(create);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should take a component out of a creation that has a constructor without it")
  void shouldRemoveConstructorArgument() throws IOException {
    Path file = fixture.addSource("app.View",
        """
            package app;

            import com.webforj.component.icons.TablerIcon;
            import com.webforj.component.layout.appnav.AppNav;
            import com.webforj.component.layout.appnav.AppNavItem;
            import com.webforj.component.layout.flexlayout.FlexLayout;

            public class View {
              public View(FlexLayout layout) {
                AppNav navigation = new AppNav();
                navigation.addItem(new AppNavItem("Dashboard", "/dashboard", TablerIcon.create("home")));
                navigation.addItem(new AppNavItem("Reports", "/reports", TablerIcon.create("chart-bar")));
                layout.add(navigation);
              }
            }
            """);
    run();

    Response response = remove(findIcon("home"));

    assertRemoved(response, file);
    assertEquals(
        """
            package app;

            import com.webforj.component.icons.TablerIcon;
            import com.webforj.component.layout.appnav.AppNav;
            import com.webforj.component.layout.appnav.AppNavItem;
            import com.webforj.component.layout.flexlayout.FlexLayout;

            public class View {
              public View(FlexLayout layout) {
                AppNav navigation = new AppNav();
                navigation.addItem(new AppNavItem("Dashboard", "/dashboard"));
                navigation.addItem(new AppNavItem("Reports", "/reports", TablerIcon.create("chart-bar")));
                layout.add(navigation);
              }
            }
            """,
        Files.readString(file));
  }

  @Test
  @DisplayName("should remove a creation with the component that is created inside of it")
  void shouldRemoveCreationWithItsArgument() throws IOException {
    Path file = fixture.addSource("app.View",
        """
            package app;

            import com.webforj.component.icons.TablerIcon;
            import com.webforj.component.layout.appnav.AppNav;
            import com.webforj.component.layout.appnav.AppNavItem;
            import com.webforj.component.layout.flexlayout.FlexLayout;

            public class View {
              public View(FlexLayout layout) {
                AppNav navigation = new AppNav();
                navigation.addItem(new AppNavItem("Dashboard", "/dashboard", TablerIcon.create("home")));
                navigation.addItem(new AppNavItem("Reports", "/reports", TablerIcon.create("chart-bar")));
                layout.add(navigation);
              }
            }
            """);
    run();
    Component dashboard = fixture.getComponents().stream()
        .filter(component -> component.getClass().getSimpleName().equals("AppNavItem")).toList()
        .get(0);

    Response response = remove(dashboard);

    assertRemoved(response, file);
    assertEquals(
        """
            package app;

            import com.webforj.component.icons.TablerIcon;
            import com.webforj.component.layout.appnav.AppNav;
            import com.webforj.component.layout.appnav.AppNavItem;
            import com.webforj.component.layout.flexlayout.FlexLayout;

            public class View {
              public View(FlexLayout layout) {
                AppNav navigation = new AppNav();
                navigation.addItem(new AppNavItem("Reports", "/reports", TablerIcon.create("chart-bar")));
                layout.add(navigation);
              }
            }
            """,
        Files.readString(file));
  }

  @Test
  @DisplayName("should remove one item of a navigation and keep the others")
  void shouldRemoveItemOfSingleComponentMethod() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.layout.appnav.AppNav;
        import com.webforj.component.layout.appnav.AppNavItem;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            AppNav navigation = new AppNav();
            navigation.addItem(new AppNavItem("Dashboard", "/dashboard"));
            navigation.addItem(new AppNavItem("Reports", "/reports"));
            layout.add(navigation);
          }
        }
        """);
    run();
    Component reports = fixture.getComponents().stream()
        .filter(component -> component.getClass().getSimpleName().equals("AppNavItem")).toList()
        .get(1);

    Response response = remove(reports);

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.layout.appnav.AppNav;
        import com.webforj.component.layout.appnav.AppNavItem;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            AppNav navigation = new AppNav();
            navigation.addItem(new AppNavItem("Dashboard", "/dashboard"));
            layout.add(navigation);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should remove a container with what it holds and keep a child a getter hands out")
  void shouldRemoveContainerWithChildren() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.H1;
        import com.webforj.component.icons.TablerIcon;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.layout.toolbar.Toolbar;

        public class View {
          private final H1 title = new H1();

          public View(FlexLayout layout) {
            Toolbar toolbar = new Toolbar();
            Button menu = new Button("Menu");
            menu.setEnabled(false);
            toolbar.addToStart(menu);
            toolbar.addToTitle(title);
            toolbar.addToEnd(TablerIcon.create("bell"));
            layout.add(toolbar);
            layout.setSpacing("1em");
          }

          public H1 getTitle() {
            return title;
          }
        }
        """);
    run();

    Response response = remove(fixture.find(Toolbar.class, toolbar -> true));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.html.elements.H1;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final H1 title = new H1();

          public View(FlexLayout layout) {
            layout.setSpacing("1em");
          }

          public H1 getTitle() {
            return title;
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should remove a field that the constructor assigns")
  void shouldRemoveAssignedField() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final Button first = new Button("First");
          private Button second;

          public View(FlexLayout layout) {
            second = new Button("Second");
            this.second.setEnabled(false);
            layout.add(first, this.second);
          }
        }
        """);
    run();

    Response response = remove(findButton("Second"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final Button first = new Button("First");

          public View(FlexLayout layout) {
            layout.add(first);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should remove a local of a case group and leave the field of the same name alone")
  void shouldRemoveLocalOfCaseGroup() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final Button target = new Button("Field");

          public View(FlexLayout layout, Integer mode) {
            layout.add(target);
            switch (mode) {
              case 1:
                Button target = new Button("Selected");
                target.setEnabled(false);
                layout.add(target);
                break;
              default:
                break;
            }
            target.setText("Field stays");
          }
        }
        """);
    fixture.compile();
    FlexLayout layout = fixture.show(new FlexLayout());
    fixture.create("app.View", layout, 1);

    Response response = remove(findButton("Selected"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final Button target = new Button("Field");

          public View(FlexLayout layout, Integer mode) {
            layout.add(target);
            switch (mode) {
              case 1:
                break;
              default:
                break;
            }
            target.setText("Field stays");
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should leave a name alone that a class in between inherits")
  void shouldLeaveInheritedNameAlone() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final Button target = new Button("Selected");

          public View(FlexLayout layout) {
            layout.add(target);
          }

          class Inner extends Base {
            void attach(FlexLayout layout) {
              layout.add(target);
            }
          }
        }

        class Base {
          protected Button target = new Button("Inherited");
        }
        """);
    run();

    Response response = remove(findButton("Selected"));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
          }

          class Inner extends Base {
            void attach(FlexLayout layout) {
              layout.add(target);
            }
          }
        }

        class Base {
          protected Button target = new Button("Inherited");
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should refuse a component that is gone and had no variable")
  void shouldRefuseStoredLocationWithoutVariable() throws IOException {
    String source = """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            layout.add(new Button("Only"));
          }
        }
        """;
    Path file = fixture.addSource("app.View", source);
    run();

    Response response = removeStored(
        new SourceLocation(file.toString(), 8, "app.View", null, Button.class.getName()));

    assertRefused(response,
        "This component is no longer part of the running application, reload it first");
    assertEquals(source, Files.readString(file));
  }

  @Test
  @DisplayName("should remove a component that is gone by the variable it was stored with")
  void shouldRemoveStoredVariable() throws IOException {
    Path file = fixture.addSource("app.View", """
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Button first = new Button("First");
            Button second = new Button("Second");
            first.setEnabled(false);
            layout.add(first, second);
          }
        }
        """);
    run();

    Response response = removeStored(
        new SourceLocation(file.toString(), 3, "app.View", "first", Button.class.getName()));

    assertRemoved(response, file);
    assertEquals("""
        package app;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(FlexLayout layout) {
            Button second = new Button("Second");
            layout.add(second);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("should refuse a stored location of a file the server never handed out")
  void shouldRefuseUnknownFile() throws IOException {
    Path other = directory.resolve("Other.java");
    Files.writeString(other, "class Other {}\n");

    Response response = removeStored(
        new SourceLocation(other.toString(), 1, "Other", "first", Button.class.getName()));

    assertRefused(response, "The source of this component was not found");
    assertEquals("class Other {}\n", Files.readString(other));
  }

  @Test
  @DisplayName("should refuse a stored location that names no type")
  void shouldRefuseStoredLocationWithoutType() throws IOException {
    String source = """
        package app;

        import com.webforj.component.button.Button;

        public class View {
          private final Button first = new Button("First");
        }
        """;
    Path file = fixture.addSource("app.View", source);

    Response response =
        removeStored(new SourceLocation(file.toString(), 6, "app.View", "first", null));

    assertRefused(response, "The type of this component is unknown");
    assertEquals(source, Files.readString(file));
  }

  @Test
  @DisplayName("should say when the editor changed nothing and when it could not read the file")
  void shouldReportWhatTheEditorAnswers() throws IOException {
    Path file = fixture.addSource("app.View", "package app;\n\nclass View {}\n");
    StructureModifier editor = mock(StructureModifier.class);
    RemoveComponentSourceAction action =
        new RemoveComponentSourceAction(editor, fixture.createSiteResolver(), id -> null);
    JsonObject request = new JsonObject();
    request.add("source", new Gson().toJsonTree(
        new SourceLocation(file.toString(), 3, "app.View", "first", Button.class.getName())));

    when(editor.remove(any(), any(), anyBoolean())).thenReturn(List.of());
    Response unchanged = action.handle(request);
    when(editor.remove(any(), any(), anyBoolean()))
        .thenThrow(new IOException("Failed to read source file: " + file));
    Response unreadable = action.handle(request);

    assertRefused(unchanged, "Nothing was removed");
    assertEquals(file.toString(), unchanged.getFile());
    assertRefused(unreadable, "Failed to read source file: " + file);
    assertNull(unreadable.getFile());
  }

  private FlexLayout run() {
    fixture.compile();
    FlexLayout layout = fixture.show(new FlexLayout());
    fixture.create("app.View", layout);

    return layout;
  }

  private Button findButton(String text) {
    return fixture.find(Button.class, button -> text.equals(button.getText()));
  }

  private Paragraph findParagraph(String text) {
    return fixture.find(Paragraph.class, paragraph -> text.equals(paragraph.getText()));
  }

  private Icon findIcon(String name) {
    return fixture.find(Icon.class, icon -> name.equals(icon.getName()));
  }

  private Response remove(Component component) {
    SourceParserService parser = new SourceParserService();
    RemoveComponentSourceAction action =
        new RemoveComponentSourceAction(new StructureModifier(new SourceFileEditor(parser), parser),
            fixture.createSiteResolver(), id -> fixture.getComponents().stream()
                .filter(known -> id.equals(known.getComponentId())).findFirst().orElse(null));
    JsonObject request = new JsonObject();
    request.addProperty("componentId", component.getComponentId());
    request.addProperty("componentType", component.getClass().getName());

    return action.handle(request);
  }

  private Response removeStored(SourceLocation location) {
    SourceParserService parser = new SourceParserService();
    RemoveComponentSourceAction action =
        new RemoveComponentSourceAction(new StructureModifier(new SourceFileEditor(parser), parser),
            fixture.createSiteResolver(), id -> null);
    JsonObject request = new JsonObject();
    request.addProperty("componentId", "gone");
    request.add("source", new Gson().toJsonTree(location));

    return action.handle(request);
  }

  private static void assertRemoved(Response response, Path file) {
    assertTrue(response.isRemoved(), response.getMessage());
    assertEquals(file.toString(), response.getFile());
    assertNull(response.getMessage());
  }

  private static void assertRefused(Response response, String message) {
    assertFalse(response.isRemoved());
    assertEquals(message, response.getMessage());
  }
}
