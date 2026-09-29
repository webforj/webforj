package com.webforj.devtools.craftforj.source.structure;

import static com.webforj.devtools.craftforj.source.structure.StructureFixture.BUTTON;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.FLEX;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.SPLITTER;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.TEXT_FIELD;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.TOOLBAR;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.line;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.point;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.variable;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.structure.model.AttachPoint;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Named;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

@DisplayName("StructureModifier remove")
class StructureModifierRemoveTest {

  private static final String CREATED_MANY_TIMES =
      "Button entry is created inside a loop or a lambda, one line builds several components";

  private final StructureModifier editor = StructureFixture.createEditor();
  private StructureFixture fixture;

  @TempDir
  Path tempDir;

  @BeforeEach
  void setUp() {
    fixture = new StructureFixture(tempDir);
  }

  @Test
  @DisplayName("removes the declaration, the calls, the listener and the attach call")
  void shouldRemoveWholeSlice() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.setSpacing("1em");
            Button save = new Button("Save");
            save.setTheme(ButtonTheme.PRIMARY);
            save.onClick(e -> {
              save.setEnabled(false);
            });
            layout.add(save);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.setSpacing("1em");
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("takes the component out of a call that lists other children")
  void shouldDetachFromSharedCall() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            Button help = new Button("Help");
            layout.add(save, cancel, help);
          }
        }
        """);

    editor.remove(List.of(variable(file, "cancel", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            Button help = new Button("Help");
            layout.add(save, help);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("keeps the import of a type the file still uses")
  void shouldKeepImportStillInUse() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            layout.add(save);
            Button cancel = new Button("Cancel");
            layout.add(cancel);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button cancel = new Button("Cancel");
            layout.add(cancel);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("keeps the import of a type a comment still names")
  void shouldKeepImportNamedInComment() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        /**
         * Shows a {@link Button}.
         */
        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            layout.add(save);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        /**
         * Shows a {@link Button}.
         */
        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("removes a field with an initializer and its calls")
  void shouldRemoveFieldWithInitializer() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();
          private final Button save = new Button("Save");
          private String title;

          public View() {
            save.setEnabled(false);
            layout.add(save);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();
          private String title;

          public View() {
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("removes a field together with the assignment that creates it")
  void shouldRemoveFieldAssignedInCode() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private FlexLayout layout;
          private Button save;

          public View() {
            layout = new FlexLayout();
            save = new Button("Save");
            this.save.setEnabled(false);
            layout.add(this.save);
            layout.setSpacing("1em");
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private FlexLayout layout;

          public View() {
            layout = new FlexLayout();
            layout.setSpacing("1em");
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("takes an inline creation out of a call that lists other children")
  void shouldRemoveInlineArgument() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.field.TextField;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.add(new Button("Save"),
                new TextField("Name"));
          }
        }
        """);

    editor.remove(List.of(line(file, 11, TEXT_FIELD)), point(variable(file, "layout", FLEX), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.add(new Button("Save"));
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("removes the whole call when the inline creation was its only child")
  void shouldRemoveInlineStatement() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.add(new Button("Save").setTheme(ButtonTheme.PRIMARY));
            layout.setSpacing("1em");
          }
        }
        """);

    editor.remove(List.of(line(file, 10, BUTTON)), point(variable(file, "layout", FLEX), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.setSpacing("1em");
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("removes a container together with the children listed below it")
  void shouldRemoveContainerWithChildren() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            FlexLayout row = new FlexLayout();
            Button save = new Button("Save");
            save.setEnabled(false);
            Button cancel = new Button("Cancel");
            row.add(save, cancel);
            layout.add(row);
            Button help = new Button("Help");
            layout.add(help);
          }
        }
        """);

    editor.remove(List.of(variable(file, "row", FLEX), variable(file, "save", BUTTON),
        variable(file, "cancel", BUTTON)), point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button help = new Button("Help");
            layout.add(help);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("skips the listed children another file creates")
  void shouldSkipChildrenOfOtherFiles() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            UserCard card = new UserCard();
            layout.add(card);
          }
        }
        """);

    editor.remove(
        List.of(variable(file, "card", "com.example.UserCard"),
            variable(file.resolveSibling("UserCard.java"), "avatar", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("refuses an inline creation the line alone cannot tell apart")
  void shouldRefuseSameLineInlineCreations() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.add(new Button("Save"), new Button("Cancel"));
          }
        }
        """);

    String original = Files.readString(file);

    List<SourceLocation> removed = List.of(line(file, 9, BUTTON));
    AttachPoint place = point(variable(file, "layout", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.remove(removed, place, false));

    assertEquals("More than one Button is created at line 9, the line alone cannot tell which",
        refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("detaches a component the listener of another component still reads")
  void shouldKeepComponentReadInOtherListener() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            cancel.onClick(e -> save.setEnabled(true));
            layout.add(save, cancel);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            cancel.onClick(e -> save.setEnabled(true));
            layout.add(cancel);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("detaches a field a getter hands out")
  void shouldKeepReturnedComponent() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();
          private final Button save = new Button("Save");

          public View() {
            layout.add(save);
          }

          public Button getSave() {
            return save;
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();
          private final Button save = new Button("Save");

          public View() {
          }

          public Button getSave() {
            return save;
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("detaches a component handed to something that is no component")
  void shouldKeepComponentPassedElsewhere() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            layout.add(save);
            new SavePresenter(save);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            new SavePresenter(save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("detaches a component whose state another statement reads")
  void shouldKeepComponentReadByStatement() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            layout.add(save);
            String label = save.getText();
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            String label = save.getText();
          }
        }
        """, Files.readString(file));
  }

  @ParameterizedTest
  @MethodSource("refusedComponents")
  void shouldRefuseComponent(String source, String name, String message) throws IOException {
    Path file = fixture.write("View.java", source);

    String original = Files.readString(file);

    List<SourceLocation> removed = List.of(variable(file, name, BUTTON));
    AttachPoint place = point(variable(file, "layout", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.remove(removed, place, false));

    assertEquals(message, refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("removes the calls that set the place of the component in its parent")
  void shouldRemoveItemCalls() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            layout.add(save, cancel);
            layout.setItemGrow(1.0, save);
            layout.setItemGrow(2.0, save, cancel);
            layout.setItemBasis("10em", cancel);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button cancel = new Button("Cancel");
            layout.add(cancel);
            layout.setItemGrow(2.0, cancel);
            layout.setItemBasis("10em", cancel);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("takes one attach call out of a fluent chain and keeps the rest")
  void shouldRemoveFromFluentChain() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.toolbar.Toolbar;

        public class View {
          public View() {
            Toolbar toolbar = new Toolbar();
            Button menu = new Button("Menu");
            Button help = new Button("Help");
            toolbar.addToStart(menu).addToEnd(help);
          }
        }
        """);

    editor.remove(List.of(variable(file, "menu", BUTTON)),
        point(variable(file, "toolbar", TOOLBAR), "addToStart"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.toolbar.Toolbar;

        public class View {
          public View() {
            Toolbar toolbar = new Toolbar();
            Button help = new Button("Help");
            toolbar.addToEnd(help);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("takes the last attach call out of a fluent chain")
  void shouldRemoveEndOfFluentChain() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.toolbar.Toolbar;

        public class View {
          public View() {
            Toolbar toolbar = new Toolbar();
            Button menu = new Button("Menu");
            Button help = new Button("Help");
            toolbar.addToStart(menu).addToEnd(help);
          }
        }
        """);

    editor.remove(List.of(variable(file, "help", BUTTON)),
        point(variable(file, "toolbar", TOOLBAR), "addToEnd"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.toolbar.Toolbar;

        public class View {
          public View() {
            Toolbar toolbar = new Toolbar();
            Button menu = new Button("Menu");
            toolbar.addToStart(menu);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("takes an attach call out of the chain that creates the parent")
  void shouldRemoveFromChainOnCreation() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.toolbar.Toolbar;

        public class View {
          public View() {
            Button menu = new Button("Menu");
            Toolbar toolbar = new Toolbar().addToStart(menu);
          }
        }
        """);

    editor.remove(List.of(variable(file, "menu", BUTTON)),
        point(variable(file, "toolbar", TOOLBAR), "addToStart"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.layout.toolbar.Toolbar;

        public class View {
          public View() {
            Toolbar toolbar = new Toolbar();
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("takes the component out of the creation that lists the children")
  void shouldRemoveFromConstructorChildren() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            FlexLayout layout = new FlexLayout(save, cancel);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            Button cancel = new Button("Cancel");
            FlexLayout layout = new FlexLayout(cancel);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("leaves an empty creation when the only child is removed")
  void shouldRemoveOnlyConstructorChild() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            Button save = new Button("Save");
            FlexLayout layout = FlexLayout.create(save).vertical().build();
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = FlexLayout.create().vertical().build();
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("refuses to take a component out of a creation with fixed arguments")
  void shouldRefuseFixedCreationArgument() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;

        public class View {
          public View() {
            Button save = new Button("Save");
            CardPanel panel = new CardPanel(save);
          }
        }
        """);

    String original = Files.readString(file);

    List<SourceLocation> removed = List.of(variable(file, "save", BUTTON));
    AttachPoint place = point(variable(file, "panel", "com.example.CardPanel"), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.remove(removed, place, false));

    assertEquals(
        "The creation of CardPanel panel at line 8 takes a fixed set of arguments, the component"
            + " cannot be taken out of it",
        refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("removes a child of the bound component of a composite")
  void shouldRemoveFromBoundComponent() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.Composite;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends Composite<FlexLayout> {
          public View() {
            Button save = new Button("Save");
            getBoundComponent().add(save);
            getBoundComponent().setItemGrow(1.0, save);
            getBoundComponent().setSpacing("1em");
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)), point(line(file, 7, FLEX), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.Composite;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends Composite<FlexLayout> {
          public View() {
            getBoundComponent().setSpacing("1em");
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("removes a child attached with an unscoped call")
  void shouldRemoveFromTheClassItself() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Div;

        public class View extends Div {
          public View() {
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            add(save);
            this.add(cancel);
          }
        }
        """);

    editor.remove(List.of(variable(file, "cancel", BUTTON)),
        point(line(file, 6, "com.example.View"), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Div;

        public class View extends Div {
          public View() {
            Button save = new Button("Save");
            add(save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("detaches a component handed to a method of the view itself")
  void shouldKeepComponentHandedToOwnMethod() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Div;

        public class View extends Div {
          public View() {
            Button save = new Button("Save");
            add(save);
            configure(save);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(line(file, 6, "com.example.View"), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Div;

        public class View extends Div {
          public View() {
            Button save = new Button("Save");
            configure(save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("removes a component a branch creates and leaves the branch")
  void shouldRemoveFromBranch() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(boolean admin) {
            FlexLayout layout = new FlexLayout();
            if (admin) {
              Button delete = new Button("Delete");
              layout.add(delete);
            }
          }
        }
        """);

    editor.remove(List.of(variable(file, "delete", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(boolean admin) {
            FlexLayout layout = new FlexLayout();
            if (admin) {
            }
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("leaves an empty body where an attach call was the whole body of a branch")
  void shouldRemoveAttachWithoutBraces() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(boolean admin) {
            FlexLayout layout = new FlexLayout();
            Button delete = new Button("Delete");
            if (admin)
              layout.add(delete);
          }
        }
        """);

    editor.remove(List.of(variable(file, "delete", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(boolean admin) {
            FlexLayout layout = new FlexLayout();
            if (admin) {
            }
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("removes one variable out of a declaration that declares several")
  void shouldRemoveOneOfSharedDeclaration() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save"), cancel = new Button("Cancel");
            layout.add(save, cancel);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button cancel = new Button("Cancel");
            layout.add(cancel);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("leaves a variable of the same name in another method alone")
  void shouldLeaveSameNameInOtherMethod() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();

          public View() {
            Button save = new Button("Save");
            layout.add(save);
          }

          private void rebuild() {
            Button save = new Button("Save again");
            save.setEnabled(false);
          }
        }
        """);

    editor.remove(List.of(new SourceLocation(file.toString(), 10, null, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();

          public View() {
          }

          private void rebuild() {
            Button save = new Button("Save again");
            save.setEnabled(false);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("leaves a local that shadows the removed field alone")
  void shouldLeaveLocalShadowingField() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();
          private final Button save = new Button("Save");

          public View() {
            layout.add(save);
          }

          private void rebuild() {
            Button save = new Button("Other");
            save.setEnabled(false);
          }
        }
        """);

    editor.remove(List.of(new SourceLocation(file.toString(), 8, null, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();

          public View() {
          }

          private void rebuild() {
            Button save = new Button("Other");
            save.setEnabled(false);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("removes a component declared with var")
  void shouldRemoveVarDeclaration() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            var layout = new FlexLayout();
            var save = new Button("Save");
            layout.add(save);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            var layout = new FlexLayout();
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("removes a component a helper method creates and leaves the helper")
  void shouldRemoveComponentFromFactoryMethod() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = createButton("Save");
            layout.add(save);
          }

          private Button createButton(String text) {
            return new Button(text);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
          }

          private Button createButton(String text) {
            return new Button(text);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("removes a component nothing attaches")
  void shouldRemoveUnattachedComponent() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            save.setEnabled(false);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("refuses a view the router places")
  void shouldRefuseRoutedView() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.Composite;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.router.annotation.Route;

        @Route("/")
        public class View extends Composite<FlexLayout> {
          public View() {
            getBoundComponent().setSpacing("1em");
          }
        }
        """);

    String original = Files.readString(file);

    List<SourceLocation> removed = List.of(line(file, 8, "com.example.View"));
    AttachPoint place = point(line(file, 8, "com.example.MainLayout"), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.remove(removed, place, false));

    assertEquals("View is placed by the router, its @Route decides where it renders",
        refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("refuses an inline component the line does not create")
  void shouldRefuseUnknownInlineComponent() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
          }
        }
        """);

    String original = Files.readString(file);

    List<SourceLocation> removed = List.of(line(file, 8, BUTTON));
    AttachPoint place = point(variable(file, "layout", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.remove(removed, place, false));

    assertEquals("No creation of Button at line 8 found in View.java", refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("takes the component out of the creation a field parent is assigned")
  void shouldRemoveFromCreationAssignedLater() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private FlexLayout layout;

          public View() {
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            layout = new FlexLayout(save, cancel);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private FlexLayout layout;

          public View() {
            Button cancel = new Button("Cancel");
            layout = new FlexLayout(cancel);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("removes the whole call of a method that takes one component")
  void shouldRemoveWholeCallOfSingleComponentMethod() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.splitter.Splitter;

        public class View {
          public View() {
            Splitter splitter = new Splitter();
            Button master = new Button("Master");
            Button detail = new Button("Detail");
            splitter.setMaster(master);
            splitter.setDetail(detail);
          }
        }
        """);

    editor.remove(List.of(variable(file, "master", BUTTON)),
        point(variable(file, "splitter", SPLITTER), "setMaster"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.splitter.Splitter;

        public class View {
          public View() {
            Splitter splitter = new Splitter();
            Button detail = new Button("Detail");
            splitter.setDetail(detail);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("removes a component a static factory creates inline")
  void shouldRemoveInlineFactory() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.icons.TablerIcon;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.add(TablerIcon.create("home"));
            layout.setSpacing("1em");
          }
        }
        """);

    editor.remove(List.of(line(file, 9, "com.webforj.component.icons.Icon")),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.setSpacing("1em");
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("removes a field declared by a supertype and created by an assignment")
  void shouldRemoveFieldDeclaredAsSupertype() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.Component;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();
          private Component save;

          public View() {
            save = new Button("Save");
            layout.add(save);
          }

          private void rename(String save) {
            layout.setSpacing(save);
          }
        }
        """);

    editor.remove(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();

          public View() {
          }

          private void rename(String save) {
            layout.setSpacing(save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("refuses a change the compiler does not take")
  void shouldRefuseChangeThatDoesNotCompile() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            final Button other;
            Button save = (other = new Button("Other"));
            layout.add(save);
            other.focus();
          }
        }
        """);

    String original = Files.readString(file);

    List<SourceLocation> removed = List.of(variable(file, "save", BUTTON));
    AttachPoint place = point(variable(file, "layout", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.remove(removed, place, false));

    assertEquals("The change leaves View.java with an error at line 10, variable other might not "
        + "have been initialized", refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("refuses an inline component that comes without a line")
  void shouldRefuseInlineComponentWithoutLine() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.add(new Button("Save"));
          }
        }
        """);

    String original = Files.readString(file);

    List<SourceLocation> removed =
        List.of(new SourceLocation(file.toString(), null, null, null, BUTTON));
    AttachPoint place = point(variable(file, "layout", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.remove(removed, place, false));

    assertEquals("No creation of Button at line null found in View.java", refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  private static Stream<Arguments> refusedComponents() {
    return Stream.of(
        Arguments.of(Named.of("refuses a component that is read and attached nowhere", """
            package com.example;

            import com.webforj.component.button.Button;
            import com.webforj.component.layout.flexlayout.FlexLayout;

            public class View {
              public View() {
                FlexLayout layout = new FlexLayout();
                Button save = new Button("Save");
                String label = save.getText();
              }
            }
            """), "save", "Button save is still used at line 10, String label = save.getText();"),
        Arguments.of(Named.of("refuses a component a loop creates several times", """
            package com.example;

            import com.webforj.component.button.Button;
            import com.webforj.component.layout.flexlayout.FlexLayout;

            public class View {
              public View(String[] names) {
                FlexLayout layout = new FlexLayout();
                for (String name : names) {
                  Button entry = new Button(name);
                  layout.add(entry);
                }
              }
            }
            """), "entry", CREATED_MANY_TIMES),
        Arguments.of(Named.of("refuses a component a lambda creates on every run", """
            package com.example;

            import com.webforj.component.button.Button;
            import com.webforj.component.layout.flexlayout.FlexLayout;

            public class View {
              public View() {
                FlexLayout layout = new FlexLayout();
                Button more = new Button("More");
                more.onClick(e -> {
                  Button entry = new Button("Entry");
                  layout.add(entry);
                });
                layout.add(more);
              }
            }
            """), "entry", CREATED_MANY_TIMES),
        Arguments.of(Named.of("refuses a component the file does not declare", """
            package com.example;

            import com.webforj.component.button.Button;
            import com.webforj.component.layout.flexlayout.FlexLayout;

            public class View {
              public View() {
                FlexLayout layout = new FlexLayout();
              }
            }
            """), "save", "No declaration of Button save found in View.java"));
  }
}
