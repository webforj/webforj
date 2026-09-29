package com.webforj.devtools.craftforj.source.structure;

import static com.webforj.devtools.craftforj.source.structure.StructureFixture.BUTTON;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.FLEX;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.TOOLBAR;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.after;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.before;
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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("StructureModifier move within a file")
class StructureModifierMoveTest {

  private final StructureModifier editor = StructureFixture.createEditor();
  private StructureFixture fixture;

  @TempDir
  Path tempDir;

  @BeforeEach
  void setUp() {
    fixture = new StructureFixture(tempDir);
  }

  @Test
  @DisplayName("reorders the arguments of one call, after a sibling")
  void shouldReorderArgumentsAfter() throws IOException {
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

    editor.move(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"),
        after(variable(file, "layout", FLEX), variable(file, "help", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            Button help = new Button("Help");
            layout.add(cancel, help, save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("reorders the arguments of one call, before a sibling")
  void shouldReorderArgumentsBefore() throws IOException {
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

    editor.move(List.of(variable(file, "help", BUTTON)),
        point(variable(file, "layout", FLEX), "add"),
        before(variable(file, "layout", FLEX), variable(file, "save", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            Button help = new Button("Help");
            layout.add(help, save, cancel);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("reorders the calls of a file that attaches one child per call")
  void shouldReorderOwnCalls() throws IOException {
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
            layout.add(save);
            layout.add(cancel);
            layout.add(help);
          }
        }
        """);

    editor.move(List.of(variable(file, "help", BUTTON)),
        point(variable(file, "layout", FLEX), "add"),
        before(variable(file, "layout", FLEX), variable(file, "cancel", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            Button help = new Button("Help");
            layout.add(save);
            layout.add(help);
            layout.add(cancel);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("moves the attach to another parent and leaves the declaration")
  void shouldMoveToOtherParent() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout header = new FlexLayout();
            FlexLayout footer = new FlexLayout();
            Button save = new Button("Save");
            save.setEnabled(false);
            Button help = new Button("Help");
            header.add(save);
            footer.add(help);
          }
        }
        """);

    editor.move(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "header", FLEX), "add"),
        after(variable(file, "footer", FLEX), variable(file, "help", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout header = new FlexLayout();
            FlexLayout footer = new FlexLayout();
            Button save = new Button("Save");
            save.setEnabled(false);
            Button help = new Button("Help");
            footer.add(help, save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("moves the declaration up when the new place comes before it")
  void shouldMoveDeclarationUp() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout header = new FlexLayout();
            Button help = new Button("Help");
            header.add(help);
            FlexLayout footer = new FlexLayout();
            Button save = new Button("Save");
            save.setEnabled(false);
            footer.add(save);
          }
        }
        """);

    editor.move(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "footer", FLEX), "add"),
        before(variable(file, "header", FLEX), variable(file, "help", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout header = new FlexLayout();
            Button help = new Button("Help");
            Button save = new Button("Save");
            save.setEnabled(false);
            header.add(save, help);
            FlexLayout footer = new FlexLayout();
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("refuses to move a declaration above a local it reads")
  void shouldRefuseMoveAboveDependency() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout header = new FlexLayout();
            Button help = new Button("Help");
            header.add(help);
            FlexLayout footer = new FlexLayout();
            String label = "Save";
            Button save = new Button(label);
            footer.add(save);
          }
        }
        """);

    String original = Files.readString(file);

    List<SourceLocation> moved = List.of(variable(file, "save", BUTTON));
    AttachPoint from = point(variable(file, "footer", FLEX), "add");
    AttachPoint to = before(variable(file, "header", FLEX), variable(file, "help", BUTTON), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("The moved code reads label, which stays in View.java", refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("moves a component to another slot of the same parent")
  void shouldMoveToOtherSlot() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.toolbar.Toolbar;

        public class View {
          public View() {
            Toolbar toolbar = new Toolbar();
            Button menu = new Button("Menu");
            Button help = new Button("Help");
            toolbar.addToStart(menu, help);
          }
        }
        """);

    editor.move(List.of(variable(file, "help", BUTTON)),
        point(variable(file, "toolbar", TOOLBAR), "addToStart"),
        point(variable(file, "toolbar", TOOLBAR), "addToEnd"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.toolbar.Toolbar;

        public class View {
          public View() {
            Toolbar toolbar = new Toolbar();
            Button menu = new Button("Menu");
            Button help = new Button("Help");
            toolbar.addToStart(menu);
            toolbar.addToEnd(help);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("drops the calls that placed the component in the parent it leaves")
  void shouldDropItemCallsOfOldParent() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout header = new FlexLayout();
            FlexLayout footer = new FlexLayout();
            Button save = new Button("Save");
            header.add(save);
            header.setItemGrow(1.0, save);
            footer.setSpacing("1em");
          }
        }
        """);

    editor.move(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "header", FLEX), "add"), point(variable(file, "footer", FLEX), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout header = new FlexLayout();
            FlexLayout footer = new FlexLayout();
            Button save = new Button("Save");
            footer.setSpacing("1em");
            footer.add(save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("keeps the item calls of the same parent behind the new attach call")
  void shouldKeepItemCallsAfterNewAttach() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            layout.add(save);
            layout.setItemGrow(1.0, save);
            layout.add(cancel);
          }
        }
        """);

    editor.move(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"),
        after(variable(file, "layout", FLEX), variable(file, "cancel", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            layout.add(cancel, save);
            layout.setItemGrow(1.0, save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("moves a component created inline as it is")
  void shouldMoveInlineComponent() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout header = new FlexLayout();
            FlexLayout footer = new FlexLayout();
            header.add(new Button("Save").setTheme(ButtonTheme.PRIMARY));
            footer.setSpacing("1em");
          }
        }
        """);

    editor.move(List.of(line(file, 11, BUTTON)), point(variable(file, "header", FLEX), "add"),
        point(variable(file, "footer", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout header = new FlexLayout();
            FlexLayout footer = new FlexLayout();
            footer.setSpacing("1em");
            footer.add(new Button("Save").setTheme(ButtonTheme.PRIMARY));
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("moves the attach of a field to a parent another method fills")
  void shouldMoveFieldAcrossMethods() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout header = new FlexLayout();
          private final FlexLayout footer = new FlexLayout();
          private final Button save = new Button("Save");

          public View() {
            header.add(save);
            buildFooter();
          }

          private void buildFooter() {
            footer.setSpacing("1em");
          }
        }
        """);

    editor.move(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "header", FLEX), "add"), point(variable(file, "footer", FLEX), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout header = new FlexLayout();
          private final FlexLayout footer = new FlexLayout();
          private final Button save = new Button("Save");

          public View() {
            buildFooter();
          }

          private void buildFooter() {
            footer.setSpacing("1em");
            footer.add(save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("carries a local and its calls into the method of the new parent")
  void shouldMoveLocalToOtherMethod() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout header = new FlexLayout();
          private final FlexLayout footer = new FlexLayout();

          public View() {
            Button save = new Button("Save");
            save.setEnabled(false);
            header.add(save);
            save.focus();
            buildFooter();
          }

          private void buildFooter() {
            Button help = new Button("Help");
            footer.add(help);
          }
        }
        """);

    editor.move(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "header", FLEX), "add"),
        before(variable(file, "footer", FLEX), variable(file, "help", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout header = new FlexLayout();
          private final FlexLayout footer = new FlexLayout();

          public View() {
            buildFooter();
          }

          private void buildFooter() {
            Button help = new Button("Help");
            Button save = new Button("Save");
            save.setEnabled(false);
            footer.add(save, help);
            save.focus();
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("renames a carried local when the new method already uses the name")
  void shouldRenameLocalOnCollision() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout header = new FlexLayout();
          private final FlexLayout footer = new FlexLayout();

          public View() {
            Button save = new Button("Save");
            save.setEnabled(false);
            header.add(save);
          }

          private void buildFooter(String save) {
            footer.setSpacing(save);
          }
        }
        """);

    editor.move(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "header", FLEX), "add"), point(variable(file, "footer", FLEX), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout header = new FlexLayout();
          private final FlexLayout footer = new FlexLayout();

          public View() {
          }

          private void buildFooter(String save) {
            footer.setSpacing(save);
            Button save2 = new Button("Save");
            save2.setEnabled(false);
            footer.add(save2);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("refuses to carry a local that reads a local of the method it leaves")
  void shouldRefuseLocalReadingOtherLocal() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout header = new FlexLayout();
          private final FlexLayout footer = new FlexLayout();

          public View(String label) {
            Button save = new Button(label);
            header.add(save);
          }

          private void buildFooter() {
            footer.setSpacing("1em");
          }
        }
        """);

    String original = Files.readString(file);

    List<SourceLocation> moved = List.of(variable(file, "save", BUTTON));
    AttachPoint from = point(variable(file, "header", FLEX), "add");
    AttachPoint to = point(variable(file, "footer", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("The moved code reads label, which stays in View.java", refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("carries a container and its children into another method")
  void shouldMoveContainerWithChildren() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout header = new FlexLayout();
          private final FlexLayout footer = new FlexLayout();

          public View() {
            FlexLayout row = new FlexLayout();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            row.add(save, cancel);
            header.add(row);
          }

          private void buildFooter() {
            footer.setSpacing("1em");
          }
        }
        """);

    editor.move(
        List.of(variable(file, "row", FLEX), variable(file, "save", BUTTON),
            variable(file, "cancel", BUTTON)),
        point(variable(file, "header", FLEX), "add"), point(variable(file, "footer", FLEX), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout header = new FlexLayout();
          private final FlexLayout footer = new FlexLayout();

          public View() {
          }

          private void buildFooter() {
            footer.setSpacing("1em");
            FlexLayout row = new FlexLayout();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            row.add(save, cancel);
            footer.add(row);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("moves a component into a container declared after it")
  void shouldMoveIntoChildContainer() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            FlexLayout row = new FlexLayout();
            layout.add(save, row);
          }
        }
        """);

    editor.move(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "layout", FLEX), "add"), point(variable(file, "row", FLEX), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            FlexLayout row = new FlexLayout();
            row.add(save);
            layout.add(row);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("refuses to move a container into itself")
  void shouldRefuseMoveIntoItself() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            FlexLayout row = new FlexLayout();
            layout.add(row);
          }
        }
        """);

    String original = Files.readString(file);

    List<SourceLocation> moved = List.of(variable(file, "row", FLEX));
    AttachPoint from = point(variable(file, "layout", FLEX), "add");
    AttachPoint to = point(variable(file, "row", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("A component cannot be moved into itself", refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("refuses to move a container into a component below it")
  void shouldRefuseMoveIntoOwnChild() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            FlexLayout row = new FlexLayout();
            FlexLayout cell = new FlexLayout();
            row.add(cell);
            layout.add(row);
          }
        }
        """);

    String original = Files.readString(file);

    List<SourceLocation> moved = List.of(variable(file, "row", FLEX), variable(file, "cell", FLEX));
    AttachPoint from = point(variable(file, "layout", FLEX), "add");
    AttachPoint to = point(variable(file, "cell", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("A component cannot be moved into itself", refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("refuses to place a component next to itself")
  void shouldRefuseMoveNextToItself() throws IOException {
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

    String original = Files.readString(file);

    List<SourceLocation> moved = List.of(variable(file, "save", BUTTON));
    AttachPoint from = point(variable(file, "layout", FLEX), "add");
    AttachPoint to = after(variable(file, "layout", FLEX), variable(file, "save", BUTTON), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("A component cannot be placed next to itself", refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("refuses to move a component a branch attaches")
  void shouldRefuseMoveOfBranchedAttach() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(boolean wide) {
            FlexLayout layout = new FlexLayout();
            FlexLayout row = new FlexLayout();
            Button save = new Button("Save");
            if (wide) {
              layout.add(save);
            }
          }
        }
        """);

    String original = Files.readString(file);

    List<SourceLocation> moved = List.of(variable(file, "save", BUTTON));
    AttachPoint from = point(variable(file, "layout", FLEX), "add");
    AttachPoint to = point(variable(file, "row", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals(
        "The attach call of Button save at line 12 sits inside a branch, a loop or a lambda",
        refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("refuses to move a component its parent does not attach")
  void shouldRefuseMoveOfUnattachedComponent() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            FlexLayout row = new FlexLayout();
            Button save = new Button("Save");
          }
        }
        """);

    String original = Files.readString(file);

    List<SourceLocation> moved = List.of(variable(file, "save", BUTTON));
    AttachPoint from = point(variable(file, "layout", FLEX), "add");
    AttachPoint to = point(variable(file, "row", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("Button save is not attached to FlexLayout layout with [add] in View.java",
        refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("moves a component out of the creation that lists the children")
  void shouldMoveOutOfConstructorChildren() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            FlexLayout header = new FlexLayout(save, cancel);
            FlexLayout footer = new FlexLayout();
          }
        }
        """);

    editor.move(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "header", FLEX), "add"), point(variable(file, "footer", FLEX), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            FlexLayout header = new FlexLayout(cancel);
            FlexLayout footer = new FlexLayout();
            footer.add(save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("carries a local up when what it reads is declared above the new place")
  void shouldCarryLocalReadingVisibleLocal() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            String label = "Save";
            FlexLayout header = new FlexLayout();
            Button help = new Button("Help");
            header.add(help);
            FlexLayout footer = new FlexLayout();
            Button save = new Button(label);
            footer.add(save);
          }
        }
        """);

    editor.move(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "footer", FLEX), "add"),
        before(variable(file, "header", FLEX), variable(file, "help", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            String label = "Save";
            FlexLayout header = new FlexLayout();
            Button help = new Button("Help");
            Button save = new Button(label);
            header.add(save, help);
            FlexLayout footer = new FlexLayout();
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("carries a local that reads a field and calls a method of its own class")
  void shouldCarryLocalUsingFieldAndMethodOfItsClass() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout header = new FlexLayout();
          private final FlexLayout footer = new FlexLayout();
          private final String label = "Save";

          public View() {
            Button save = new Button(label);
            save.onClick(e -> submit());
            save.onClick(this::track);
            header.add(save);
          }

          private void buildFooter() {
            footer.setSpacing("1em");
          }
        }
        """);

    editor.move(List.of(variable(file, "save", BUTTON)),
        point(variable(file, "header", FLEX), "add"), point(variable(file, "footer", FLEX), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout header = new FlexLayout();
          private final FlexLayout footer = new FlexLayout();
          private final String label = "Save";

          public View() {
          }

          private void buildFooter() {
            footer.setSpacing("1em");
            Button save = new Button(label);
            save.onClick(e -> submit());
            save.onClick(this::track);
            footer.add(save);
          }
        }
        """, Files.readString(file));
  }
}
