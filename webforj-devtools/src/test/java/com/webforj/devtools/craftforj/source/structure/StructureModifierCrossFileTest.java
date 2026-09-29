package com.webforj.devtools.craftforj.source.structure;

import static com.webforj.devtools.craftforj.source.structure.StructureFixture.BUTTON;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.DIV;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.FLEX;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.before;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.line;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.point;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.variable;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.staging.StagingException;
import com.webforj.devtools.craftforj.source.structure.model.AttachPoint;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("StructureModifier move between files")
class StructureModifierCrossFileTest {

  private static final String OTHER_BUTTON = "com.example.widgets.Button";

  private static final String SOURCE = """
      package com.example.layout;

      import com.webforj.component.button.Button;
      import com.webforj.component.layout.flexlayout.FlexLayout;

      public class MainLayout {
        public MainLayout() {
          FlexLayout header = new FlexLayout();
          Button save = new Button("Save");
          header.add(save);
        }
      }
      """;

  private static final String TARGET = """
      package com.example.views;

      import com.webforj.component.layout.flexlayout.FlexLayout;

      public class DashboardView {
        public DashboardView() {
          FlexLayout content = new FlexLayout();
        }
      }
      """;

  private final StructureModifier editor = StructureFixture.createEditor();
  private StructureFixture fixture;

  @TempDir
  Path tempDir;

  @BeforeEach
  void setUp() {
    fixture = new StructureFixture(tempDir);
  }

  @Test
  @DisplayName("cuts the slice out of one view and writes it into the other")
  void shouldMoveSliceToOtherFile() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button("Save");
            save.setTheme(ButtonTheme.PRIMARY);
            save.onClick(e -> {
              save.setEnabled(false);
            });
            header.add(save);
            header.setItemGrow(1.0, save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
          }
        }
        """);

    editor.move(List.of(variable(source, "save", BUTTON)),
        point(variable(source, "header", FLEX), "add"),
        point(variable(target, "content", FLEX), "add"), false);

    assertEquals("""
        package com.example.layout;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
          }
        }
        """, Files.readString(source));
    assertEquals("""
        package com.example.views;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
            Button save = new Button("Save");
            save.setTheme(ButtonTheme.PRIMARY);
            save.onClick(e -> {
              save.setEnabled(false);
            });
            content.add(save);
          }
        }
        """, Files.readString(target));
  }

  @Test
  @DisplayName("places the moved component before a sibling of the other file")
  void shouldPlaceNextToSiblingInOtherFile() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button("Save");
            Button help = new Button("Help");
            header.add(save, help);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            Button refresh = new Button("Refresh");
            content.add(refresh);
          }
        }
        """);

    editor.move(List.of(variable(source, "save", BUTTON)),
        point(variable(source, "header", FLEX), "add"),
        before(variable(target, "content", FLEX), variable(target, "refresh", BUTTON), "add"),
        false);

    assertEquals("""
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button help = new Button("Help");
            header.add(help);
          }
        }
        """, Files.readString(source));
    assertEquals("""
        package com.example.views;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            Button save = new Button("Save");
            Button refresh = new Button("Refresh");
            content.add(save, refresh);
          }
        }
        """, Files.readString(target));
  }

  @Test
  @DisplayName("renames the moved variable when the other class already uses the name")
  void shouldRenameOnCollisionInOtherFile() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button("Save");
            save.onClick(e -> save.setEnabled(false));
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            Button save = new Button("Save draft");
            content.add(save);
          }
        }
        """);

    editor.move(List.of(variable(source, "save", BUTTON)),
        point(variable(source, "header", FLEX), "add"),
        point(variable(target, "content", FLEX), "add"), false);

    assertEquals("""
        package com.example.layout;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
          }
        }
        """, Files.readString(source));
    assertEquals("""
        package com.example.views;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            Button save = new Button("Save draft");
            Button save2 = new Button("Save");
            save2.onClick(e -> save2.setEnabled(false));
            content.add(save, save2);
          }
        }
        """, Files.readString(target));
  }

  @Test
  @DisplayName("moves a field as a field of the other class")
  void shouldMoveFieldAsField() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          private final FlexLayout header = new FlexLayout();
          private final Button save = new Button("Save");

          public MainLayout() {
            save.setTheme(ButtonTheme.PRIMARY);
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          private final FlexLayout content = new FlexLayout();

          public DashboardView() {
            content.setSpacing("1em");
          }
        }
        """);

    editor.move(List.of(variable(source, "save", BUTTON)),
        point(variable(source, "header", FLEX), "add"),
        point(variable(target, "content", FLEX), "add"), false);

    assertEquals("""
        package com.example.layout;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          private final FlexLayout header = new FlexLayout();

          public MainLayout() {
          }
        }
        """, Files.readString(source));
    assertEquals("""
        package com.example.views;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          private final FlexLayout content = new FlexLayout();
          private final Button save = new Button("Save");

          public DashboardView() {
            content.setSpacing("1em");
            save.setTheme(ButtonTheme.PRIMARY);
            content.add(save);
          }
        }
        """, Files.readString(target));
  }

  @Test
  @DisplayName("moves a container with the children listed below it")
  void shouldMoveContainerWithChildrenToOtherFile() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            FlexLayout row = new FlexLayout();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            row.add(save, cancel);
            row.setItemGrow(1.0, save);
            header.add(row);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
          }
        }
        """);

    editor.move(
        List.of(variable(source, "row", FLEX), variable(source, "save", BUTTON),
            variable(source, "cancel", BUTTON)),
        point(variable(source, "header", FLEX), "add"),
        point(variable(target, "content", FLEX), "add"), false);

    assertEquals("""
        package com.example.layout;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
          }
        }
        """, Files.readString(source));
    assertEquals("""
        package com.example.views;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
            FlexLayout row = new FlexLayout();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            row.add(save, cancel);
            row.setItemGrow(1.0, save);
            content.add(row);
          }
        }
        """, Files.readString(target));
  }

  @Test
  @DisplayName("moves a component created inline into the other file")
  void shouldMoveInlineToOtherFile() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            header.add(new Button("Save").setTheme(ButtonTheme.PRIMARY));
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
          }
        }
        """);

    editor.move(List.of(line(source, 10, BUTTON)), point(variable(source, "header", FLEX), "add"),
        point(variable(target, "content", FLEX), "add"), false);

    assertEquals("""
        package com.example.layout;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
          }
        }
        """, Files.readString(source));
    assertEquals("""
        package com.example.views;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
            content.add(new Button("Save").setTheme(ButtonTheme.PRIMARY));
          }
        }
        """, Files.readString(target));
  }

  @Test
  @DisplayName("moves what a component created inline holds along with it")
  void shouldMoveInlineWithWhatItHolds() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Div;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button("Save");
            header.add(new Div(save));
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
          }
        }
        """);

    editor.move(List.of(line(source, 11, DIV), variable(source, "save", BUTTON)),
        point(variable(source, "header", FLEX), "add"),
        point(variable(target, "content", FLEX), "add"), false);

    assertEquals("""
        package com.example.layout;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
          }
        }
        """, Files.readString(source));
    assertEquals("""
        package com.example.views;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Div;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
            Button save = new Button("Save");
            content.add(new Div(save));
          }
        }
        """, Files.readString(target));
  }

  @Test
  @DisplayName("refuses a listener that calls a method of the view it leaves")
  void shouldRefuseCodeCallingItsView() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button("Save");
            save.onClick(e -> submit());
            header.add(save);
          }

          private void submit() {}
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
          }
        }
        """);

    String sourceBefore = Files.readString(source);
    String targetBefore = Files.readString(target);

    List<SourceLocation> moved = List.of(variable(source, "save", BUTTON));
    AttachPoint from = point(variable(source, "header", FLEX), "add");
    AttachPoint to = point(variable(target, "content", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("The moved code calls submit(), which stays in MainLayout.java",
        refused.getMessage());
    assertEquals(sourceBefore, Files.readString(source));
    assertEquals(targetBefore, Files.readString(target));
  }

  @Test
  @DisplayName("refuses code that reads a local of the view it leaves")
  void shouldRefuseCodeReadingLocal() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout(String label) {
            FlexLayout header = new FlexLayout();
            Button save = new Button(label);
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
          }
        }
        """);

    String sourceBefore = Files.readString(source);
    String targetBefore = Files.readString(target);

    List<SourceLocation> moved = List.of(variable(source, "save", BUTTON));
    AttachPoint from = point(variable(source, "header", FLEX), "add");
    AttachPoint to = point(variable(target, "content", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("The moved code reads label, which stays in MainLayout.java",
        refused.getMessage());
    assertEquals(sourceBefore, Files.readString(source));
    assertEquals(targetBefore, Files.readString(target));
  }

  @Test
  @DisplayName("refuses code that reads a field of the view it leaves")
  void shouldRefuseCodeReadingField() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          private final String label = "Save";

          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button(label);
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
          }
        }
        """);

    String sourceBefore = Files.readString(source);
    String targetBefore = Files.readString(target);

    List<SourceLocation> moved = List.of(variable(source, "save", BUTTON));
    AttachPoint from = point(variable(source, "header", FLEX), "add");
    AttachPoint to = point(variable(target, "content", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("The moved code reads label, which stays in MainLayout.java",
        refused.getMessage());
    assertEquals(sourceBefore, Files.readString(source));
    assertEquals(targetBefore, Files.readString(target));
  }

  @Test
  @DisplayName("refuses code that hands out the view it leaves")
  void shouldRefuseCodeReadingThis() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button("Save");
            save.onClick(this::onSave);
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
          }
        }
        """);

    String sourceBefore = Files.readString(source);
    String targetBefore = Files.readString(target);

    List<SourceLocation> moved = List.of(variable(source, "save", BUTTON));
    AttachPoint from = point(variable(source, "header", FLEX), "add");
    AttachPoint to = point(variable(target, "content", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("The moved code reads this, which is the class in MainLayout.java",
        refused.getMessage());
    assertEquals(sourceBefore, Files.readString(source));
    assertEquals(targetBefore, Files.readString(target));
  }

  @Test
  @DisplayName("refuses code that reads a constant of the view it leaves")
  void shouldRefuseCodeReadingConstant() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          private static final String LABEL = "Save";

          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button(LABEL);
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
          }
        }
        """);

    String sourceBefore = Files.readString(source);
    String targetBefore = Files.readString(target);

    List<SourceLocation> moved = List.of(variable(source, "save", BUTTON));
    AttachPoint from = point(variable(source, "header", FLEX), "add");
    AttachPoint to = point(variable(target, "content", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("The moved code reads LABEL, which stays in MainLayout.java",
        refused.getMessage());
    assertEquals(sourceBefore, Files.readString(source));
    assertEquals(targetBefore, Files.readString(target));
  }

  @Test
  @DisplayName("refuses a component the view it leaves still reads")
  void shouldRefuseComponentStillUsed() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button("Save");
            Button reset = new Button("Reset");
            reset.onClick(e -> save.setEnabled(true));
            header.add(save, reset);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            content.setSpacing("1em");
          }
        }
        """);

    String sourceBefore = Files.readString(source);
    String targetBefore = Files.readString(target);

    List<SourceLocation> moved = List.of(variable(source, "save", BUTTON));
    AttachPoint from = point(variable(source, "header", FLEX), "add");
    AttachPoint to = point(variable(target, "content", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("Button save is still used at line 12, reset.onClick(e -> save.setEnabled(true));",
        refused.getMessage());
    assertEquals(sourceBefore, Files.readString(source));
    assertEquals(targetBefore, Files.readString(target));
  }

  @Test
  @DisplayName("imports a type that needed no import in the package it leaves")
  void shouldImportSamePackageTypeOfSourceFile() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            UserCard card = new UserCard();
            header.add(card);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
          }
        }
        """);

    editor.move(List.of(variable(source, "card", "com.example.layout.UserCard")),
        point(variable(source, "header", FLEX), "add"),
        point(variable(target, "content", FLEX), "add"), false);

    assertEquals("""
        package com.example.layout;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
          }
        }
        """, Files.readString(source));
    assertEquals("""
        package com.example.views;

        import com.example.layout.UserCard;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            UserCard card = new UserCard();
            content.add(card);
          }
        }
        """, Files.readString(target));
  }

  @Test
  @DisplayName("adds no import for a type of the package both files share")
  void shouldSkipImportInsideOnePackage() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            UserCard card = new UserCard();
            header.add(card);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
          }
        }
        """);

    editor.move(List.of(variable(source, "card", "com.example.views.UserCard")),
        point(variable(source, "header", FLEX), "add"),
        point(variable(target, "content", FLEX), "add"), false);

    assertEquals("""
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
          }
        }
        """, Files.readString(source));
    assertEquals("""
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            UserCard card = new UserCard();
            content.add(card);
          }
        }
        """, Files.readString(target));
  }

  @Test
  @DisplayName("resolves a type the file it leaves imports through a wildcard")
  void shouldResolveWildcardImport() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.*;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button("Save");
            save.setTheme(ButtonTheme.PRIMARY);
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
          }
        }
        """);

    editor.move(List.of(variable(source, "save", BUTTON)),
        point(variable(source, "header", FLEX), "add"),
        point(variable(target, "content", FLEX), "add"), false);

    assertEquals("""
        package com.example.layout;

        import com.webforj.component.button.*;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
          }
        }
        """, Files.readString(source));
    assertEquals("""
        package com.example.views;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            Button save = new Button("Save");
            save.setTheme(ButtonTheme.PRIMARY);
            content.add(save);
          }
        }
        """, Files.readString(target));
  }

  @Test
  @DisplayName("refuses when the other file imports another type of the same name")
  void shouldRefuseImportCollision() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button("Save");
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.example.widgets.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
          }
        }
        """);

    String sourceBefore = Files.readString(source);
    String targetBefore = Files.readString(target);

    List<SourceLocation> moved = List.of(variable(source, "save", BUTTON));
    AttachPoint from = point(variable(source, "header", FLEX), "add");
    AttachPoint to = point(variable(target, "content", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("The target file already imports another Button, " + OTHER_BUTTON,
        refused.getMessage());
    assertEquals(sourceBefore, Files.readString(source));
    assertEquals(targetBefore, Files.readString(target));
  }

  @Test
  @DisplayName("refuses code that uses a type nested in the class it leaves")
  void shouldRefuseNestedTypeOfSourceClass() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Badge badge = new Badge();
            header.add(badge);
          }

          static class Badge extends FlexLayout {}
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
          }
        }
        """);

    String sourceBefore = Files.readString(source);
    String targetBefore = Files.readString(target);

    List<SourceLocation> moved =
        List.of(variable(source, "badge", "com.example.layout.MainLayout.Badge"));
    AttachPoint from = point(variable(source, "header", FLEX), "add");
    AttachPoint to = point(variable(target, "content", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("The moved code uses Badge, which is declared inside the class it leaves",
        refused.getMessage());
    assertEquals(sourceBefore, Files.readString(source));
    assertEquals(targetBefore, Files.readString(target));
  }

  @Test
  @DisplayName("writes into the bound component of a composite in the other file")
  void shouldMoveIntoBoundComponentOfOtherFile() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button("Save");
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.Composite;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView extends Composite<FlexLayout> {
        }
        """);

    editor.move(List.of(variable(source, "save", BUTTON)),
        point(variable(source, "header", FLEX), "add"), point(line(target, 6, FLEX), "add"), false);

    assertEquals("""
        package com.example.layout;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
          }
        }
        """, Files.readString(source));
    assertEquals("""
        package com.example.views;

        import com.webforj.component.Composite;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView extends Composite<FlexLayout> {
          public DashboardView() {
            Button save = new Button("Save");
            getBoundComponent().add(save);
          }
        }
        """, Files.readString(target));
  }

  @Test
  @DisplayName("keeps the moved code as the developer wrote it at the new depth")
  void shouldKeepFormattingOfMovedCode() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            // The main action
            Button save = new Button("Save");
            save.onClick(e -> {
              if (e != null) {
                save.setEnabled(false);
              }
            });
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView(boolean wide) {
            if (wide) {
              FlexLayout content = new FlexLayout();
              content.setSpacing("1em");
            }
          }
        }
        """);

    editor.move(List.of(variable(source, "save", BUTTON)),
        point(variable(source, "header", FLEX), "add"),
        point(variable(target, "content", FLEX), "add"), false);

    assertEquals("""
        package com.example.layout;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
          }
        }
        """, Files.readString(source));
    assertEquals("""
        package com.example.views;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView(boolean wide) {
            if (wide) {
              FlexLayout content = new FlexLayout();
              content.setSpacing("1em");
              // The main action
              Button save = new Button("Save");
              save.onClick(e -> {
                if (e != null) {
                  save.setEnabled(false);
                }
              });
              content.add(save);
            }
          }
        }
        """, Files.readString(target));
  }

  @Test
  @DisplayName("leaves both files alone when the other file has a syntax error")
  void shouldRefuseBrokenTargetFile() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button("Save");
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout(
          }
        }
        """);

    String sourceBefore = Files.readString(source);
    String targetBefore = Files.readString(target);

    List<SourceLocation> moved = List.of(variable(source, "save", BUTTON));
    AttachPoint from = point(variable(source, "header", FLEX), "add");
    AttachPoint to = point(variable(target, "content", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("Failed to parse source file: " + target, refused.getMessage());
    assertEquals(sourceBefore, Files.readString(source));
    assertEquals(targetBefore, Files.readString(target));
  }

  @Test
  @DisplayName("leaves both files alone when the other file lacks the parent")
  void shouldRefuseUnknownTargetParent() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button("Save");
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
          }
        }
        """);

    String sourceBefore = Files.readString(source);
    String targetBefore = Files.readString(target);

    List<SourceLocation> moved = List.of(variable(source, "save", BUTTON));
    AttachPoint from = point(variable(source, "header", FLEX), "add");
    AttachPoint to = point(variable(target, "missing", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("No declaration of FlexLayout missing found in DashboardView.java",
        refused.getMessage());
    assertEquals(sourceBefore, Files.readString(source));
    assertEquals(targetBefore, Files.readString(target));
  }

  @Test
  @DisplayName("refuses a component whose parent attaches it in another file")
  void shouldRefuseParentOfOtherFile() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button("Save");
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
          }
        }
        """);

    String sourceBefore = Files.readString(source);
    String targetBefore = Files.readString(target);

    List<SourceLocation> moved = List.of(variable(source, "save", BUTTON));
    AttachPoint from = point(variable(target, "content", FLEX), "add");
    AttachPoint to = point(variable(target, "content", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals(
        "Button save is created in MainLayout.java and attached in DashboardView.java, the two"
            + " must be one file",
        refused.getMessage());
    assertEquals(sourceBefore, Files.readString(source));
    assertEquals(targetBefore, Files.readString(target));
  }

  @Test
  @DisplayName("writes nothing on a dry run and returns the patch of both files")
  void shouldReturnBothPatchesOnDryRun() throws IOException {
    Path source = fixture.write("MainLayout.java", SOURCE);
    Path target = fixture.write("DashboardView.java", TARGET);

    List<FilePatch> patches = editor.move(List.of(variable(source, "save", BUTTON)),
        point(variable(source, "header", FLEX), "add"),
        point(variable(target, "content", FLEX), "add"), true);

    assertEquals(List.of(source.toString(), target.toString()),
        patches.stream().map(FilePatch::getFile).toList());
    assertEquals(SOURCE, patches.get(0).getOriginal());
    assertEquals("""
        package com.example.layout;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
          }
        }
        """, patches.get(0).getPatched());
    assertEquals(TARGET, patches.get(1).getOriginal());
    assertEquals("""
        package com.example.views;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            Button save = new Button("Save");
            content.add(save);
          }
        }
        """, patches.get(1).getPatched());
    assertEquals(SOURCE, Files.readString(source));
    assertEquals(TARGET, Files.readString(target));
  }

  @Test
  @DisplayName("restores the first file when the second one cannot be written")
  void shouldRestoreFirstFileWhenSecondWriteFails() throws IOException {
    Path source = fixture.write("MainLayout.java", SOURCE);
    Path target = fixture.write("DashboardView.java", TARGET);
    assertTrue(target.toFile().setWritable(false));

    try {
      List<SourceLocation> moved = List.of(variable(source, "save", BUTTON));
      AttachPoint from = point(variable(source, "header", FLEX), "add");
      AttachPoint to = point(variable(target, "content", FLEX), "add");

      assertThrows(StagingException.class, () -> editor.move(moved, from, to, false));
    } finally {
      assertTrue(target.toFile().setWritable(true));
    }

    assertEquals(SOURCE, Files.readString(source));
    assertEquals(TARGET, Files.readString(target));
  }

  @Test
  @DisplayName("refuses code that reads the parent class of the view it leaves")
  void shouldRefuseCodeReadingSuper() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button(super.toString());
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
          }
        }
        """);

    String sourceBefore = Files.readString(source);
    String targetBefore = Files.readString(target);

    List<SourceLocation> moved = List.of(variable(source, "save", BUTTON));
    AttachPoint from = point(variable(source, "header", FLEX), "add");
    AttachPoint to = point(variable(target, "content", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals("The moved code reads this, which is the class in MainLayout.java",
        refused.getMessage());
    assertEquals(sourceBefore, Files.readString(source));
    assertEquals(targetBefore, Files.readString(target));
  }

  @Test
  @DisplayName("imports nested, generic and lambda types and skips java.lang and var")
  void shouldImportOnlyWhatTheOtherFileLacks() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        package com.example.layout;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import java.util.Map;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            Button save = new Button(String.valueOf(1));
            save.onClick(e -> {
              Map.Entry<String, Integer> entry = Map.entry("a", Integer.valueOf(1));
              var key = entry.getKey();
              save.setText(key);
            });
            header.add(save);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", """
        package com.example.views;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
          }
        }
        """);

    editor.move(List.of(variable(source, "save", BUTTON)),
        point(variable(source, "header", FLEX), "add"),
        point(variable(target, "content", FLEX), "add"), false);

    assertEquals("""
        package com.example.layout;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
          }
        }
        """, Files.readString(source));
    assertEquals("""
        package com.example.views;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import java.util.Map;

        public class DashboardView {
          public DashboardView() {
            FlexLayout content = new FlexLayout();
            Button save = new Button(String.valueOf(1));
            save.onClick(e -> {
              Map.Entry<String, Integer> entry = Map.entry("a", Integer.valueOf(1));
              var key = entry.getKey();
              save.setText(key);
            });
            content.add(save);
          }
        }
        """, Files.readString(target));
  }

  @Test
  @DisplayName("refuses a type of the default package, which no other package can import")
  void shouldRefuseTypeOfDefaultPackage() throws IOException {
    Path source = fixture.write("MainLayout.java", """
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class MainLayout {
          public MainLayout() {
            FlexLayout header = new FlexLayout();
            UserCard card = new UserCard();
            header.add(card);
          }
        }
        """);
    Path target = fixture.write("DashboardView.java", TARGET);
    String sourceBefore = Files.readString(source);

    List<SourceLocation> moved = List.of(variable(source, "card", "UserCard"));
    AttachPoint from = point(variable(source, "header", FLEX), "add");
    AttachPoint to = point(variable(target, "content", FLEX), "add");

    SourceModificationException refused =
        assertThrows(SourceModificationException.class, () -> editor.move(moved, from, to, false));

    assertEquals(
        "The moved code uses UserCard, which sits in the default package and cannot be imported",
        refused.getMessage());
    assertEquals(sourceBefore, Files.readString(source));
    assertEquals(TARGET, Files.readString(target));
  }
}
