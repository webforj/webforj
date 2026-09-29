package com.webforj.devtools.craftforj.source.structure;

import static com.webforj.devtools.craftforj.source.structure.StructureFixture.BUTTON;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.FLEX;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.TEXT_FIELD;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.TOOLBAR;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.after;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.before;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.line;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.point;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.variable;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.structure.model.AttachPoint;
import com.webforj.devtools.craftforj.source.structure.model.ComponentCreation;
import com.webforj.devtools.craftforj.source.structure.model.InsertResult;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Named;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

@DisplayName("StructureModifier insert")
class StructureModifierInsertTest {

  private final StructureModifier editor = StructureFixture.createEditor();
  private StructureFixture fixture;

  @TempDir
  Path tempDir;

  @BeforeEach
  void setUp() {
    fixture = new StructureFixture(tempDir);
  }

  private static ComponentCreation button() {
    return new ComponentCreation(BUTTON, "new Button(\"Button\")");
  }

  private static ComponentCreation themedButton() {
    ComponentCreation creation = button();
    creation.getCalls().add("setTheme(ButtonTheme.PRIMARY)");
    creation.getCalls().add("setWidth(\"10em\")");
    creation.getImports().add("com.webforj.component.button.ButtonTheme");

    return creation;
  }

  @ParameterizedTest
  @MethodSource("siblingAttachCases")
  void shouldAttachNextToSibling(String source, String expected) throws IOException {
    Path file = fixture.write("View.java", source);

    editor.insert(button(),
        after(variable(file, "layout", FLEX), variable(file, "save", BUTTON), "add"), false);

    assertEquals(expected, Files.readString(file));
  }

  @Test
  @DisplayName("adds an argument before the sibling")
  void shouldAddArgumentBeforeSibling() throws IOException {
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
          }
        }
        """);

    editor.insert(button(),
        before(variable(file, "layout", FLEX), variable(file, "save", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button button = new Button("Button");
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            layout.add(button, save, cancel);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("appends to the only attach call when no sibling is named")
  void shouldAppendToOnlyCall() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            layout.add(save);
          }
        }
        """);

    editor.insert(button(), point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            Button button = new Button("Button");
            layout.add(save, button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("writes before the declaration of a sibling whose statements stand together")
  void shouldWriteOwnCallBeforeGroupedSibling() throws IOException {
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
            cancel.setEnabled(false);
            layout.add(cancel);
          }
        }
        """);

    editor.insert(button(),
        before(variable(file, "layout", FLEX), variable(file, "cancel", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            layout.add(save);
            Button button = new Button("Button");
            layout.add(button);
            Button cancel = new Button("Cancel");
            cancel.setEnabled(false);
            layout.add(cancel);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("writes right before the sibling call when the declarations stand apart from the"
      + " calls")
  void shouldWriteOwnCallBeforeSiblingCall() throws IOException {
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
            layout.add(cancel);
          }
        }
        """);

    editor.insert(button(),
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
            layout.add(save);
            Button button = new Button("Button");
            layout.add(button);
            layout.add(cancel);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("fills an empty slot after the calls that configure the parent")
  void shouldFillEmptySlotAfterParentCalls() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.setSpacing("1em");
            layout.setMargin("0");
            String title = "Title";
          }
        }
        """);

    editor.insert(button(), point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.setSpacing("1em");
            layout.setMargin("0");
            Button button = new Button("Button");
            layout.add(button);
            String title = "Title";
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("fills an empty slot right after a parent nothing configures")
  void shouldFillEmptySlotAfterParentDeclaration() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            String title = "Title";
          }
        }
        """);

    editor.insert(button(), point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button button = new Button("Button");
            layout.add(button);
            String title = "Title";
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("fills an empty slot of a field parent after its assignment")
  void shouldFillEmptySlotOfFieldAssignedInConstructor() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private FlexLayout layout;

          public View() {
            layout = new FlexLayout();
            String title = "Title";
          }
        }
        """);

    editor.insert(button(), point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private FlexLayout layout;

          public View() {
            layout = new FlexLayout();
            Button button = new Button("Button");
            layout.add(button);
            String title = "Title";
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("fills an empty slot where a method configures the field parent")
  void shouldFillEmptySlotOfFieldConfiguredInMethod() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();

          public View() {
            build();
          }

          private void build() {
            layout.setSpacing("1em");
          }
        }
        """);

    editor.insert(button(), point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();

          public View() {
            build();
          }

          private void build() {
            layout.setSpacing("1em");
            Button button = new Button("Button");
            layout.add(button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("creates a constructor when the field parent is touched nowhere")
  void shouldCreateConstructorForUntouchedField() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();
        }
        """);

    editor.insert(button(), point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();

          public View() {
            Button button = new Button("Button");
            layout.add(button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("attaches through getBoundComponent in a composite")
  void shouldAttachToBoundComponent() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.Composite;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends Composite<FlexLayout> {
          public View() {
            getBoundComponent().setSpacing("1em");
          }
        }
        """);

    editor.insert(button(), point(line(file, 6, FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.Composite;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends Composite<FlexLayout> {
          public View() {
            getBoundComponent().setSpacing("1em");
            Button button = new Button("Button");
            getBoundComponent().add(button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("adds an argument to a getBoundComponent call")
  void shouldAttachNextToBoundComponentSibling() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.Composite;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends Composite<FlexLayout> {
          public View() {
            Button save = new Button("Save");
            getBoundComponent().add(save);
          }
        }
        """);

    editor.insert(button(), before(line(file, 7, FLEX), variable(file, "save", BUTTON), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.Composite;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends Composite<FlexLayout> {
          public View() {
            Button button = new Button("Button");
            Button save = new Button("Save");
            getBoundComponent().add(button, save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("attaches through the alias a composite keeps for its bound component")
  void shouldAttachThroughBoundComponentAlias() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.Composite;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends Composite<FlexLayout> {
          private final FlexLayout self = getBoundComponent();

          public View() {
            self.setSpacing("1em");
          }
        }
        """);

    editor.insert(button(), point(variable(file, "self", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.Composite;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends Composite<FlexLayout> {
          private final FlexLayout self = getBoundComponent();

          public View() {
            self.setSpacing("1em");
            Button button = new Button("Button");
            self.add(button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("attaches through the alias when the composite is addressed by its class")
  void shouldAttachThroughAliasOfClassAddressedComposite() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.Composite;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends Composite<FlexLayout> {
          private final FlexLayout self = getBoundComponent();

          public View() {
            Button save = new Button("Save");
            self.add(save);
          }
        }
        """);

    SourceLocation parent =
        new SourceLocation(file.toString(), null, "com.example.View", null, FLEX);
    editor.insert(button(), before(parent, variable(file, "save", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.Composite;
        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends Composite<FlexLayout> {
          private final FlexLayout self = getBoundComponent();

          public View() {
            Button button = new Button("Button");
            Button save = new Button("Save");
            self.add(button, save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("attaches with an unscoped call when the view extends a container")
  void shouldAttachToTheClassItself() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Div;

        public class View extends Div {
          public View() {
            Button save = new Button("Save");
            add(save);
            setText("Title");
          }
        }
        """);

    editor.insert(button(),
        after(line(file, 6, "com.example.View"), variable(file, "save", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Div;

        public class View extends Div {
          public View() {
            Button save = new Button("Save");
            Button button = new Button("Button");
            add(save, button);
            setText("Title");
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("appends at the end of the constructor of a view with no children")
  void shouldAppendToTheClassItself() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.html.elements.Div;

        public class View extends Div {
          public View() {
            setText("Title");
          }
        }
        """);

    editor.insert(button(), point(line(file, 5, "com.example.View"), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Div;

        public class View extends Div {
          public View() {
            setText("Title");
            Button button = new Button("Button");
            add(button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("attaches into a named slot through its own method")
  void shouldAttachIntoNamedSlot() throws IOException {
    Path file = fixture.write("View.java", """
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
        """);

    editor.insert(button(),
        after(variable(file, "toolbar", TOOLBAR), variable(file, "menu", BUTTON), "addToStart"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.toolbar.Toolbar;

        public class View {
          public View() {
            Toolbar toolbar = new Toolbar();
            Button menu = new Button("Menu");
            Button button = new Button("Button");
            Button help = new Button("Help");
            toolbar.addToStart(menu, button);
            toolbar.addToEnd(help);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("adds an argument inside a fluent chain that fills several slots")
  void shouldAttachIntoFluentChain() throws IOException {
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

    editor.insert(button(),
        before(variable(file, "toolbar", TOOLBAR), variable(file, "help", BUTTON), "addToEnd"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.toolbar.Toolbar;

        public class View {
          public View() {
            Toolbar toolbar = new Toolbar();
            Button menu = new Button("Menu");
            Button button = new Button("Button");
            Button help = new Button("Help");
            toolbar.addToStart(menu).addToEnd(button, help);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("fills an empty named slot after the last call on the parent run")
  void shouldFillEmptyNamedSlot() throws IOException {
    Path file = fixture.write("View.java", """
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
        """);

    editor.insert(button(), point(variable(file, "toolbar", TOOLBAR), "addToEnd"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.toolbar.Toolbar;

        public class View {
          public View() {
            Toolbar toolbar = new Toolbar();
            Button menu = new Button("Menu");
            toolbar.addToStart(menu);
            Button button = new Button("Button");
            toolbar.addToEnd(button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("creates the component inline when the sibling is created inline")
  void shouldCreateInlineNextToInlineSibling() throws IOException {
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

    editor.insert(button(), after(variable(file, "layout", FLEX), line(file, 9, BUTTON), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.add(new Button("Save"), new Button("Button"));
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("keeps the fixed leading argument of a creation in place")
  void shouldAddAfterFixedConstructorArgument() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexDirection;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            Button save = new Button("Save");
            FlexLayout layout = new FlexLayout(FlexDirection.ROW, save);
          }
        }
        """);

    editor.insert(button(),
        before(variable(file, "layout", FLEX), variable(file, "save", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexDirection;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            Button button = new Button("Button");
            Button save = new Button("Save");
            FlexLayout layout = new FlexLayout(FlexDirection.ROW, button, save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("appends with a call when the children so far come from the creation")
  void shouldAppendAfterConstructorChildren() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            Button save = new Button("Save");
            FlexLayout layout = new FlexLayout(save);
            layout.setSpacing("1em");
          }
        }
        """);

    editor.insert(button(), point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            Button save = new Button("Save");
            FlexLayout layout = new FlexLayout(save);
            layout.setSpacing("1em");
            Button button = new Button("Button");
            layout.add(button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("refuses a place next to a child handed to a creation with fixed arguments")
  void shouldRefuseFixedCreationArguments() throws IOException {
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

    ComponentCreation creation = button();
    AttachPoint place = after(variable(file, "panel", "com.example.CardPanel"),
        variable(file, "save", BUTTON), "add");

    SourceModificationException refused = assertThrows(SourceModificationException.class,
        () -> editor.insert(creation, place, false));

    assertEquals(
        "Cannot place a component next to Button save, it is handed to the creation of CardPanel"
            + " panel which takes a fixed set of arguments",
        refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("writes a call of its own when no loaded signature proves varargs")
  void shouldWriteOwnCallForUnknownSignature() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;

        public class View {
          public View() {
            CardPanel panel = new CardPanel();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            panel.addItem(save);
            panel.addItem(cancel);
          }
        }
        """);

    editor.insert(button(), after(variable(file, "panel", "com.example.CardPanel"),
        variable(file, "save", BUTTON), "addItem"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;

        public class View {
          public View() {
            CardPanel panel = new CardPanel();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            panel.addItem(save);
            Button button = new Button("Button");
            panel.addItem(button);
            panel.addItem(cancel);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("refuses a sibling whose statement chains several fixed attach calls")
  void shouldRefuseChainOfFixedCalls() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;

        public class View {
          public View() {
            CardPanel panel = new CardPanel();
            Button save = new Button("Save");
            Button cancel = new Button("Cancel");
            panel.addItem(save).addItem(cancel);
          }
        }
        """);

    String original = Files.readString(file);

    ComponentCreation creation = button();
    AttachPoint place = after(variable(file, "panel", "com.example.CardPanel"),
        variable(file, "save", BUTTON), "addItem");

    SourceModificationException refused = assertThrows(SourceModificationException.class,
        () -> editor.insert(creation, place, false));

    assertEquals(
        "Cannot place a component next to Button save, its statement attaches several components"
            + " in one chain",
        refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("gives a parent created inline a variable before attaching to it")
  void shouldNameInlineParentBeforeAttaching() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends FlexLayout {
          public View() {
            add(new FlexLayout());
          }
        }
        """);

    editor.insert(button(), point(line(file, 8, FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View extends FlexLayout {
          public View() {
            FlexLayout flexlayout = new FlexLayout();
            Button button = new Button("Button");
            flexlayout.add(button);
            add(flexlayout);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("writes the calls of the creation and imports what they need")
  void shouldWriteCreationCallsAndImports() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
          }
        }
        """);

    editor.insert(themedButton(), point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button button = new Button("Button");
            button.setTheme(ButtonTheme.PRIMARY);
            button.setWidth("10em");
            layout.add(button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("declares a variable next to an inline sibling when the creation has calls")
  void shouldDeclareCreationWithCallsNextToInlineSibling() throws IOException {
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

    editor.insert(themedButton(),
        after(variable(file, "layout", FLEX), line(file, 9, BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.button.ButtonTheme;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button button = new Button("Button");
            button.setTheme(ButtonTheme.PRIMARY);
            button.setWidth("10em");
            layout.add(new Button("Save"), button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("picks a name no variable, field or parameter of the class uses")
  void shouldPickFreeName() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private Button button2;

          public View(String button3) {
            FlexLayout layout = new FlexLayout();
            Button button = new Button("Save");
            layout.add(button);
          }
        }
        """);

    editor.insert(button(), point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private Button button2;

          public View(String button3) {
            FlexLayout layout = new FlexLayout();
            Button button = new Button("Save");
            Button button4 = new Button("Button");
            layout.add(button, button4);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("names the variable after the type in camel case")
  void shouldNameAfterCamelCaseType() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
          }
        }
        """);

    editor.insert(new ComponentCreation(TEXT_FIELD, "new TextField(\"Label\")"),
        point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.field.TextField;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            TextField textField = new TextField("Label");
            layout.add(textField);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("finds the calls on a field parent read through this")
  void shouldAttachToFieldReadThroughThis() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();

          public View() {
            Button save = new Button("Save");
            this.layout.add(save);
          }
        }
        """);

    editor.insert(button(),
        before(variable(file, "layout", FLEX), variable(file, "save", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();

          public View() {
            Button button = new Button("Button");
            Button save = new Button("Save");
            this.layout.add(button, save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("appends after the branch that holds the last attach call")
  void shouldAppendAfterBranchHoldingLastAttach() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(boolean admin) {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            layout.add(save);
            if (admin) {
              Button delete = new Button("Delete");
              layout.add(delete);
            }
          }
        }
        """);

    editor.insert(button(), point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View(boolean admin) {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            layout.add(save);
            if (admin) {
              Button delete = new Button("Delete");
              layout.add(delete);
            }
            Button button = new Button("Button");
            layout.add(button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("ignores calls that reach another component through an accessor")
  void shouldIgnoreCallsThroughAccessor() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.applayout.AppLayout;

        public class View {
          public View() {
            AppLayout layout = new AppLayout();
            Button save = new Button("Save");
            layout.getHeader().add(save);
          }
        }
        """);

    String original = Files.readString(file);

    ComponentCreation creation = button();
    AttachPoint place =
        after(variable(file, "layout", "com.webforj.component.layout.applayout.AppLayout"),
            variable(file, "save", BUTTON), "add");

    SourceModificationException refused = assertThrows(SourceModificationException.class,
        () -> editor.insert(creation, place, false));

    assertEquals("Button save is not attached to AppLayout layout with [add] in View.java",
        refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @ParameterizedTest
  @MethodSource("refusedSiblingCases")
  void shouldRefuseSibling(String source, String sibling, String message) throws IOException {
    Path file = fixture.write("View.java", source);
    String original = Files.readString(file);
    ComponentCreation creation = button();
    AttachPoint place =
        after(variable(file, "layout", FLEX), variable(file, sibling, BUTTON), "add");

    SourceModificationException refused = assertThrows(SourceModificationException.class,
        () -> editor.insert(creation, place, false));

    assertEquals(message, refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("refuses a parent the file does not declare")
  void shouldRefuseUnknownParent() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            layout.add(save);
          }
        }
        """);

    String original = Files.readString(file);

    ComponentCreation creation = button();
    AttachPoint place = point(variable(file, "missing", FLEX), "add");

    SourceModificationException refused = assertThrows(SourceModificationException.class,
        () -> editor.insert(creation, place, false));

    assertEquals("No declaration of FlexLayout missing found in View.java", refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("refuses a parent name declared twice with no line to tell them apart")
  void shouldRefuseAmbiguousParent() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
          }

          private void build() {
            FlexLayout layout = new FlexLayout();
          }
        }
        """);

    String original = Files.readString(file);

    ComponentCreation creation = button();
    AttachPoint place = point(variable(file, "layout", FLEX), "add");

    SourceModificationException refused = assertThrows(SourceModificationException.class,
        () -> editor.insert(creation, place, false));

    assertEquals("More than one declaration of FlexLayout layout found in View.java",
        refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("tells two parents of the same name apart by the line")
  void shouldTellDeclarationsApartByLine() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
          }

          private void build() {
            FlexLayout layout = new FlexLayout();
          }
        }
        """);

    editor.insert(button(),
        point(new SourceLocation(file.toString(), 12, null, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
          }

          private void build() {
            FlexLayout layout = new FlexLayout();
            Button button = new Button("Button");
            layout.add(button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("refuses a creation whose creation is not Java")
  void shouldRefuseCreationThatDoesNotParse() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            layout.add(save);
          }
        }
        """);

    String original = Files.readString(file);

    ComponentCreation creation = new ComponentCreation(BUTTON, "new Button(");
    AttachPoint place = point(variable(file, "layout", FLEX), "add");

    SourceModificationException refused = assertThrows(SourceModificationException.class,
        () -> editor.insert(creation, place, false));

    assertEquals("The creation of Button does not parse, new Button(", refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("refuses a file with a syntax error and leaves it untouched")
  void shouldLeaveBrokenFileUntouched() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout(
          }
        }
        """);

    String original = Files.readString(file);

    ComponentCreation creation = button();
    AttachPoint place = point(variable(file, "layout", FLEX), "add");

    SourceModificationException refused = assertThrows(SourceModificationException.class,
        () -> editor.insert(creation, place, false));

    assertEquals("Failed to parse source file: " + file, refused.getMessage());
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("follows the local sibling of the method over a field of the same name")
  void shouldFollowLocalSiblingOverFieldOfSameName() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private Button save;

          public View() {
            save = new Button("Field");
            build();
          }

          private void build() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Local");
            layout.add(save);
          }
        }
        """);

    editor.insert(button(),
        point(new SourceLocation(file.toString(), 15, null, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private Button save;

          public View() {
            save = new Button("Field");
            build();
          }

          private void build() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Local");
            Button button = new Button("Button");
            layout.add(save, button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("proves the varargs call of a parent known by its simple type only")
  void shouldResolveSimpleParentTypeThroughImport() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            layout.add(save);
          }
        }
        """);

    editor.insert(button(),
        after(variable(file, "layout", "FlexLayout"), variable(file, "save", BUTTON), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            Button button = new Button("Button");
            layout.add(save, button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("proves the varargs call of a parent imported through a wildcard")
  void shouldResolveSimpleParentTypeThroughWildcard() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.*;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            layout.add(save);
          }
        }
        """);

    editor.insert(button(),
        after(variable(file, "layout", "FlexLayout"), variable(file, "save", BUTTON), "add"),
        false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.*;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button save = new Button("Save");
            Button button = new Button("Button");
            layout.add(save, button);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("leaves a static helper call alone when the view itself is the parent")
  void shouldLeaveStaticHelperCallsAlone() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Div;

        public class View extends Div {
          public View() {
            Button save = new Button("Save");
            super.add(save);
            Styles.add(save);
          }
        }
        """);

    editor.insert(button(),
        after(line(file, 6, "com.example.View"), variable(file, "save", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.html.elements.Div;

        public class View extends Div {
          public View() {
            Button save = new Button("Save");
            Button button = new Button("Button");
            super.add(save, button);
            Styles.add(save);
          }
        }
        """, Files.readString(file));
  }

  @Test
  @DisplayName("answers the line and name of a new field initialized where it is declared")
  void shouldLocateNewInitializedField() throws IOException {
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
        }
        """);

    InsertResult result = editor.insert(button(),
        after(variable(file, "layout", FLEX), variable(file, "save", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private final FlexLayout layout = new FlexLayout();
          private final Button save = new Button("Save");
          private Button button = new Button("Button");

          public View() {
            layout.add(save, button);
          }
        }
        """, Files.readString(file));
    SourceLocation location = result.getLocation();
    assertEquals(file.toString(), location.getFile());
    assertEquals(9, location.getLine());
    assertEquals("com.example.View", location.getDeclaringClass());
    assertEquals("button", location.getVariableName());
    assertEquals(BUTTON, location.getComponentType());
  }

  @Test
  @DisplayName("answers the line of the assignment that creates a new field")
  void shouldLocateNewAssignedField() throws IOException {
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
            layout.add(save);
          }
        }
        """);

    InsertResult result = editor.insert(button(),
        after(variable(file, "layout", FLEX), variable(file, "save", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private FlexLayout layout;
          private Button save;
          private Button button;

          public View() {
            layout = new FlexLayout();
            save = new Button("Save");
            button = new Button("Button");
            layout.add(save, button);
          }
        }
        """, Files.readString(file));
    assertEquals(14, result.getLocation().getLine());
    assertEquals("button", result.getLocation().getVariableName());
  }

  @Test
  @DisplayName("locates a field initializer creation by its text, not by its index in the tree")
  void shouldLocateFieldInitializerAmongLaterCreations() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private FlexLayout layout = new FlexLayout();
          private Button save = new Button("Save");

          public View() {
            layout.add(save);
            layout.add(new Button("Later"));
          }
        }
        """);

    InsertResult result = editor.insert(button(),
        after(variable(file, "layout", FLEX), variable(file, "save", BUTTON), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          private FlexLayout layout = new FlexLayout();
          private Button save = new Button("Save");
          private Button button = new Button("Button");

          public View() {
            layout.add(save);
            layout.add(button);
            layout.add(new Button("Later"));
          }
        }
        """, Files.readString(file));
    assertEquals(9, result.getLocation().getLine());
    assertEquals("button", result.getLocation().getVariableName());
  }

  @Test
  @DisplayName("answers the line of a new inline creation, which has no name")
  void shouldLocateNewInlineCreation() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.add(new Button("Save"));
            layout.add(new Button("Cancel"));
          }
        }
        """);

    InsertResult result =
        editor.insert(button(), point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            layout.add(new Button("Save"));
            layout.add(new Button("Cancel"));
            layout.add(new Button("Button"));
          }
        }
        """, Files.readString(file));
    assertEquals(11, result.getLocation().getLine());
    assertNull(result.getLocation().getVariableName());
  }

  @Test
  @DisplayName("names the top level class as the declaring class of a creation in a nested class")
  void shouldLocateCreationInNestedClass() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          static class Panel {
            Panel() {
              FlexLayout layout = new FlexLayout();
            }
          }
        }
        """);

    InsertResult result =
        editor.insert(button(), point(variable(file, "layout", FLEX), "add"), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          static class Panel {
            Panel() {
              FlexLayout layout = new FlexLayout();
              Button button = new Button("Button");
              layout.add(button);
            }
          }
        }
        """, Files.readString(file));
    assertEquals(10, result.getLocation().getLine());
    assertEquals("com.example.View", result.getLocation().getDeclaringClass());
  }

  @Test
  @DisplayName("answers the location on a dry run and leaves the file alone")
  void shouldLocateOnDryRun() throws IOException {
    String before = """
        package com.example;

        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
          }
        }
        """;
    Path file = fixture.write("View.java", before);

    InsertResult result =
        editor.insert(button(), point(variable(file, "layout", FLEX), "add"), true);

    assertEquals(before, Files.readString(file));
    assertEquals("""
        package com.example;

        import com.webforj.component.button.Button;
        import com.webforj.component.layout.flexlayout.FlexLayout;

        public class View {
          public View() {
            FlexLayout layout = new FlexLayout();
            Button button = new Button("Button");
            layout.add(button);
          }
        }
        """, result.getFiles().get(0).getPatched());
    assertEquals(9, result.getLocation().getLine());
  }

  @Test
  @DisplayName("writes a lambda argument of a part call as it was given")
  void shouldKeepLambdaArgumentText() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        import com.webforj.component.table.Table;

        public class View {
          public View() {
            Table<String> table = new Table<>();
            table.addColumn("Name", name -> name);
          }
        }
        """);

    editor.insertCall(
        point(variable(file, "table", "com.webforj.component.table.Table"), "addColumn"),
        List.of("\"Column\"", "item -> \"\""), Set.of(), false);

    assertEquals("""
        package com.example;

        import com.webforj.component.table.Table;

        public class View {
          public View() {
            Table<String> table = new Table<>();
            table.addColumn("Name", name -> name);
            table.addColumn("Column", item -> "");
          }
        }
        """, Files.readString(file));
  }

  private static Stream<Arguments> siblingAttachCases() {
    return Stream.of(
        Arguments.of(
            Named.of("adds an argument after the sibling of a call listing several children", """
                package com.example;

                import com.webforj.component.button.Button;
                import com.webforj.component.layout.flexlayout.FlexLayout;

                public class View {
                  public View() {
                    FlexLayout layout = new FlexLayout();
                    Button save = new Button("Save");
                    Button cancel = new Button("Cancel");
                    layout.add(save, cancel);
                  }
                }
                """), """
                package com.example;

                import com.webforj.component.button.Button;
                import com.webforj.component.layout.flexlayout.FlexLayout;

                public class View {
                  public View() {
                    FlexLayout layout = new FlexLayout();
                    Button save = new Button("Save");
                    Button button = new Button("Button");
                    Button cancel = new Button("Cancel");
                    layout.add(save, button, cancel);
                  }
                }
                """),
        Arguments
            .of(Named.of("writes a call of its own in a file that attaches one child per call", """
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
                """), """
                package com.example;

                import com.webforj.component.button.Button;
                import com.webforj.component.layout.flexlayout.FlexLayout;

                public class View {
                  public View() {
                    FlexLayout layout = new FlexLayout();
                    Button save = new Button("Save");
                    layout.add(save);
                    Button button = new Button("Button");
                    layout.add(button);
                    Button cancel = new Button("Cancel");
                    layout.add(cancel);
                  }
                }
                """),
        Arguments
            .of(Named.of("declares a field when the sibling is a field with an initializer", """
                package com.example;

                import com.webforj.component.button.Button;
                import com.webforj.component.layout.flexlayout.FlexLayout;

                public class View {
                  private final FlexLayout layout = new FlexLayout();
                  private final Button save = new Button("Save");
                  private String title;

                  public View() {
                    layout.add(save);
                  }
                }
                """), """
                package com.example;

                import com.webforj.component.button.Button;
                import com.webforj.component.layout.flexlayout.FlexLayout;

                public class View {
                  private final FlexLayout layout = new FlexLayout();
                  private final Button save = new Button("Save");
                  private Button button = new Button("Button");
                  private String title;

                  public View() {
                    layout.add(save, button);
                  }
                }
                """),
        Arguments
            .of(Named.of("declares a field and assigns it when the sibling is assigned in code", """
                package com.example;

                import com.webforj.component.button.Button;
                import com.webforj.component.layout.flexlayout.FlexLayout;

                public class View {
                  private FlexLayout layout;
                  private Button save;

                  public View() {
                    layout = new FlexLayout();
                    save = new Button("Save");
                    save.setEnabled(false);
                    layout.add(save);
                  }
                }
                """), """
                package com.example;

                import com.webforj.component.button.Button;
                import com.webforj.component.layout.flexlayout.FlexLayout;

                public class View {
                  private FlexLayout layout;
                  private Button save;
                  private Button button;

                  public View() {
                    layout = new FlexLayout();
                    save = new Button("Save");
                    save.setEnabled(false);
                    button = new Button("Button");
                    layout.add(save, button);
                  }
                }
                """),
        Arguments.of(Named.of("adds an argument to the creation that takes the children", """
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
            """), """
            package com.example;

            import com.webforj.component.button.Button;
            import com.webforj.component.layout.flexlayout.FlexLayout;

            public class View {
              public View() {
                Button save = new Button("Save");
                Button button = new Button("Button");
                Button cancel = new Button("Cancel");
                FlexLayout layout = new FlexLayout(save, button, cancel);
              }
            }
            """), Arguments
            .of(Named.of("adds an argument to the static factory that starts a builder chain", """
                package com.example;

                import com.webforj.component.button.Button;
                import com.webforj.component.layout.flexlayout.FlexLayout;

                public class View {
                  public View() {
                    Button save = new Button("Save");
                    FlexLayout layout = FlexLayout.create(save).vertical().build();
                  }
                }
                """), """
                package com.example;

                import com.webforj.component.button.Button;
                import com.webforj.component.layout.flexlayout.FlexLayout;

                public class View {
                  public View() {
                    Button save = new Button("Save");
                    Button button = new Button("Button");
                    FlexLayout layout = FlexLayout.create(save, button).vertical().build();
                  }
                }
                """));
  }

  private static Stream<Arguments> refusedSiblingCases() {
    return Stream.of(
        Arguments.of(Named.of("refuses a sibling that is attached inside a branch", """
            package com.example;

            import com.webforj.component.button.Button;
            import com.webforj.component.layout.flexlayout.FlexLayout;

            public class View {
              public View(boolean admin) {
                FlexLayout layout = new FlexLayout();
                Button save = new Button("Save");
                if (admin) {
                  layout.add(save);
                }
              }
            }
            """), "save",
            "The attach call of Button save at line 11 sits inside a branch, a loop or a lambda"),
        Arguments.of(Named.of("refuses a sibling that is attached in more than one place", """
            package com.example;

            import com.webforj.component.button.Button;
            import com.webforj.component.layout.flexlayout.FlexLayout;

            public class View {
              public View(boolean wide) {
                FlexLayout layout = new FlexLayout();
                Button save = new Button("Save");
                if (wide) {
                  layout.add(save);
                } else {
                  layout.add(save);
                }
              }
            }
            """), "save",
            "Button save is attached more than once to FlexLayout layout with [add] in View.java"),
        Arguments.of(Named.of("refuses a sibling the file does not declare", """
            package com.example;

            import com.webforj.component.button.Button;
            import com.webforj.component.layout.flexlayout.FlexLayout;

            public class View {
              public View() {
                FlexLayout layout = new FlexLayout();
                Button save = new Button("Save");
                layout.add(save);
              }
            }
            """), "missing", "No declaration of Button missing found in View.java"),
        Arguments.of(Named.of("ignores a call that goes through an is accessor of the parent", """
            package com.example;

            import com.webforj.component.button.Button;
            import com.webforj.component.layout.flexlayout.FlexLayout;

            public class View {
              public View() {
                FlexLayout layout = new FlexLayout();
                Button save = new Button("Save");
                layout.isVisible().add(save);
              }
            }
            """), "save",
            "Button save is not attached to FlexLayout layout with [add] in View.java"));
  }
}
