package com.webforj.devtools.craftforj.source.structure;

import static com.webforj.devtools.craftforj.source.structure.StructureFixture.TEXT_FIELD;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.point;
import static com.webforj.devtools.craftforj.source.structure.StructureFixture.variable;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.devtools.craftforj.source.model.SourceLocation;
import java.io.IOException;
import java.nio.file.Path;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Named;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

@DisplayName("StructureModifier attach call")
class StructureModifierAttachCallTest {

  private static final String PREFIX = "setPrefixComponent";

  private final StructureModifier editor = StructureFixture.createEditor();
  private StructureFixture fixture;

  @TempDir
  Path tempDir;

  @BeforeEach
  void setUp() {
    fixture = new StructureFixture(tempDir);
  }

  private static SourceLocation composite(Path file) {
    return new SourceLocation(file.toString(), null, "com.example.Card", null, TEXT_FIELD);
  }

  @ParameterizedTest
  @MethodSource("callsOnTheParent")
  void shouldFindCallOnParent(String source) throws IOException {
    Path file = fixture.write("View.java", source);

    assertTrue(editor.hasAttachCall(point(variable(file, "field", TEXT_FIELD), PREFIX)));
  }

  @ParameterizedTest
  @MethodSource("callsNotOnTheParent")
  void shouldNotFindCallOnParent(String source) throws IOException {
    Path file = fixture.write("View.java", source);

    assertFalse(editor.hasAttachCall(point(variable(file, "field", TEXT_FIELD), PREFIX)));
  }

  @Test
  @DisplayName("finds a call on the bound component of a composite without an alias")
  void shouldFindCallOnBoundComponent() throws IOException {
    Path file = fixture.write("Card.java", """
        package com.example;

        import com.webforj.component.Composite;

        public class Card extends Composite<TextField> {
          public Card() {
            getBoundComponent().setPrefixComponent(new Icon("star", "tabler"));
          }
        }
        """);

    assertTrue(editor.hasAttachCall(point(composite(file), PREFIX)));
  }

  @Test
  @DisplayName("does not count another component's call in a composite without an alias")
  void shouldIgnoreSiblingCallInComposite() throws IOException {
    Path file = fixture.write("Card.java", """
        package com.example;

        import com.webforj.component.Composite;

        public class Card extends Composite<TextField> {
          public Card() {
            TextField other = new TextField("Other");
            other.setPrefixComponent(new Icon("star", "tabler"));
          }
        }
        """);

    assertFalse(editor.hasAttachCall(point(composite(file), PREFIX)));
  }

  @Test
  @DisplayName("finds a call through the alias a composite keeps for its bound component")
  void shouldFindCallThroughAlias() throws IOException {
    Path file = fixture.write("Card.java", """
        package com.example;

        import com.webforj.component.Composite;

        public class Card extends Composite<TextField> {
          private final TextField self = getBoundComponent();

          public Card() {
            self.setPrefixComponent(new Icon("star", "tabler"));
          }
        }
        """);

    assertTrue(editor.hasAttachCall(point(composite(file), PREFIX)));
  }

  @Test
  @DisplayName("finds an unscoped call when the view itself is the parent")
  void shouldFindUnscopedCallOnView() throws IOException {
    Path file = fixture.write("View.java", """
        package com.example;

        public class View extends TextField {
          public View() {
            setPrefixComponent(new Icon("star", "tabler"));
          }
        }
        """);

    assertTrue(editor.hasAttachCall(
        point(new SourceLocation(file.toString(), 3, "com.example.View", null, "com.example.View"),
            PREFIX)));
  }

  private static Stream<Arguments> callsOnTheParent() {
    return Stream.of(Arguments.of(Named.of("finds a call on the parent variable", """
        package com.example;

        public class View {
          public View() {
            TextField field = new TextField("Text");
            field.setPrefixComponent(new Icon("star", "tabler"));
          }
        }
        """)), Arguments.of(Named.of("finds a call on the parent field read through this", """
        package com.example;

        public class View {
          private final TextField field = new TextField("Text");

          public View() {
            this.field.setPrefixComponent(new Icon("star", "tabler"));
          }
        }
        """)), Arguments.of(Named.of("finds a call at the end of a fluent chain on the parent", """
        package com.example;

        public class View {
          public View() {
            TextField field = new TextField("Text");
            field.setLabel("Name").setPrefixComponent(new Icon("star", "tabler"));
          }
        }
        """)), Arguments.of(Named.of("finds a call chained on the creation of the parent", """
        package com.example;

        public class View {
          public View() {
            TextField field = new TextField("Text").setPrefixComponent(new Icon("star", "tabler"));
          }
        }
        """)));
  }

  private static Stream<Arguments> callsNotOnTheParent() {
    return Stream.of(
        Arguments.of(Named.of("does not count the call of another component in the same file", """
            package com.example;

            public class View {
              public View() {
                TextField other = new TextField("Other");
                other.setPrefixComponent(new Icon("star", "tabler"));
                TextField field = new TextField("Text");
              }
            }
            """)),
        Arguments.of(Named.of("does not count a call reached through an accessor of the parent", """
            package com.example;

            public class View {
              public View() {
                TextField field = new TextField("Text");
                field.getSuffixComponent().setPrefixComponent(new Icon("star", "tabler"));
              }
            }
            """)), Arguments.of(Named.of("finds no call when the parent calls another method", """
            package com.example;

            public class View {
              public View() {
                TextField field = new TextField("Text");
                field.setSuffixComponent(new Icon("star", "tabler"));
              }
            }
            """)));
  }
}
