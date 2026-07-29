package com.webforj.devtools.craftforj.inspector.source;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.contribution.layout.columnslayout.item.ColumnsLayoutItemSpanContribution;
import com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout.item.FlexItemAlignmentContribution;
import com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout.item.FlexItemGrowContribution;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.SourceLocation;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeResult;
import com.webforj.devtools.craftforj.inspector.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourcePathRegistry;
import com.webforj.devtools.craftforj.utilities.ComponentLocator;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

@DisplayName("SourceCodeModifier layout item changes")
class SourceCodeModifierItemTest {

  private static final String FLEX = "com.webforj.component.layout.flexlayout.FlexLayout";
  private static final String COLUMNS = "com.webforj.component.layout.columnslayout.ColumnsLayout";
  private static final String BUTTON = "com.webforj.component.button.Button";
  private static final String FLEX_ALIGNMENT =
      "com.webforj.component.layout.flexlayout.FlexAlignment";

  private FeatureHandlerRegistry registry;
  private SourceCodeModifier modifier;
  private MockedStatic<ComponentLocator> locatorMock;
  private MockedStatic<SourcePathRegistry> pathRegistryMock;

  @TempDir
  Path tempDir;

  @BeforeEach
  void setUp() {
    registry = mock(FeatureHandlerRegistry.class);
    modifier = new SourceCodeModifier(registry, new SourceParserService());

    locatorMock = mockStatic(ComponentLocator.class);
    locatorMock.when(() -> ComponentLocator.findById(anyString())).thenReturn(Optional.empty());
    pathRegistryMock = mockStatic(SourcePathRegistry.class);
    pathRegistryMock.when(() -> SourcePathRegistry.isRecorded(anyString())).thenReturn(true);

    when(registry.getHandler("FlexItemGrow"))
        .thenReturn(Optional.of(new FlexItemGrowContribution()));
    when(registry.getHandler("FlexItemAlignment"))
        .thenReturn(Optional.of(new FlexItemAlignmentContribution()));
    when(registry.getHandler("ColumnsLayoutItemSpan"))
        .thenReturn(Optional.of(new ColumnsLayoutItemSpanContribution()));

    FeatureHandler scalarHandler = mock(FeatureHandler.class);
    when(scalarHandler.getSourceMethodName(anyString()))
        .thenAnswer(inv -> "set" + inv.getArgument(0));
    when(scalarHandler.getSourceValue(any(FeatureProperty.class)))
        .thenAnswer(inv -> ((FeatureProperty) inv.getArgument(0)).getValue());
    when(registry.getHandler("HasText")).thenReturn(Optional.of(scalarHandler));
  }

  @AfterEach
  void tearDown() {
    locatorMock.close();
    pathRegistryMock.close();
  }

  private Path write(String name, String content) throws IOException {
    Path file = tempDir.resolve(name);
    Files.writeString(file, content);

    return file;
  }

  private ChangeRequest itemChange(FeatureProperty property, String childFile, int childLine,
      String childVar, String childType, String parentFile, int parentLine, String parentVar,
      String parentType) {
    SourceLocation child = new SourceLocation(childFile, childLine, null, childVar, childType);
    SourceLocation parent = new SourceLocation(parentFile, parentLine, null, parentVar, parentType);

    return new ChangeRequest("btn", property, child, "parent", parent);
  }

  private FeatureProperty grow(Object value) {
    return FeatureProperty.builder("Grow", "FlexItemGrow").decimal().value(value).build();
  }

  private FeatureProperty span(Object value) {
    return FeatureProperty.builder("Span", "ColumnsLayoutItemSpan").integer().value(value).build();
  }

  private FeatureProperty alignment(Object value) {
    return FeatureProperty.builder("Alignment", "FlexItemAlignment").value(value).build();
  }

  private List<ChangeResult> successful(List<ChangeResult> results) {
    return results.stream().filter(ChangeResult::isSuccess).toList();
  }

  private List<ChangeResult> failed(List<ChangeResult> results) {
    return results.stream().filter(r -> !r.isSuccess()).toList();
  }

  @Test
  @DisplayName("a: local-variable parent and child inserts the call after add")
  void localVariableParentAndChild() throws IOException {
    Path file = write("View.java", """
        package com.example;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.button.Button;
        public class View {
          public void build() {
            FlexLayout layout = new FlexLayout();
            Button btn = new Button("Hi");
            layout.add(btn);
          }
        }
        """);

    ChangeRequest change = itemChange(grow(2.0), file.toString(), 7, "btn", BUTTON, file.toString(),
        6, "layout", FLEX);

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, successful(results).size());
    String written = Files.readString(file);
    assertTrue(written.contains("layout.setItemGrow(2.0, btn);"));
    assertTrue(
        written.indexOf("layout.setItemGrow(2.0, btn)") > written.indexOf("layout.add(btn)"));
  }

  @Test
  @DisplayName("preview does not write, apply writes")
  void previewDoesNotWrite() throws IOException {
    String original = """
        package com.example;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.button.Button;
        public class View {
          public void build() {
            FlexLayout layout = new FlexLayout();
            Button btn = new Button("Hi");
            layout.add(btn);
          }
        }
        """;
    Path file = write("PreviewView.java", original);

    ChangeRequest change = itemChange(grow(2.0), file.toString(), 7, "btn", BUTTON, file.toString(),
        6, "layout", FLEX);

    List<ChangeResult> preview = modifier.preview(List.of(change));

    assertEquals(1, successful(preview).size());
    assertEquals(original, Files.readString(file));

    modifier.apply(List.of(change));

    assertTrue(Files.readString(file).contains("layout.setItemGrow(2.0, btn);"));
  }

  @Test
  @DisplayName("b: updates an existing call without duplicating")
  void updatesExistingCall() throws IOException {
    Path file = write("UpdateView.java", """
        package com.example;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.button.Button;
        public class View {
          public void build() {
            FlexLayout layout = new FlexLayout();
            Button btn = new Button("Hi");
            layout.add(btn);
            layout.setItemGrow(1.0, btn);
          }
        }
        """);

    ChangeRequest change = itemChange(grow(3.0), file.toString(), 7, "btn", BUTTON, file.toString(),
        6, "layout", FLEX);

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, successful(results).size());
    String written = Files.readString(file);
    assertTrue(written.contains("layout.setItemGrow(3.0, btn)"));
    assertFalse(written.contains("setItemGrow(1.0"));
    assertEquals(written.indexOf("setItemGrow"), written.lastIndexOf("setItemGrow"));
  }

  @Test
  @DisplayName("c: reset removes the existing call")
  void resetRemovesCall() throws IOException {
    Path file = write("ResetView.java", """
        package com.example;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.button.Button;
        public class View {
          public void build() {
            FlexLayout layout = new FlexLayout();
            Button btn = new Button("Hi");
            layout.add(btn);
            layout.setItemGrow(1.0, btn);
          }
        }
        """);

    ChangeRequest change = itemChange(grow(null), file.toString(), 7, "btn", BUTTON,
        file.toString(), 6, "layout", FLEX);

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, successful(results).size());
    assertFalse(Files.readString(file).contains("setItemGrow"));
  }

  @Test
  @DisplayName("d: a pre-existing setStyle line is left untouched")
  void leavesLegacyStyleUntouched() throws IOException {
    Path file = write("LegacyView.java", """
        package com.example;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.button.Button;
        public class View {
          public void build() {
            FlexLayout layout = new FlexLayout();
            Button btn = new Button("Hi");
            layout.add(btn);
            btn.setStyle("flex-grow", "1");
          }
        }
        """);

    ChangeRequest change = itemChange(grow(2.0), file.toString(), 7, "btn", BUTTON, file.toString(),
        6, "layout", FLEX);

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, successful(results).size());
    String written = Files.readString(file);
    assertTrue(written.contains("btn.setStyle(\"flex-grow\", \"1\")"));
    assertTrue(written.contains("layout.setItemGrow(2.0, btn)"));
  }

  @Test
  @DisplayName("e: field parent and child insert the call in the constructor")
  void fieldParentAndChild() throws IOException {
    Path file = write("FieldView.java", """
        package com.example;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.button.Button;
        public class View {
          private FlexLayout layout = new FlexLayout();
          private Button btn = new Button("Hi");
          public View() {
            layout.add(btn);
          }
        }
        """);

    ChangeRequest change = itemChange(grow(2.0), file.toString(), 6, "btn", BUTTON, file.toString(),
        5, "layout", FLEX);

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, successful(results).size());
    String written = Files.readString(file);
    assertTrue(written.contains("layout.setItemGrow(2.0, btn)"));
    assertTrue(written.indexOf("setItemGrow") > written.indexOf("public View()"));
  }

  @Test
  @DisplayName("f: bound-component parent scopes the call to getBoundComponent()")
  void boundComponentParent() throws IOException {
    Path file = write("MyView.java", """
        package com.example;
        import com.webforj.component.Composite;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.button.Button;
        public class MyView extends Composite<FlexLayout> {
          public MyView() {
            Button btn = new Button("Hi");
            getBoundComponent().add(btn);
          }
        }
        """);

    ChangeRequest change =
        itemChange(grow(2.0), file.toString(), 7, "btn", BUTTON, file.toString(), 5, null, FLEX);

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, successful(results).size());
    String written = Files.readString(file);
    assertTrue(written.contains("getBoundComponent().setItemGrow(2.0, btn)"));
  }

  @Test
  @DisplayName("g: columns span uses item-first and leaves a breakpoint call untouched")
  void columnsSpan() throws IOException {
    Path file = write("ColumnsView.java", """
        package com.example;
        import com.webforj.component.layout.columnslayout.ColumnsLayout;
        import com.webforj.component.button.Button;
        public class View {
          public void build() {
            ColumnsLayout layout = new ColumnsLayout();
            Button btn = new Button("Hi");
            layout.add(btn);
            layout.setSpan(btn, "medium", 4);
          }
        }
        """);

    ChangeRequest change = itemChange(span(2), file.toString(), 7, "btn", BUTTON, file.toString(),
        6, "layout", COLUMNS);

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, successful(results).size());
    String written = Files.readString(file);
    assertTrue(written.contains("layout.setSpan(btn, 2)"));
    assertTrue(written.contains("layout.setSpan(btn, \"medium\", 4)"));
  }

  @Test
  @DisplayName("h: enum value generates the field access and adds the import")
  void enumAlignment() throws IOException {
    Path file = write("EnumView.java", """
        package com.example;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.button.Button;
        public class View {
          public void build() {
            FlexLayout layout = new FlexLayout();
            Button btn = new Button("Hi");
            layout.add(btn);
          }
        }
        """);

    ChangeRequest change = itemChange(alignment(FLEX_ALIGNMENT + ".CENTER"), file.toString(), 7,
        "btn", BUTTON, file.toString(), 6, "layout", FLEX);

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, successful(results).size());
    String written = Files.readString(file);
    assertTrue(written.contains("layout.setItemAlignment(FlexAlignment.CENTER, btn)"));
    assertTrue(written.contains("import " + FLEX_ALIGNMENT + ";"));
  }

  @Test
  @DisplayName("i: item and parent in different files fail")
  void differentFilesFail() throws IOException {
    Path file = write("ParentView.java", """
        package com.example;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.button.Button;
        public class View {
          public void build() {
            FlexLayout layout = new FlexLayout();
            Button btn = new Button("Hi");
            layout.add(btn);
          }
        }
        """);

    ChangeRequest change = itemChange(grow(2.0), "/other/Other.java", 7, "btn", BUTTON,
        file.toString(), 6, "layout", FLEX);

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, failed(results).size());
    assertTrue(failed(results).get(0).getError().contains("same file"));
  }

  @Test
  @DisplayName("j: inline child is extracted to a variable the call references")
  void inlineChildExtraction() throws IOException {
    Path file = write("InlineView.java", """
        package com.example;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.button.Button;
        public class View {
          public void build() {
            FlexLayout layout = new FlexLayout();
            layout.add(new Button("Hi"));
          }
        }
        """);

    ChangeRequest change =
        itemChange(grow(2.0), file.toString(), 7, null, BUTTON, file.toString(), 6, "layout", FLEX);

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, successful(results).size());
    String written = Files.readString(file);
    assertTrue(written.contains("Button button = new Button(\"Hi\")"));
    assertTrue(written.contains("layout.setItemGrow(2.0, button)"));
  }

  @Test
  @DisplayName("k: a regular change and an item change for the same component both succeed")
  void mixedBatch() throws IOException {
    Path file = write("MixedView.java", """
        package com.example;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.button.Button;
        public class View {
          public void build() {
            FlexLayout layout = new FlexLayout();
            Button btn = new Button("Hi");
            layout.add(btn);
          }
        }
        """);

    FeatureProperty textProperty =
        FeatureProperty.builder("Text", "HasText").text().value("Label").build();
    SourceLocation textSource = new SourceLocation(file.toString(), 7, null, "btn", BUTTON);
    ChangeRequest textChange = new ChangeRequest("btn", textProperty, textSource);

    ChangeRequest itemChange = itemChange(grow(2.0), file.toString(), 7, "btn", BUTTON,
        file.toString(), 6, "layout", FLEX);

    List<ChangeResult> results = modifier.apply(List.of(textChange, itemChange));

    assertEquals(2, successful(results).size());
    String written = Files.readString(file);
    assertTrue(written.contains("btn.setText(\"Label\")"));
    assertTrue(written.contains("layout.setItemGrow(2.0, btn)"));
  }

  @Test
  @DisplayName("l: destroyed parent and child re-anchor by variable name and type")
  void destroyedParentAndChildReanchor() throws IOException {
    Path file = write("ShiftedView.java", """
        package com.example;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.button.Button;
        public class View {
          public void build() {
            FlexLayout layout = new FlexLayout();
            Button btn = new Button("Hi");
            layout.add(btn);
          }
        }
        """);

    ChangeRequest change = itemChange(grow(2.0), file.toString(), 9, "btn", BUTTON, file.toString(),
        10, "layout", FLEX);

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, successful(results).size());
    String written = Files.readString(file);
    assertTrue(written.contains("layout.setItemGrow(2.0, btn);"));
  }

  @Test
  @DisplayName("m: destroyed composite parent redirects the item call to the alias variable")
  void destroyedCompositeParentUsesAlias() throws IOException {
    Path file = write("CompositeView.java", """
        package com.example;
        import com.webforj.component.layout.flexlayout.FlexLayout;
        import com.webforj.component.button.Button;
        public class CompositeView extends Composite<FlexLayout> {
          private FlexLayout self = getBoundComponent();
          private Button btn = new Button("Hi");
          public CompositeView() {
            self.add(btn);
          }
        }
        """);

    ChangeRequest change = itemChange(grow(2.0), file.toString(), 6, "btn", BUTTON, file.toString(),
        4, null, "com.example.CompositeView");

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, successful(results).size());
    String written = Files.readString(file);
    assertTrue(written.contains("self.setItemGrow(2.0, btn);"));
  }
}
