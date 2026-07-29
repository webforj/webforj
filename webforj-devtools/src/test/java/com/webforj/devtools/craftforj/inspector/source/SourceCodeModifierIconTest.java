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
import com.webforj.devtools.craftforj.inspector.contribution.content.IconContribution;
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

@DisplayName("SourceCodeModifier icon changes")
class SourceCodeModifierIconTest {

  private static final String ICON_TYPE = "com.webforj.component.icons." + "Icon";
  private static final String ICON_BUTTON_TYPE = "com.webforj.component.icons." + "IconButton";

  @TempDir
  Path tempDir;

  private FeatureHandlerRegistry registry;
  private SourceCodeModifier modifier;
  private MockedStatic<ComponentLocator> locatorMock;
  private MockedStatic<SourcePathRegistry> pathRegistryMock;

  @BeforeEach
  void setUp() {
    registry = mock(FeatureHandlerRegistry.class);
    modifier = new SourceCodeModifier(registry, new SourceParserService());
    locatorMock = mockStatic(ComponentLocator.class);
    locatorMock.when(() -> ComponentLocator.findById(anyString())).thenReturn(Optional.empty());
    pathRegistryMock = mockStatic(SourcePathRegistry.class);
    pathRegistryMock.when(() -> SourcePathRegistry.isRecorded(anyString())).thenReturn(true);

    when(registry.getHandler("Icon")).thenReturn(Optional.of(new IconContribution()));

    FeatureHandler scalarHandler = mock(FeatureHandler.class);
    when(scalarHandler.getSourceMethodName(anyString()))
        .thenAnswer(inv -> "set" + inv.getArgument(0));
    when(scalarHandler.getSourceValue(any(FeatureProperty.class)))
        .thenAnswer(inv -> ((FeatureProperty) inv.getArgument(0)).getValue());
    when(registry.getHandler("HasLabel")).thenReturn(Optional.of(scalarHandler));
  }

  @AfterEach
  void tearDown() {
    locatorMock.close();
    pathRegistryMock.close();
  }

  private Path createTestFile(String name, String content) throws IOException {
    Path file = tempDir.resolve(name);
    Files.writeString(file, content);

    return file;
  }

  private ChangeRequest iconChange(String value, Path file, int line, String componentType) {
    FeatureProperty property = FeatureProperty.builder("Icon", "Icon").icon().value(value).build();
    SourceLocation source = new SourceLocation(file.toString(), line, null, null, componentType);

    return new ChangeRequest("component-1", property, source);
  }

  @Test
  @DisplayName("rewrites a factory call passed as method argument")
  void shouldRewriteFactoryCallInMethodArgument() throws IOException {
    String code = """
        package com.example;

        public class MyView {
          public MyView() {
            btn.setPrefixComponent(FeatherIcon.BELL.create());
          }
        }
        """;
    Path file = createTestFile("MyView.java", code);

    List<ChangeResult> results =
        modifier.apply(List.of(iconChange("tabler:home", file, 5, ICON_TYPE)));

    assertTrue(results.get(0).isSuccess());
    String content = Files.readString(file);
    assertTrue(content.contains("btn.setPrefixComponent(TablerIcon.create(\"home\"));"));
    assertTrue(content.contains("import " + "com.webforj.component.icons.TablerIcon" + ";"));
  }

  @Test
  @DisplayName("rewrites a factory call passed as constructor argument")
  void shouldRewriteFactoryCallInConstructorArgument() throws IOException {
    String code = """
        package com.example;

        public class MainLayout {
          public MainLayout() {
            appNav.addItem(new AppNavItem("Inbox", InboxView.class, TablerIcon.create("inbox")));
          }
        }
        """;
    Path file = createTestFile("MainLayout.java", code);

    List<ChangeResult> results =
        modifier.apply(List.of(iconChange("feather:bell", file, 5, ICON_TYPE)));

    assertTrue(results.get(0).isSuccess());
    String content = Files.readString(file);
    assertTrue(
        content.contains("new AppNavItem(\"Inbox\", InboxView.class, FeatherIcon.BELL.create())"));
    assertTrue(content.contains("import " + "com.webforj.component.icons.FeatherIcon" + ";"));
  }

  @Test
  @DisplayName("rewrites a factory call in a field declaration")
  void shouldRewriteFactoryCallInField() throws IOException {
    String code = """
        package com.example;

        public class MyView {
          private final Icon icon = TablerIcon.create("home");
        }
        """;
    Path file = createTestFile("MyView.java", code);

    List<ChangeResult> results =
        modifier.apply(List.of(iconChange("dwc:calendar", file, 4, ICON_TYPE)));

    assertTrue(results.get(0).isSuccess());
    String content = Files.readString(file);
    assertTrue(content.contains("private final Icon icon = DwcIcon.CALENDAR.create();"));
    assertTrue(content.contains("import " + "com.webforj.component.icons.DwcIcon" + ";"));
  }

  @Test
  @DisplayName("rewrites the literals of a generic Icon creation")
  void shouldRewriteGenericCreationLiterals() throws IOException {
    String code = """
        package com.example;

        public class MyView {
          public MyView() {
            add(new Icon("home", "tabler"));
          }
        }
        """;
    Path file = createTestFile("MyView.java", code);

    List<ChangeResult> results =
        modifier.apply(List.of(iconChange("feather:bell", file, 5, ICON_TYPE)));

    assertTrue(results.get(0).isSuccess());
    assertTrue(Files.readString(file).contains("add(new Icon(\"bell\", \"feather\"));"));
  }

  @Test
  @DisplayName("rewrites the inner factory call of an IconButton creation")
  void shouldRewriteInnerFactoryCallOfIconButton() throws IOException {
    String code = """
        package com.example;

        public class MyView {
          public MyView() {
            add(new IconButton(TablerIcon.create("menu-2")));
          }
        }
        """;
    Path file = createTestFile("MyView.java", code);

    List<ChangeResult> results =
        modifier.apply(List.of(iconChange("feather:bell", file, 5, ICON_BUTTON_TYPE)));

    assertTrue(results.get(0).isSuccess());
    assertTrue(Files.readString(file).contains("add(new IconButton(FeatherIcon.BELL.create()));"));
  }

  @Test
  @DisplayName("applies an icon change together with a setter change")
  void shouldApplyIconAndSetterChangesTogether() throws IOException {
    String code = """
        package com.example;

        public class MyView {
          private final Icon icon = TablerIcon.create("home");

          public MyView() {
          }
        }
        """;
    Path file = createTestFile("MyView.java", code);

    FeatureProperty labelProperty =
        FeatureProperty.builder("Label", "HasLabel").text().value("Home icon").build();
    SourceLocation source = new SourceLocation(file.toString(), 4, null, "icon", ICON_TYPE);
    ChangeRequest labelChange = new ChangeRequest("component-1", labelProperty, source);

    List<ChangeResult> results =
        modifier.apply(List.of(iconChange("feather:bell", file, 4, ICON_TYPE), labelChange));

    assertTrue(results.stream().allMatch(ChangeResult::isSuccess));
    String content = Files.readString(file);
    assertTrue(content.contains("FeatherIcon.BELL.create()"));
    assertTrue(content.contains("setLabel(\"Home icon\")"));
  }

  @Test
  @DisplayName("fails when multiple icon expressions share the line")
  void shouldFailOnAmbiguousLine() throws IOException {
    String code = """
        package com.example;

        public class MyView {
          public MyView() {
            add(TablerIcon.create("a"), TablerIcon.create("b"));
          }
        }
        """;
    Path file = createTestFile("MyView.java", code);
    String original = Files.readString(file);

    List<ChangeResult> results =
        modifier.apply(List.of(iconChange("feather:bell", file, 5, ICON_TYPE)));

    assertFalse(results.get(0).isSuccess());
    assertTrue(results.get(0).getError().contains("Multiple icon expressions"));
    assertEquals(original, Files.readString(file));
  }

  @Test
  @DisplayName("fails when no icon expression exists at the line")
  void shouldFailWhenNoIconExpression() throws IOException {
    String code = """
        package com.example;

        public class MyView {
          public MyView() {
            btn.setText("Hello");
          }
        }
        """;
    Path file = createTestFile("MyView.java", code);

    List<ChangeResult> results =
        modifier.apply(List.of(iconChange("feather:bell", file, 5, ICON_TYPE)));

    assertFalse(results.get(0).isSuccess());
    assertTrue(results.get(0).getError().contains("No icon expression"));
  }

  @Test
  @DisplayName("preview validates without writing the file")
  void shouldPreviewWithoutWriting() throws IOException {
    String code = """
        package com.example;

        public class MyView {
          public MyView() {
            btn.setPrefixComponent(FeatherIcon.BELL.create());
          }
        }
        """;
    Path file = createTestFile("MyView.java", code);
    String original = Files.readString(file);

    List<ChangeResult> results =
        modifier.preview(List.of(iconChange("tabler:home", file, 5, ICON_TYPE)));

    assertTrue(results.get(0).isSuccess());
    assertEquals(original, Files.readString(file));
  }
}
