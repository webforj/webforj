package com.webforj.devtools.craftforj.inspector.source;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.component.ComponentSourceRegistry;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.concern.HasText;
import com.webforj.devtools.craftforj.inspector.contribution.EnumConcernContribution;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.model.SourceLocation;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeResult;
import com.webforj.devtools.craftforj.inspector.source.model.FilePatch;
import com.webforj.devtools.craftforj.inspector.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourceFileResolver;
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
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

@DisplayName("SourceCodeModifier")
class SourceCodeModifierTest {

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

    FeatureHandler scalarHandler = mock(FeatureHandler.class);
    when(scalarHandler.getSourceMethodName(anyString()))
        .thenAnswer(inv -> "set" + inv.getArgument(0));
    when(scalarHandler.getSourceValue(any(FeatureProperty.class)))
        .thenAnswer(inv -> ((FeatureProperty) inv.getArgument(0)).getValue());
    when(registry.getHandler("HasText")).thenReturn(Optional.of(scalarHandler));
    when(registry.getHandler("HasVisible")).thenReturn(Optional.of(scalarHandler));
    when(registry.getHandler("HasMax")).thenReturn(Optional.of(scalarHandler));
  }

  @AfterEach
  void tearDown() {
    locatorMock.close();
    pathRegistryMock.close();
  }

  private ChangeRequest createChange(String componentId, String featureType, String propertyName,
      Object value, String file, Integer line, String componentType) {
    FeatureProperty property =
        FeatureProperty.builder(propertyName, featureType).text().value(value).build();
    SourceLocation source =
        (file != null || line != null) ? new SourceLocation(file, line, null, null, componentType)
            : null;

    return new ChangeRequest(componentId, property, source);
  }

  private ChangeRequest createChangeWithVarName(String componentId, String featureType,
      String propertyName, Object value, String file, Integer line, String varName,
      String componentType) {
    FeatureProperty property =
        FeatureProperty.builder(propertyName, featureType).text().value(value).build();
    SourceLocation source = new SourceLocation(file, line, null, varName, componentType);

    return new ChangeRequest(componentId, property, source);
  }

  private List<ChangeResult> successful(List<ChangeResult> results) {
    return results.stream().filter(ChangeResult::isSuccess).toList();
  }

  private List<ChangeResult> failed(List<ChangeResult> results) {
    return results.stream().filter(r -> !r.isSuccess()).toList();
  }

  @Test
  @DisplayName("creates instance with registry")
  void shouldCreateWithRegistry() {
    assertNotNull(modifier);
  }

  @Nested
  class Preview {

    @TempDir
    Path tempDir;

    private Path createTestFile(String name, String content) throws IOException {
      Path file = tempDir.resolve(name);
      Files.writeString(file, content);

      return file;
    }

    @Test
    @DisplayName("returns empty result for empty change list")
    void shouldReturnEmptyForEmptyList() {
      List<ChangeResult> results = modifier.preview(List.of());
      assertTrue(results.isEmpty());
    }

    @Test
    @DisplayName("fails when no source location info provided")
    void shouldFailWithoutSourceLocation() {
      ChangeRequest change = createChange("id", "HasText", "Text", "v", null, null, null);
      List<ChangeResult> results = modifier.preview(List.of(change));

      assertEquals(0, successful(results).size());
      assertEquals(1, failed(results).size());
      assertTrue(failed(results).get(0).getError().contains("not found"));
    }

    @Test
    @DisplayName("fails when source location is incomplete")
    void shouldFailWithIncompleteSourceLocation() {
      ChangeRequest c1 = createChange("id", "HasText", "Text", "v", null, 10, "View");
      ChangeRequest c2 = createChange("id", "HasText", "Text", "v", "/View.java", null, "View");

      assertEquals(1, failed(modifier.preview(List.of(c1))).size());
      assertEquals(1, failed(modifier.preview(List.of(c2))).size());
    }

    @Test
    @DisplayName("succeeds with complete source location")
    void shouldSucceedWithCompleteSourceLocation() throws IOException {
      Path file = createTestFile("FormView.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class FormView {
              private Button btn = new Button();
          }
          """);

      ChangeRequest change = createChangeWithVarName("id", "HasText", "Text", "Hello",
          file.toString(), 4, "btn", "Button");

      List<ChangeResult> results = modifier.preview(List.of(change));

      assertEquals(1, successful(results).size());
      assertEquals(file.toString(), successful(results).get(0).getSource().getFile());
    }

    @Test
    @DisplayName("returns property in result")
    void shouldReturnProperty() throws IOException {
      Path file = createTestFile("View.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class View {
              private Button btn = new Button();
          }
          """);

      ChangeRequest change = createChangeWithVarName("id", "HasText", "Text", "v", file.toString(),
          4, "btn", "Button");

      ChangeResult item = successful(modifier.preview(List.of(change))).get(0);

      assertEquals("HasText", item.getProperty().getFeatureType());
      assertEquals("Text", item.getProperty().getName());
      assertEquals("v", item.getProperty().getValue());
    }

    @Test
    @DisplayName("resolves source location from live component")
    void shouldResolveFromLiveComponent() throws IOException {
      Path file = createTestFile("TestView.java", """
          package com.example;
          public class TestView {
              private TestComponent btn = new TestComponent();
          }
          """);

      TestComponent component = mock(TestComponent.class);
      SourcePoint sourcePoint = new SourcePoint("com.example.TestView", "TestView.java", 3);

      locatorMock.when(() -> ComponentLocator.findById("live")).thenReturn(Optional.of(component));

      try (
          MockedStatic<ComponentSourceRegistry> regMock = mockStatic(ComponentSourceRegistry.class);
          MockedStatic<SourceFileResolver> resMock = mockStatic(SourceFileResolver.class)) {

        regMock.when(() -> ComponentSourceRegistry.getSourcePoint(component))
            .thenReturn(sourcePoint);
        resMock.when(() -> SourceFileResolver.resolve(eq("com.example.TestView"),
            eq(SourceFileResolver.JAVA_ONLY))).thenReturn(file.toString());

        ChangeRequest change = createChange("live", "HasText", "Text", "Hi", null, null, null);
        List<ChangeResult> results = modifier.preview(List.of(change));

        assertEquals(1, successful(results).size());
        assertEquals(file.toString(), successful(results).get(0).getSource().getFile());
      }
    }

    @Test
    @DisplayName("rejects a client fallback file the server never resolved")
    void shouldRejectUnrecordedFallbackFile() throws IOException {
      Path file = createTestFile("EvilView.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class EvilView {
              private Button btn = new Button();
          }
          """);

      pathRegistryMock.when(() -> SourcePathRegistry.isRecorded(anyString())).thenReturn(false);

      ChangeRequest change = createChangeWithVarName("id", "HasText", "Text", "Hello",
          file.toString(), 4, "btn", "Button");

      List<ChangeResult> results = modifier.preview(List.of(change));

      assertEquals(0, successful(results).size());
      assertEquals(1, failed(results).size());
    }

    @Test
    @DisplayName("fails when live component has no source point")
    void shouldFailWhenComponentHasNoSourcePoint() {
      TestComponent component = mock(TestComponent.class);
      locatorMock.when(() -> ComponentLocator.findById("no-src"))
          .thenReturn(Optional.of(component));

      try (MockedStatic<ComponentSourceRegistry> regMock =
          mockStatic(ComponentSourceRegistry.class)) {
        regMock.when(() -> ComponentSourceRegistry.getSourcePoint(component)).thenReturn(null);

        ChangeRequest change = createChange("no-src", "HasText", "Text", "v", null, null, null);
        List<ChangeResult> results = modifier.preview(List.of(change));

        assertEquals(1, failed(results).size());
      }
    }

    @Test
    @DisplayName("enum: succeeds with valid handler")
    void shouldSucceedWithValidEnumHandler() throws IOException {
      Path file = createTestFile("EnumView.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class EnumView {
              private Button btn = new Button();
          }
          """);

      EnumConcernContribution<?> enumHandler = mock(EnumConcernContribution.class);
      when(enumHandler.getSourceMethodName(anyString()))
          .thenAnswer(inv -> "set" + inv.getArgument(0));
      when(enumHandler.getSourceValue(any(FeatureProperty.class)))
          .thenAnswer(inv -> ((FeatureProperty) inv.getArgument(0)).getValue());
      when(registry.getHandler("HasTheme")).thenReturn(Optional.of(enumHandler));

      String fullyQualifiedValue = TestTheme.class.getName() + ".PRIMARY";
      ChangeRequest change = createChangeWithVarName("id", "HasTheme", "Theme", fullyQualifiedValue,
          file.toString(), 4, "btn", "Button");

      List<ChangeResult> results = modifier.preview(List.of(change));

      assertEquals(1, successful(results).size());
    }

    @Test
    @DisplayName("fails when no handler found for feature type")
    void shouldFailWhenNoHandlerFound() throws IOException {
      Path file = createTestFile("View.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class View {
              private Button btn = new Button();
          }
          """);

      ChangeRequest change = createChangeWithVarName("id", "UnknownFeature", "Prop", "value",
          file.toString(), 4, "btn", "Button");

      List<ChangeResult> results = modifier.preview(List.of(change));

      assertEquals(1, failed(results).size());
      assertTrue(failed(results).get(0).getError().contains("No handler found"));
    }

    @Test
    @DisplayName("reports success and failure separately for mixed batch")
    void shouldHandleMixedResults() throws IOException {
      Path file = createTestFile("V.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class V {
              private Button btn = new Button();
          }
          """);

      ChangeRequest valid = createChangeWithVarName("c1", "HasText", "Text", "v", file.toString(),
          4, "btn", "Button");
      ChangeRequest invalid = createChange("c2", "HasText", "Text", "v", null, null, null);

      List<ChangeResult> results = modifier.preview(List.of(valid, invalid));

      assertEquals(1, successful(results).size());
      assertEquals(1, failed(results).size());
    }
  }

  @Nested
  class Apply {

    @TempDir
    Path tempDir;

    @Test
    @DisplayName("returns empty result for empty change list")
    void shouldReturnEmptyForEmptyList() {
      List<ChangeResult> results = modifier.apply(List.of());
      assertTrue(results.isEmpty());
    }

    @Test
    @DisplayName("reports changes without source location as failures")
    void shouldSkipChangesWithoutSourceLocation() {
      ChangeRequest change = createChange("id", "HasText", "Text", "v", null, null, null);
      List<ChangeResult> results = modifier.apply(List.of(change));

      assertEquals(0, successful(results).size());
      assertEquals(1, failed(results).size());
      assertEquals("Source file not found", failed(results).get(0).getError());
    }

    @Test
    @DisplayName("writes multiline string values as text blocks")
    void shouldWriteMultilineValueAsTextBlock() throws IOException {
      Path file = createTestFile("MultilineView.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class MultilineView {
              private Button btn = new Button();
              public MultilineView() {}
          }
          """);

      ChangeRequest change = createChangeWithVarName("btn", "HasText", "Text",
          "first line\nsecond line\n", file.toString(), 4, "btn", "Button");

      List<ChangeResult> results = modifier.apply(List.of(change));

      assertEquals(1, successful(results).size());
      String written = Files.readString(file);
      assertTrue(written.contains("btn.setText(\"\"\""));
      assertTrue(written.contains("        first line\n        second line\n        \"\"\""));
    }

    @Test
    @DisplayName("inserts setter call after field declaration")
    void shouldInsertSetterAfterFieldDeclaration() throws IOException {
      Path file = createTestFile("FormView.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class FormView {
              private Button btn = new Button();
              public FormView() {}
          }
          """);

      ChangeRequest change = createChangeWithVarName("btn", "HasText", "Text", "Submit",
          file.toString(), 4, "btn", "Button");

      List<ChangeResult> results = modifier.apply(List.of(change));

      assertEquals(1, successful(results).size());
      assertTrue(Files.readString(file).contains("btn.setText"));
    }

    @Test
    @DisplayName("inserts setter call after local variable declaration")
    void shouldInsertSetterAfterLocalVariable() throws IOException {
      Path file = createTestFile("LocalView.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class LocalView {
              public void build() {
                  Button b = new Button();
              }
          }
          """);

      ChangeRequest change = createChangeWithVarName("b", "HasText", "Text", "Click",
          file.toString(), 5, "b", "Button");

      List<ChangeResult> results = modifier.apply(List.of(change));

      assertEquals(1, successful(results).size());
      assertTrue(Files.readString(file).contains("b.setText"));
    }

    @Test
    @DisplayName("groups multiple changes to same file in single write")
    void shouldGroupChangesToSameFile() throws IOException {
      Path file = createTestFile("MultiView.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class MultiView {
              private Button b1 = new Button();
              private Button b2 = new Button();
              public MultiView() {}
          }
          """);

      ChangeRequest c1 = createChangeWithVarName("b1", "HasText", "Text", "First", file.toString(),
          4, "b1", "Button");
      ChangeRequest c2 = createChangeWithVarName("b2", "HasText", "Text", "Second", file.toString(),
          5, "b2", "Button");

      List<ChangeResult> results = modifier.apply(List.of(c1, c2));

      assertEquals(2, successful(results).size());
    }

    @Test
    @DisplayName("fails when source file does not exist")
    void shouldFailWhenFileNotFound() {
      ChangeRequest change =
          createChange("id", "HasText", "Text", "v", "/nonexistent/View.java", 10, "View");

      List<ChangeResult> results = modifier.apply(List.of(change));

      assertEquals(1, failed(results).size());
    }

    @Test
    @DisplayName("fails when source file has syntax errors")
    void shouldFailWithSyntaxErrors() throws IOException {
      Path file = createTestFile("Broken.java", "package com.example not valid");

      ChangeRequest change =
          createChange("id", "HasText", "Text", "v", file.toString(), 1, "Broken");

      List<ChangeResult> results = modifier.apply(List.of(change));

      assertEquals(1, failed(results).size());
    }

    @Test
    @DisplayName("reports success and failure separately for mixed batch")
    void shouldHandleMixedResults() throws IOException {
      Path valid = createTestFile("Valid.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class Valid {
              private Button btn = new Button();
              public Valid() {}
          }
          """);

      ChangeRequest validChange = createChangeWithVarName("btn", "HasText", "Text", "OK",
          valid.toString(), 4, "btn", "Button");
      ChangeRequest invalidChange =
          createChange("x", "HasText", "Text", "v", "/missing/X.java", 1, "X");

      List<ChangeResult> results = modifier.apply(List.of(validChange, invalidChange));

      assertEquals(1, successful(results).size());
      assertEquals(1, failed(results).size());
    }

    @Test
    @DisplayName("isolates per-component failures in same file")
    void shouldIsolatePerComponentFailures() throws IOException {
      Path file = createTestFile("MixedView.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class MixedView {
              private Button btn1 = new Button();
              public MixedView() {}
          }
          """);

      // btn1 has valid source, should succeed
      ChangeRequest valid = createChangeWithVarName("btn1", "HasText", "Text", "OK",
          file.toString(), 4, "btn1", "Button");
      // btn2 points to same file but wrong line, should fail individually
      ChangeRequest invalid = createChangeWithVarName("btn2", "HasMax", "Max", "100",
          file.toString(), 99, null, "Slider");

      List<ChangeResult> results = modifier.apply(List.of(valid, invalid));

      assertEquals(1, successful(results).size());
      assertEquals("btn1", successful(results).get(0).getComponentId());
      assertEquals(1, failed(results).size());
      assertEquals("btn2", failed(results).get(0).getComponentId());
    }

    @Test
    @DisplayName("preserves comments in modified file")
    void shouldPreserveComments() throws IOException {
      Path file = createTestFile("Commented.java", """
          package com.example;
          import com.webforj.component.button.Button;
          // Class comment
          public class Commented {
              // Field comment
              private Button btn = new Button();
              public Commented() {}
          }
          """);

      ChangeRequest change = createChangeWithVarName("btn", "HasText", "Text", "v", file.toString(),
          6, "btn", "Button");

      modifier.apply(List.of(change));

      String modified = Files.readString(file);
      assertTrue(modified.contains("Class comment"));
      assertTrue(modified.contains("Field comment"));
    }

    private Path createTestFile(String name, String content) throws IOException {
      Path file = tempDir.resolve(name);
      Files.writeString(file, content);

      return file;
    }
  }

  @Nested
  class ComputedValueDetection {

    @TempDir
    Path tempDir;

    private Path createTestFile(String name, String content) throws IOException {
      Path file = tempDir.resolve(name);
      Files.writeString(file, content);

      return file;
    }

    @Test
    @DisplayName("reports the computed expression an update overwrites")
    void shouldReportComputedExpression() throws IOException {
      Path file = createTestFile("View.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class View {
              private Button btn = new Button();
              public View() {
                  btn.setText(getLabel());
              }
          }
          """);

      ChangeRequest change = createChangeWithVarName("id", "HasText", "Text", "Hello",
          file.toString(), 4, "btn", "Button");

      ChangeResult item = successful(modifier.preview(List.of(change))).get(0);

      assertEquals("getLabel()", item.getReplacedExpression());
    }

    @Test
    @DisplayName("stays silent when the overwritten argument is a literal")
    void shouldNotReportLiteralArgument() throws IOException {
      Path file = createTestFile("View.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class View {
              private Button btn = new Button();
              public View() {
                  btn.setText("Old");
              }
          }
          """);

      ChangeRequest change = createChangeWithVarName("id", "HasText", "Text", "Hello",
          file.toString(), 4, "btn", "Button");

      ChangeResult item = successful(modifier.preview(List.of(change))).get(0);

      assertNull(item.getReplacedExpression());
    }

    @Test
    @DisplayName("stays silent when the change inserts a new call")
    void shouldNotReportInsertedCall() throws IOException {
      Path file = createTestFile("View.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class View {
              private Button btn = new Button();
          }
          """);

      ChangeRequest change = createChangeWithVarName("id", "HasText", "Text", "Hello",
          file.toString(), 4, "btn", "Button");

      ChangeResult item = successful(modifier.preview(List.of(change))).get(0);

      assertNull(item.getReplacedExpression());
    }

    @Test
    @DisplayName("updates the this-qualified constructor call, never an unrelated method body")
    void shouldUpdateConstructorCallNotUnrelatedMethod() throws IOException {
      Path file = createTestFile("SignalCard.java", """
          package com.example;
          import com.webforj.component.html.elements.Span;
          public class SignalCard {
              private final Span value = new Span();
              public SignalCard(String label, String value) {
                  this.value.setText(value);
              }
              public SignalCard setValue(String text) {
                  value.setText(text);
                  return this;
              }
          }
          """);

      ChangeRequest change = createChangeWithVarName("id", "HasText", "Text", "65%",
          file.toString(), 4, "value", "Span");

      ChangeResult item = successful(modifier.apply(List.of(change))).get(0);
      String modified = Files.readString(file);

      assertTrue(modified.contains("this.value.setText(\"65%\");"));
      assertTrue(modified.contains("value.setText(text);"));
      assertEquals("value", item.getReplacedExpression());
    }

    @Test
    @DisplayName("reports the computed expression on a composite's bound alias")
    void shouldReportComputedExpressionOnCompositeAlias() throws IOException {
      Path file = createTestFile("TestView.java", """
          package com.example;
          public class TestView extends Composite<TestComponent> {
            private TestComponent self = getBoundComponent();
            public TestView() {
              self.setText(getLabel());
            }
          }
          """);

      ChangeRequest change = createChange("gone", "HasText", "Text", "Hello", file.toString(), 4,
          "com.example.TestView");

      ChangeResult item = successful(modifier.preview(List.of(change))).get(0);

      assertEquals("getLabel()", item.getReplacedExpression());
    }
  }

  @Nested
  class CompositeRedirect {

    @TempDir
    Path tempDir;

    private Path createTestFile(String name, String content) throws IOException {
      Path file = tempDir.resolve(name);
      Files.writeString(file, content);

      return file;
    }

    @Test
    @DisplayName("targets the getBoundComponent() alias variable for a composite's own feature")
    void shouldRedirectCompositeFeatureToBoundAliasVariable() throws IOException {
      Path file = createTestFile("TestView.java", """
          package com.example;
          public class TestView extends Composite<TestComponent> {
            private TestComponent self = getBoundComponent();
            public TestView() {
              self.setText("Old");
            }
          }
          """);

      TestView composite = mock(TestView.class);
      TestComponent bound = mock(TestComponent.class);
      locatorMock.when(() -> ComponentLocator.findById("view")).thenReturn(Optional.of(composite));

      try (
          MockedStatic<ComponentSourceRegistry> regMock = mockStatic(ComponentSourceRegistry.class);
          MockedStatic<SourceFileResolver> resMock = mockStatic(SourceFileResolver.class);
          MockedStatic<ComponentUtil> utilMock = mockStatic(ComponentUtil.class)) {

        regMock.when(() -> ComponentSourceRegistry.getSourcePoint(composite))
            .thenReturn(new SourcePoint("com.example.TestView", "TestView.java", 4));
        regMock.when(() -> ComponentSourceRegistry.getSourcePoint(bound))
            .thenReturn(new SourcePoint("com.example.TestView", "TestView.java", 3));
        resMock.when(() -> SourceFileResolver.resolve(eq("com.example.TestView"),
            eq(SourceFileResolver.JAVA_ONLY))).thenReturn(file.toString());
        utilMock.when(() -> ComponentUtil.getBoundComponent(composite)).thenReturn(bound);

        ChangeRequest change = createChange("view", "HasText", "Text", "Hello", null, null, null);
        List<ChangeResult> results = modifier.apply(List.of(change));

        assertEquals(1, successful(results).size());
        String modified = Files.readString(file);
        assertTrue(modified.contains("self.setText(\"Hello\");"));
        assertTrue(!modified.contains("Old"));
      }
    }

    @Test
    @DisplayName("targets the alias variable when the bound source point hits the constructor line")
    void shouldRedirectToAliasWhenBoundPointsAtConstructor() throws IOException {
      Path file = createTestFile("TestView.java", """
          package com.example;
          public class TestView extends Composite<TestComponent> {
            private final TestComponent self = getBoundComponent();

            /**
             * Creates the view.
             *
             * @param status the status to show
             */
            public TestView(String status) {
              self.setText("Old");
            }
          }
          """);

      TestView composite = mock(TestView.class);
      TestComponent bound = mock(TestComponent.class);
      locatorMock.when(() -> ComponentLocator.findById("view")).thenReturn(Optional.of(composite));

      try (
          MockedStatic<ComponentSourceRegistry> regMock = mockStatic(ComponentSourceRegistry.class);
          MockedStatic<SourceFileResolver> resMock = mockStatic(SourceFileResolver.class);
          MockedStatic<ComponentUtil> utilMock = mockStatic(ComponentUtil.class)) {

        // Composite creates the bound component inside super(), so both frames record the
        // constructor signature line, far below the alias field once javadoc sits between them
        regMock.when(() -> ComponentSourceRegistry.getSourcePoint(composite))
            .thenReturn(new SourcePoint("com.example.TestView", "TestView.java", 10));
        regMock.when(() -> ComponentSourceRegistry.getSourcePoint(bound))
            .thenReturn(new SourcePoint("com.example.TestView", "TestView.java", 10));
        resMock.when(() -> SourceFileResolver.resolve(eq("com.example.TestView"),
            eq(SourceFileResolver.JAVA_ONLY))).thenReturn(file.toString());
        utilMock.when(() -> ComponentUtil.getBoundComponent(composite)).thenReturn(bound);

        ChangeRequest change = createChange("view", "HasText", "Text", "Hello", null, null, null);
        List<ChangeResult> results = modifier.apply(List.of(change));

        assertEquals(1, successful(results).size());
        String modified = Files.readString(file);
        assertTrue(modified.contains("self.setText(\"Hello\");"));
        assertTrue(!modified.contains("Old"));
      }
    }

    @Test
    @DisplayName("keeps the bound-component strategy when no alias variable exists")
    void shouldKeepBoundComponentStrategyWithoutAlias() throws IOException {
      Path file = createTestFile("TestView.java", """
          package com.example;
          public class TestView extends Composite<TestComponent> {
            public TestView() {
              getBoundComponent().setText("Old");
            }
          }
          """);

      TestView composite = mock(TestView.class);
      TestComponent bound = mock(TestComponent.class);
      locatorMock.when(() -> ComponentLocator.findById("view")).thenReturn(Optional.of(composite));

      try (
          MockedStatic<ComponentSourceRegistry> regMock = mockStatic(ComponentSourceRegistry.class);
          MockedStatic<SourceFileResolver> resMock = mockStatic(SourceFileResolver.class);
          MockedStatic<ComponentUtil> utilMock = mockStatic(ComponentUtil.class)) {

        regMock.when(() -> ComponentSourceRegistry.getSourcePoint(composite))
            .thenReturn(new SourcePoint("com.example.TestView", "TestView.java", 3));
        regMock.when(() -> ComponentSourceRegistry.getSourcePoint(bound))
            .thenReturn(new SourcePoint("com.example.TestView", "TestView.java", 4));
        resMock.when(() -> SourceFileResolver.resolve(eq("com.example.TestView"),
            eq(SourceFileResolver.JAVA_ONLY))).thenReturn(file.toString());
        utilMock.when(() -> ComponentUtil.getBoundComponent(composite)).thenReturn(bound);

        ChangeRequest change = createChange("view", "HasText", "Text", "Hello", null, null, null);
        List<ChangeResult> results = modifier.apply(List.of(change));

        assertEquals(1, successful(results).size());
        String modified = Files.readString(file);
        assertTrue(modified.contains("getBoundComponent().setText(\"Hello\");"));
      }
    }
  }

  @Nested
  class DestroyedComponentReanchor {

    @TempDir
    Path tempDir;

    private Path createTestFile(String name, String content) throws IOException {
      Path file = tempDir.resolve(name);
      Files.writeString(file, content);

      return file;
    }

    @Test
    @DisplayName("redirects a destroyed composite's change to the bound-component alias variable")
    void shouldRedirectDestroyedCompositeToAliasVariable() throws IOException {
      Path file = createTestFile("TestView.java", """
          package com.example;
          public class TestView extends Composite<TestComponent> {
            private TestComponent self = getBoundComponent();
            public TestView() {
              self.setText("Old");
            }
          }
          """);

      ChangeRequest change = createChange("gone", "HasText", "Text", "Hello", file.toString(), 4,
          "com.example.TestView");
      List<ChangeResult> results = modifier.apply(List.of(change));

      assertEquals(1, successful(results).size());
      String modified = Files.readString(file);
      assertTrue(modified.contains("self.setText(\"Hello\");"));
      assertTrue(!modified.contains("Old"));
    }

    @Test
    @DisplayName("keeps the bound-component strategy for a destroyed composite without an alias")
    void shouldKeepBoundComponentStrategyForDestroyedComposite() throws IOException {
      Path file = createTestFile("TestView.java", """
          package com.example;
          public class TestView extends Composite<TestComponent> {
            public TestView() {
              getBoundComponent().setText("Old");
            }
          }
          """);

      ChangeRequest change = createChange("gone", "HasText", "Text", "Hello", file.toString(), 3,
          "com.example.TestView");
      List<ChangeResult> results = modifier.apply(List.of(change));

      assertEquals(1, successful(results).size());
      String modified = Files.readString(file);
      assertTrue(modified.contains("getBoundComponent().setText(\"Hello\");"));
    }

    @Test
    @DisplayName("re-anchors a shifted declaration by variable name and type")
    void shouldReanchorShiftedDeclaration() throws IOException {
      Path file = createTestFile("FormView.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class FormView {
              private Button btn = new Button();
              public FormView() {
                  System.out.println("init");
              }
          }
          """);

      ChangeRequest change = createChangeWithVarName("gone", "HasText", "Text", "Hello",
          file.toString(), 6, "btn", "com.webforj.component.button.Button");
      List<ChangeResult> results = modifier.apply(List.of(change));

      assertEquals(1, successful(results).size());
      String modified = Files.readString(file);
      assertTrue(modified.contains("btn.setText(\"Hello\");"));
    }

    @Test
    @DisplayName("refuses to guess between ambiguous matching declarations")
    void shouldNotGuessBetweenAmbiguousDeclarations() throws IOException {
      Path file = createTestFile("AmbiguousView.java", """
          package com.example;
          import com.webforj.component.button.Button;
          public class AmbiguousView {
              public void first() {
                  Button btn = new Button();
              }
              public void second() {
                  Button btn = new Button();
              }
          }
          """);

      ChangeRequest change = createChangeWithVarName("gone", "HasText", "Text", "Hello",
          file.toString(), 3, "btn", "com.webforj.component.button.Button");
      List<ChangeResult> results = modifier.apply(List.of(change));

      assertEquals(1, failed(results).size());
    }
  }

  @Nested
  class PreviewPatches {

    @TempDir
    Path tempDir;

    private static final String SOURCE = """
        package com.example;
        import com.webforj.component.button.Button;
        public class FormView {
            private Button btn = new Button();
            public FormView() {}
        }
        """;

    private Path createTestFile(String name, String content) throws IOException {
      Path file = tempDir.resolve(name);
      Files.writeString(file, content);

      return file;
    }

    @Test
    @DisplayName("returns the file before and after without writing it")
    void shouldReturnPatchWithoutWriting() throws IOException {
      Path file = createTestFile("FormView.java", SOURCE);
      ChangeRequest change = createChangeWithVarName("id", "HasText", "Text", "Hello",
          file.toString(), 4, "btn", "Button");

      List<FilePatch> patches = modifier.previewPatches(List.of(change));

      assertEquals(1, patches.size());
      assertEquals(file.toString(), patches.get(0).getFile());
      assertEquals(SOURCE, patches.get(0).getOriginal());
      assertTrue(patches.get(0).getPatched().contains("btn.setText(\"Hello\")"));
      assertEquals(SOURCE, Files.readString(file));
    }

    @Test
    @DisplayName("returns one patch per file for changes spread across files")
    void shouldReturnOnePatchPerFile() throws IOException {
      Path first = createTestFile("FirstView.java", SOURCE);
      Path second = createTestFile("SecondView.java", SOURCE);

      List<FilePatch> patches = modifier.previewPatches(List.of(
          createChangeWithVarName("a", "HasText", "Text", "One", first.toString(), 4, "btn",
              "Button"),
          createChangeWithVarName("b", "HasText", "Text", "Two", second.toString(), 4, "btn",
              "Button")));

      assertEquals(2, patches.size());
      assertTrue(patches.stream().anyMatch(patch -> patch.getFile().equals(first.toString())));
      assertTrue(patches.stream().anyMatch(patch -> patch.getFile().equals(second.toString())));
    }

    @Test
    @DisplayName("carries every change to the same file into a single patch")
    void shouldMergeChangesToTheSameFile() throws IOException {
      Path file = createTestFile("FormView.java", SOURCE);

      List<FilePatch> patches = modifier.previewPatches(List.of(
          createChangeWithVarName("id", "HasText", "Text", "Hello", file.toString(), 4, "btn",
              "Button"),
          createChangeWithVarName("id", "HasVisible", "Visible", false, file.toString(), 4, "btn",
              "Button")));

      assertEquals(1, patches.size());
      assertTrue(patches.get(0).getPatched().contains("setText(\"Hello\")"));
      assertTrue(patches.get(0).getPatched().contains("setVisible("));
    }

    @Test
    @DisplayName("returns nothing when the change cannot be placed")
    void shouldReturnNothingWhenChangeFails() {
      ChangeRequest change = createChange("id", "HasText", "Text", "v", null, null, null);

      assertTrue(modifier.previewPatches(List.of(change)).isEmpty());
    }

    @Test
    @DisplayName("returns nothing for an empty change list")
    void shouldReturnNothingForEmptyList() {
      assertTrue(modifier.previewPatches(List.of()).isEmpty());
    }
  }

  abstract static class TestComponent extends Component implements HasText<Component> {
  }

  abstract static class TestView extends Composite<TestComponent> {
  }

  enum TestTheme {
    PRIMARY, SECONDARY, DANGER
  }
}
