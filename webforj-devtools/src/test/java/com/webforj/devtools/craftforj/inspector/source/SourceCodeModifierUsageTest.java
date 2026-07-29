package com.webforj.devtools.craftforj.inspector.source;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.component.ComponentSourceRegistry;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.button.Button;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeResult;
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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

@DisplayName("SourceCodeModifier usage-site targeting")
class SourceCodeModifierUsageTest {

  private static final String EXPLORE_CLASS = "com.example.Explore";
  private static final String DASHBOARD_CLASS = "com.example.DashboardView";

  private static final String EXPLORE_SOURCE = """
      package com.example;

      public class Explore {
        public Explore(String message, String iconName, String ctaLabel) {
          Paragraph messageLabel = new Paragraph(message);
          Button cta = new Button(ctaLabel)
              .setTheme(ButtonTheme.PRIMARY);
          add(messageLabel, cta);
        }
      }
      """;

  private static final String DASHBOARD_SOURCE = """
      package com.example;

      public class DashboardView {
        public DashboardView() {
          add(new Explore("Your dashboard is empty", "layout-dashboard", "Create widget"));
        }
      }
      """;

  @TempDir
  Path tempDir;

  private SourceCodeModifier modifier;
  private Button component;
  private Path exploreFile;
  private Path dashboardFile;
  private MockedStatic<ComponentLocator> locatorMock;
  private MockedStatic<SourcePathRegistry> pathRegistryMock;
  private MockedStatic<ComponentSourceRegistry> sourceRegistryMock;
  private MockedStatic<SourceFileResolver> resolverMock;

  @BeforeEach
  void setUp() throws IOException {
    exploreFile = tempDir.resolve("Explore.java");
    dashboardFile = tempDir.resolve("DashboardView.java");
    Files.writeString(exploreFile, EXPLORE_SOURCE);
    Files.writeString(dashboardFile, DASHBOARD_SOURCE);

    FeatureHandlerRegistry registry = mock(FeatureHandlerRegistry.class);
    FeatureHandler handler = mock(FeatureHandler.class);
    when(handler.getSourceMethodName(anyString())).thenAnswer(inv -> "set" + inv.getArgument(0));
    when(handler.getSourceValue(any(FeatureProperty.class)))
        .thenAnswer(inv -> ((FeatureProperty) inv.getArgument(0)).getValue());
    when(registry.getHandler("HasText")).thenReturn(Optional.of(handler));

    modifier = new SourceCodeModifier(registry, new SourceParserService());
    component = mock(Button.class);

    locatorMock = mockStatic(ComponentLocator.class);
    locatorMock.when(() -> ComponentLocator.findById("cmp-1")).thenReturn(Optional.of(component));

    pathRegistryMock = mockStatic(SourcePathRegistry.class);
    pathRegistryMock.when(() -> SourcePathRegistry.isRecorded(anyString())).thenReturn(true);

    sourceRegistryMock = mockStatic(ComponentSourceRegistry.class);
    sourceRegistryMock.when(() -> ComponentSourceRegistry.getSourcePoint(component))
        .thenReturn(new SourcePoint(EXPLORE_CLASS, "Explore.java", 6));
    sourceRegistryMock.when(() -> ComponentSourceRegistry.getSourceChain(component))
        .thenReturn(List.of(new SourcePoint(EXPLORE_CLASS, "Explore.java", 6),
            new SourcePoint(DASHBOARD_CLASS, "DashboardView.java", 5)));

    resolverMock = mockStatic(SourceFileResolver.class);
    resolverMock.when(() -> SourceFileResolver.resolve(eq(EXPLORE_CLASS), any()))
        .thenReturn(exploreFile.toString());
    resolverMock.when(() -> SourceFileResolver.resolve(eq(DASHBOARD_CLASS), any()))
        .thenReturn(dashboardFile.toString());
  }

  @AfterEach
  void tearDown() {
    locatorMock.close();
    pathRegistryMock.close();
    sourceRegistryMock.close();
    resolverMock.close();
  }

  private ChangeRequest usageChange(Object originalValue, Object newValue) {
    FeatureProperty property = FeatureProperty.builder("Text", "HasText").text()
        .javaType(String.class).value(newValue).build();
    ChangeRequest change = new ChangeRequest("cmp-1", property, null);
    change.setTarget(ChangeRequest.TARGET_USAGE);
    change.setOriginalValue(originalValue);

    return change;
  }

  @Test
  @DisplayName("preview resolves to the usage site without writing")
  void shouldPreviewAtUsageSite() throws IOException {
    List<ChangeResult> results =
        modifier.preview(List.of(usageChange("Create widget", "New label")));

    assertEquals(1, results.size());
    assertTrue(results.get(0).isSuccess());
    assertEquals(ChangeRequest.TARGET_USAGE, results.get(0).getResolvedTarget());
    assertEquals(dashboardFile.toString(), results.get(0).getSource().getFile());
    assertEquals(5, results.get(0).getSource().getLine());
    assertEquals(DASHBOARD_SOURCE, Files.readString(dashboardFile));
    assertEquals(EXPLORE_SOURCE, Files.readString(exploreFile));
  }

  @Test
  @DisplayName("apply rewrites the usage argument and leaves the shared class untouched")
  void shouldApplyAtUsageSite() throws IOException {
    List<ChangeResult> results = modifier.apply(List.of(usageChange("Create widget", "New label")));

    assertEquals(1, results.size());
    assertTrue(results.get(0).isSuccess());
    assertEquals(ChangeRequest.TARGET_USAGE, results.get(0).getResolvedTarget());

    String dashboard = Files.readString(dashboardFile);
    assertTrue(dashboard
        .contains("new Explore(\"Your dashboard is empty\", \"layout-dashboard\", \"New label\")"));
    assertEquals(EXPLORE_SOURCE, Files.readString(exploreFile));
  }

  @Test
  @DisplayName("preview falls back to the definition when the property is not traceable")
  void shouldFallBackToDefinitionInPreview() throws IOException {
    Files.writeString(exploreFile,
        EXPLORE_SOURCE.replace("new Button(ctaLabel)", "new Button(compute(ctaLabel))"));

    List<ChangeResult> results =
        modifier.preview(List.of(usageChange("Create widget", "New label")));

    assertEquals(1, results.size());
    assertTrue(results.get(0).isSuccess());
    assertEquals(ChangeRequest.TARGET_DEFINITION, results.get(0).getResolvedTarget());
    assertEquals(exploreFile.toString(), results.get(0).getSource().getFile());
  }

  @Test
  @DisplayName("apply fails instead of silently editing the definition")
  void shouldFailApplyWhenNotTraceable() throws IOException {
    Files.writeString(exploreFile,
        EXPLORE_SOURCE.replace("new Button(ctaLabel)", "new Button(compute(ctaLabel))"));

    List<ChangeResult> results = modifier.apply(List.of(usageChange("Create widget", "New label")));

    assertEquals(1, results.size());
    assertFalse(results.get(0).isSuccess());
    assertTrue(results.get(0).getError().contains("usage site"));
    assertEquals(EXPLORE_SOURCE.replace("new Button(ctaLabel)", "new Button(compute(ctaLabel))"),
        Files.readString(exploreFile));
    assertEquals(DASHBOARD_SOURCE, Files.readString(dashboardFile));
  }

  @Test
  @DisplayName("apply fails when the usage argument changed since the preview")
  void shouldFailApplyOnStaleOriginalValue() throws IOException {
    List<ChangeResult> results = modifier.apply(List.of(usageChange("Stale value", "New label")));

    assertEquals(1, results.size());
    assertFalse(results.get(0).isSuccess());
    assertEquals(DASHBOARD_SOURCE, Files.readString(dashboardFile));
  }

  @Test
  @DisplayName("usage targeting requires an original value")
  void shouldRequireOriginalValue() {
    ChangeRequest change = usageChange(null, "New label");

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, results.size());
    assertFalse(results.get(0).isSuccess());
  }

  @Test
  @DisplayName("apply rewrites a computed usage argument of a field-initialized component")
  void shouldRewriteComputedUsageArgumentOfFieldInitializedComponent() throws IOException {
    String signalSource = """
        package com.example;

        public class SignalCard {
          private final Button value = new Button();

          public SignalCard(String label, String value) {
            this.value.setText(value);
          }
        }
        """;
    String boardSource = """
        package com.example;

        public class SignalBoardView {
          public SignalBoardView() {
            add(new SignalCard("Fleet utilisation", utilisation + "%"));
          }
        }
        """;
    Path signalFile = tempDir.resolve("SignalCard.java");
    Path boardFile = tempDir.resolve("SignalBoardView.java");
    Files.writeString(signalFile, signalSource);
    Files.writeString(boardFile, boardSource);

    sourceRegistryMock.when(() -> ComponentSourceRegistry.getSourcePoint(component))
        .thenReturn(new SourcePoint("com.example.SignalCard", "SignalCard.java", 4));
    sourceRegistryMock.when(() -> ComponentSourceRegistry.getSourceChain(component))
        .thenReturn(List.of(new SourcePoint("com.example.SignalCard", "SignalCard.java", 4),
            new SourcePoint("com.example.SignalBoardView", "SignalBoardView.java", 5)));
    resolverMock.when(() -> SourceFileResolver.resolve(eq("com.example.SignalCard"), any()))
        .thenReturn(signalFile.toString());
    resolverMock.when(() -> SourceFileResolver.resolve(eq("com.example.SignalBoardView"), any()))
        .thenReturn(boardFile.toString());

    List<ChangeResult> results = modifier.apply(List.of(usageChange("64%", "65%")));

    assertEquals(1, results.size());
    assertTrue(results.get(0).isSuccess());
    assertEquals(ChangeRequest.TARGET_USAGE, results.get(0).getResolvedTarget());
    assertEquals("utilisation + \"%\"", results.get(0).getReplacedExpression());
    assertTrue(
        Files.readString(boardFile).contains("new SignalCard(\"Fleet utilisation\", \"65%\")"));
    assertEquals(signalSource, Files.readString(signalFile));
  }

  @Test
  @DisplayName("definition-targeted changes keep the existing behavior")
  void shouldKeepDefinitionBehavior() throws IOException {
    FeatureProperty property = FeatureProperty.builder("Text", "HasText").text()
        .javaType(String.class).value("New label").build();
    ChangeRequest change = new ChangeRequest("cmp-1", property, null);

    List<ChangeResult> results = modifier.apply(List.of(change));

    assertEquals(1, results.size());
    assertTrue(results.get(0).isSuccess());
    assertTrue(Files.readString(exploreFile).contains("setText(\"New label\")"));
    assertEquals(DASHBOARD_SOURCE, Files.readString(dashboardFile));
  }
}
