package com.webforj.devtools.craftforj.utilities;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.App;
import com.webforj.component.ComponentSourceRegistry;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.component.element.Element;
import com.webforj.component.window.Frame;
import com.webforj.devtools.craftforj.inspector.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourceFileResolver;
import com.webforj.devtools.craftforj.model.ComponentMeta;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class ComponentMapBuilderTest {

  private static final String CREATION_CLASS = "com.example.MyView";
  private static final String CREATION_FILE = "/project/src/main/java/com/example/MyView.java";

  private ComponentMeta buildSingleComponentMeta(Frame frame, Element element,
      SourceParserService parserService) {
    try (MockedStatic<App> appMock = mockStatic(App.class)) {
      appMock.when(App::getFrames).thenReturn(List.of(frame));

      ComponentMapBuilder builder = new ComponentMapBuilder(parserService);
      Map<String, List<ComponentMeta>> map = builder.buildComponentMap();

      List<ComponentMeta> stack = map.get("dwc-1");
      assertNotNull(stack);

      return stack.get(0);
    }
  }

  private Frame newFrame(Element element) {
    Frame frame = mock(Frame.class);
    when(frame.getComponentId()).thenReturn("frame-1");
    when(frame.getClientComponentId()).thenReturn(null);
    when(frame.getComponents()).thenReturn(List.of(element));

    return frame;
  }

  private Element newElement() {
    Element element = mock(Element.class);
    when(element.getComponentId()).thenReturn("id-1");
    when(element.getClientComponentId()).thenReturn("dwc-1");
    when(element.getComponents()).thenReturn(List.of());

    return element;
  }

  @Test
  @DisplayName("Should leave usageSource null when the chain has only the creation frame")
  void shouldLeaveUsageSourceNullWhenChainHasOnlyCreationFrame() {
    Element element = newElement();
    Frame frame = newFrame(element);
    SourceParserService parserService = mock(SourceParserService.class);

    SourcePoint creation = new SourcePoint(CREATION_CLASS, "MyView.java", 10);

    try (
        MockedStatic<ComponentSourceRegistry> registryMock =
            mockStatic(ComponentSourceRegistry.class);
        MockedStatic<SourceFileResolver> resolverMock = mockStatic(SourceFileResolver.class)) {
      registryMock.when(() -> ComponentSourceRegistry.getSourcePoint(element)).thenReturn(creation);
      registryMock.when(() -> ComponentSourceRegistry.getSourceChain(element))
          .thenReturn(List.of(creation));
      resolverMock.when(() -> SourceFileResolver.resolve(anyString(), any()))
          .thenReturn(CREATION_FILE);
      when(parserService.extractVariableName(any(), anyInt(), anySet())).thenReturn("view");

      ComponentMeta meta = buildSingleComponentMeta(frame, element, parserService);

      assertNotNull(meta.getSource());
      assertNull(meta.getUsageSource());
    }
  }

  @Test
  @DisplayName("Should populate usageSource with the first differing resolvable caller")
  void shouldPopulateUsageSourceWithFirstDifferingResolvableCaller() {
    Element element = newElement();
    Frame frame = newFrame(element);
    SourceParserService parserService = mock(SourceParserService.class);

    SourcePoint creation = new SourcePoint(CREATION_CLASS, "MyView.java", 10);
    String callerClass = "com.example.CallerView";
    String callerFile = "/project/src/main/java/com/example/CallerView.java";
    SourcePoint caller = new SourcePoint(callerClass, "CallerView.java", 42);

    try (
        MockedStatic<ComponentSourceRegistry> registryMock =
            mockStatic(ComponentSourceRegistry.class);
        MockedStatic<SourceFileResolver> resolverMock = mockStatic(SourceFileResolver.class)) {
      registryMock.when(() -> ComponentSourceRegistry.getSourcePoint(element)).thenReturn(creation);
      registryMock.when(() -> ComponentSourceRegistry.getSourceChain(element))
          .thenReturn(List.of(creation, caller));
      resolverMock
          .when(() -> SourceFileResolver.resolve(CREATION_CLASS, SourceFileResolver.ALL_EXTENSIONS))
          .thenReturn(CREATION_FILE);
      resolverMock
          .when(() -> SourceFileResolver.resolve(CREATION_CLASS, SourceFileResolver.JAVA_ONLY))
          .thenReturn(CREATION_FILE);
      resolverMock.when(() -> SourceFileResolver.resolve(callerClass, SourceFileResolver.JAVA_ONLY))
          .thenReturn(callerFile);
      when(parserService.extractVariableName(any(), anyInt(), anySet())).thenReturn("view");

      ComponentMeta meta = buildSingleComponentMeta(frame, element, parserService);

      assertNotNull(meta.getUsageSource());
      assertEquals(callerFile, meta.getUsageSource().getFile());
      assertEquals(42, meta.getUsageSource().getLine());
      assertEquals(callerClass, meta.getUsageSource().getDeclaringClass());
    }
  }

  @Test
  @DisplayName("Should leave usageSource null when every caller is jar-only")
  void shouldLeaveUsageSourceNullWhenCallersAreJarOnly() {
    Element element = newElement();
    Frame frame = newFrame(element);
    SourceParserService parserService = mock(SourceParserService.class);

    SourcePoint creation = new SourcePoint(CREATION_CLASS, "MyView.java", 10);
    SourcePoint jarCaller = new SourcePoint("com.vendor.SomeLib", "SomeLib.java", 7);

    try (
        MockedStatic<ComponentSourceRegistry> registryMock =
            mockStatic(ComponentSourceRegistry.class);
        MockedStatic<SourceFileResolver> resolverMock = mockStatic(SourceFileResolver.class)) {
      registryMock.when(() -> ComponentSourceRegistry.getSourcePoint(element)).thenReturn(creation);
      registryMock.when(() -> ComponentSourceRegistry.getSourceChain(element))
          .thenReturn(List.of(creation, jarCaller));
      resolverMock
          .when(() -> SourceFileResolver.resolve(CREATION_CLASS, SourceFileResolver.ALL_EXTENSIONS))
          .thenReturn(CREATION_FILE);
      resolverMock
          .when(() -> SourceFileResolver.resolve(CREATION_CLASS, SourceFileResolver.JAVA_ONLY))
          .thenReturn(CREATION_FILE);
      resolverMock
          .when(
              () -> SourceFileResolver.resolve("com.vendor.SomeLib", SourceFileResolver.JAVA_ONLY))
          .thenReturn(null);
      when(parserService.extractVariableName(any(), anyInt(), anySet())).thenReturn("view");

      ComponentMeta meta = buildSingleComponentMeta(frame, element, parserService);

      assertNull(meta.getUsageSource());
    }
  }

  @Test
  @DisplayName("Should skip same-file caller frames and use the first differing file")
  void shouldSkipSameFileCallerFrames() {
    Element element = newElement();
    Frame frame = newFrame(element);
    SourceParserService parserService = mock(SourceParserService.class);

    SourcePoint creation = new SourcePoint(CREATION_CLASS, "MyView.java", 10);
    String helperClass = "com.example.MyView$Inner";
    SourcePoint sameFileFrame = new SourcePoint(helperClass, "MyView.java", 20);
    String callerClass = "com.example.CallerView";
    String callerFile = "/project/src/main/java/com/example/CallerView.java";
    SourcePoint caller = new SourcePoint(callerClass, "CallerView.java", 42);

    try (
        MockedStatic<ComponentSourceRegistry> registryMock =
            mockStatic(ComponentSourceRegistry.class);
        MockedStatic<SourceFileResolver> resolverMock = mockStatic(SourceFileResolver.class)) {
      registryMock.when(() -> ComponentSourceRegistry.getSourcePoint(element)).thenReturn(creation);
      registryMock.when(() -> ComponentSourceRegistry.getSourceChain(element))
          .thenReturn(List.of(creation, sameFileFrame, caller));
      resolverMock
          .when(() -> SourceFileResolver.resolve(CREATION_CLASS, SourceFileResolver.ALL_EXTENSIONS))
          .thenReturn(CREATION_FILE);
      resolverMock
          .when(() -> SourceFileResolver.resolve(CREATION_CLASS, SourceFileResolver.JAVA_ONLY))
          .thenReturn(CREATION_FILE);
      resolverMock.when(() -> SourceFileResolver.resolve(helperClass, SourceFileResolver.JAVA_ONLY))
          .thenReturn(CREATION_FILE);
      resolverMock.when(() -> SourceFileResolver.resolve(callerClass, SourceFileResolver.JAVA_ONLY))
          .thenReturn(callerFile);
      when(parserService.extractVariableName(any(), anyInt(), anySet())).thenReturn("view");

      ComponentMeta meta = buildSingleComponentMeta(frame, element, parserService);

      assertNotNull(meta.getUsageSource());
      assertEquals(callerFile, meta.getUsageSource().getFile());
      assertEquals(callerClass, meta.getUsageSource().getDeclaringClass());
    }
  }

  @kotlin.Metadata
  static class KotlinView {
  }

  @Test
  @DisplayName("Should flag a component whose declaring class was compiled from Kotlin")
  void shouldFlagKotlinDeclaringClass() {
    Element element = newElement();
    Frame frame = newFrame(element);
    SourceParserService parserService = mock(SourceParserService.class);

    SourcePoint creation = new SourcePoint(KotlinView.class.getName(), "KotlinView.kt", 10);

    try (
        MockedStatic<ComponentSourceRegistry> registryMock =
            mockStatic(ComponentSourceRegistry.class);
        MockedStatic<SourceFileResolver> resolverMock = mockStatic(SourceFileResolver.class)) {
      registryMock.when(() -> ComponentSourceRegistry.getSourcePoint(element)).thenReturn(creation);
      registryMock.when(() -> ComponentSourceRegistry.getSourceChain(element))
          .thenReturn(List.of(creation));
      resolverMock
          .when(() -> SourceFileResolver.resolve(ComponentMapBuilderTest.class.getName(),
              SourceFileResolver.ALL_EXTENSIONS))
          .thenReturn("/project/src/main/kotlin/KotlinView.kt");

      ComponentMeta meta = buildSingleComponentMeta(frame, element, parserService);

      assertTrue(meta.isKotlin());
    }
  }

  @Test
  @DisplayName("Should leave a component declared in Java unflagged")
  void shouldLeaveJavaDeclaringClassUnflagged() {
    Element element = newElement();
    Frame frame = newFrame(element);
    SourceParserService parserService = mock(SourceParserService.class);

    SourcePoint creation = new SourcePoint(ComponentMapBuilderTest.class.getName(), "X.java", 10);

    try (
        MockedStatic<ComponentSourceRegistry> registryMock =
            mockStatic(ComponentSourceRegistry.class);
        MockedStatic<SourceFileResolver> resolverMock = mockStatic(SourceFileResolver.class)) {
      registryMock.when(() -> ComponentSourceRegistry.getSourcePoint(element)).thenReturn(creation);
      registryMock.when(() -> ComponentSourceRegistry.getSourceChain(element))
          .thenReturn(List.of(creation));
      resolverMock.when(() -> SourceFileResolver.resolve(anyString(), any()))
          .thenReturn(CREATION_FILE);
      when(parserService.extractVariableName(any(), anyInt(), anySet())).thenReturn("view");

      ComponentMeta meta = buildSingleComponentMeta(frame, element, parserService);

      assertFalse(meta.isKotlin());
    }
  }

  @Test
  @DisplayName("Should skip a DSL frame that resolves to no project file and declare on the next frame")
  void shouldDeclareOnFirstResolvableFrameBehindDsl() {
    Element element = newElement();
    Frame frame = newFrame(element);
    SourceParserService parserService = mock(SourceParserService.class);

    SourcePoint dsl = new SourcePoint("com.webforj.kotlin.dsl.ElementsKt", "Elements.kt", 5);
    SourcePoint declaring = new SourcePoint("com.example.DrawerHeader", "DrawerHeader.kt", 18);
    String declaringFile = "/project/src/main/kotlin/com/example/DrawerHeader.kt";

    try (
        MockedStatic<ComponentSourceRegistry> registryMock =
            mockStatic(ComponentSourceRegistry.class);
        MockedStatic<SourceFileResolver> resolverMock = mockStatic(SourceFileResolver.class)) {
      registryMock.when(() -> ComponentSourceRegistry.getSourceChain(element))
          .thenReturn(List.of(dsl, declaring));
      resolverMock.when(() -> SourceFileResolver.resolve("com.example.DrawerHeader",
          SourceFileResolver.ALL_EXTENSIONS)).thenReturn(declaringFile);

      ComponentMeta meta = buildSingleComponentMeta(frame, element, parserService);

      assertNotNull(meta.getSource());
      assertEquals(declaringFile, meta.getSource().getFile());
      assertEquals(18, meta.getSource().getLine());
      assertEquals("com.example.DrawerHeader", meta.getSource().getDeclaringClass());
      assertNull(meta.getUsageSource());
    }
  }

  @Test
  @DisplayName("Should resolve a lambda frame through its outer class")
  void shouldResolveLambdaFrameThroughOuterClass() {
    Element element = newElement();
    Frame frame = newFrame(element);
    SourceParserService parserService = mock(SourceParserService.class);

    SourcePoint lambda = new SourcePoint("com.example.DrawerHeader$init$1", "DrawerHeader.kt", 21);
    String declaringFile = "/project/src/main/kotlin/com/example/DrawerHeader.kt";

    try (
        MockedStatic<ComponentSourceRegistry> registryMock =
            mockStatic(ComponentSourceRegistry.class);
        MockedStatic<SourceFileResolver> resolverMock = mockStatic(SourceFileResolver.class)) {
      registryMock.when(() -> ComponentSourceRegistry.getSourceChain(element))
          .thenReturn(List.of(lambda));
      resolverMock.when(() -> SourceFileResolver.resolve("com.example.DrawerHeader",
          SourceFileResolver.ALL_EXTENSIONS)).thenReturn(declaringFile);

      ComponentMeta meta = buildSingleComponentMeta(frame, element, parserService);

      assertNotNull(meta.getSource());
      assertEquals(declaringFile, meta.getSource().getFile());
      assertEquals(21, meta.getSource().getLine());
      assertEquals("com.example.DrawerHeader", meta.getSource().getDeclaringClass());
    }
  }

  @Test
  @DisplayName("Should leave source null when no frame resolves")
  void shouldLeaveSourceNullWhenNoFrameResolves() {
    Element element = newElement();
    Frame frame = newFrame(element);
    SourceParserService parserService = mock(SourceParserService.class);

    try (
        MockedStatic<ComponentSourceRegistry> registryMock =
            mockStatic(ComponentSourceRegistry.class);
        MockedStatic<SourceFileResolver> resolverMock = mockStatic(SourceFileResolver.class)) {
      registryMock.when(() -> ComponentSourceRegistry.getSourceChain(element))
          .thenReturn(List.of(new SourcePoint("com.webforj.kotlin.dsl.ElementsKt", "E.kt", 5)));

      ComponentMeta meta = buildSingleComponentMeta(frame, element, parserService);

      assertNull(meta.getSource());
      assertNull(meta.getUsageSource());
    }
  }
}
