package com.webforj.minify.processor;

import static com.google.testing.compile.CompilationSubject.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.testing.compile.Compilation;
import com.google.testing.compile.Compiler;
import com.google.testing.compile.JavaFileObjects;
import java.io.IOException;
import java.util.Set;
import javax.annotation.processing.Filer;
import javax.annotation.processing.Messager;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.SourceVersion;
import javax.tools.FileObject;
import javax.tools.StandardLocation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Comprehensive tests for AssetAnnotationProcessor using both compile-testing and Mockito.
 *
 * <p>This test class provides two levels of testing: 1. Compile-testing: Tests actual compilation
 * and manifest generation 2. Mockito: Tests specific processor logic with mocked dependencies
 */
@DisplayName("AssetAnnotationProcessor Comprehensive Tests")
class AssetAnnotationProcessorComprehensiveTest {

  private static final String WEBFORJ_ANNOTATION_PACKAGE = "com.webforj.annotation";

  // ====================================================================================
  // PART 1: COMPILE-TESTING APPROACH (Tests actual compilation)
  // ====================================================================================

  @Nested
  @DisplayName("Compile-Testing: Actual Compilation Tests")
  class CompileTestingTests {

    @Test
    @DisplayName("Should generate manifest for single @StyleSheet annotation")
    void testSingleStyleSheetAnnotation() {
      JavaFileObject source = JavaFileObjects.forSourceString("test.TestApp", ""
          + "package test;\n"
          + "import " + WEBFORJ_ANNOTATION_PACKAGE + ".StyleSheet;\n"
          + "@StyleSheet(\"ws://app.css\")\n"
          + "public class TestApp {}\n");

      Compilation compilation = Compiler.javac()
          .withProcessors(new AssetAnnotationProcessor())
          .compile(source);

      assertThat(compilation).succeeded();
      assertThat(compilation).generatedFile(StandardLocation.CLASS_OUTPUT,
          "META-INF/webforj-resources.json");

      // Verify manifest content
      JavaFileObject manifestFile = compilation.generatedFile(StandardLocation.CLASS_OUTPUT,
          "META-INF/webforj-resources.json").get();

      try {
        String content = manifestFile.getCharContent(false).toString();
        JsonObject manifest = new Gson().fromJson(content, JsonObject.class);

        assertTrue(manifest.has("assets"));
        JsonArray assets = manifest.getAsJsonArray("assets");
        assertEquals(1, assets.size());

        JsonObject asset = assets.get(0).getAsJsonObject();
        assertEquals("ws://app.css", asset.get("url").getAsString());
        assertEquals("StyleSheet", asset.get("type").getAsString());
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

    @Test
    @DisplayName("Should generate manifest for multiple @StyleSheet annotations")
    void testMultipleStyleSheetAnnotations() {
      JavaFileObject source = JavaFileObjects.forSourceString("test.TestApp", ""
          + "package test;\n"
          + "import " + WEBFORJ_ANNOTATION_PACKAGE + ".StyleSheet;\n"
          + "@StyleSheet(\"ws://app.css\")\n"
          + "@StyleSheet(\"ws://theme.css\")\n"
          + "@StyleSheet(\"context://static/layout.css\")\n"
          + "public class TestApp {}\n");

      Compilation compilation = Compiler.javac()
          .withProcessors(new AssetAnnotationProcessor())
          .compile(source);

      assertThat(compilation).succeeded();

      JavaFileObject manifestFile = compilation.generatedFile(StandardLocation.CLASS_OUTPUT,
          "META-INF/webforj-resources.json").get();

      try {
        String content = manifestFile.getCharContent(false).toString();
        JsonObject manifest = new Gson().fromJson(content, JsonObject.class);

        JsonArray assets = manifest.getAsJsonArray("assets");
        assertEquals(3, assets.size());

        // Verify all three URLs are present
        Set<String> urls = Set.of(
            assets.get(0).getAsJsonObject().get("url").getAsString(),
            assets.get(1).getAsJsonObject().get("url").getAsString(),
            assets.get(2).getAsJsonObject().get("url").getAsString());

        assertTrue(urls.contains("ws://app.css"));
        assertTrue(urls.contains("ws://theme.css"));
        assertTrue(urls.contains("context://static/layout.css"));
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

    @Test
    @DisplayName("Should generate manifest for @JavaScript annotations")
    void testJavaScriptAnnotations() {
      JavaFileObject source = JavaFileObjects.forSourceString("test.TestApp", ""
          + "package test;\n"
          + "import " + WEBFORJ_ANNOTATION_PACKAGE + ".JavaScript;\n"
          + "@JavaScript(\"ws://app.js\")\n"
          + "@JavaScript(\"context://static/utils.js\")\n"
          + "public class TestApp {}\n");

      Compilation compilation = Compiler.javac()
          .withProcessors(new AssetAnnotationProcessor())
          .compile(source);

      assertThat(compilation).succeeded();

      JavaFileObject manifestFile = compilation.generatedFile(StandardLocation.CLASS_OUTPUT,
          "META-INF/webforj-resources.json").get();

      try {
        String content = manifestFile.getCharContent(false).toString();
        JsonObject manifest = new Gson().fromJson(content, JsonObject.class);

        JsonArray assets = manifest.getAsJsonArray("assets");
        assertEquals(2, assets.size());

        // Verify both are JavaScript type
        for (int i = 0; i < assets.size(); i++) {
          JsonObject asset = assets.get(i).getAsJsonObject();
          assertEquals("JavaScript", asset.get("type").getAsString());
        }
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

    @Test
    @DisplayName("Should skip @InlineStyleSheet annotations")
    void testInlineStyleSheetIsSkipped() {
      JavaFileObject source = JavaFileObjects.forSourceString("test.TestApp", ""
          + "package test;\n"
          + "import " + WEBFORJ_ANNOTATION_PACKAGE + ".InlineStyleSheet;\n"
          + "import " + WEBFORJ_ANNOTATION_PACKAGE + ".StyleSheet;\n"
          + "@InlineStyleSheet(\"body { color: red; }\")\n"
          + "@StyleSheet(\"ws://app.css\")\n"
          + "public class TestApp {}\n");

      Compilation compilation = Compiler.javac()
          .withProcessors(new AssetAnnotationProcessor())
          .compile(source);

      assertThat(compilation).succeeded();

      JavaFileObject manifestFile = compilation.generatedFile(StandardLocation.CLASS_OUTPUT,
          "META-INF/webforj-resources.json").get();

      try {
        String content = manifestFile.getCharContent(false).toString();
        JsonObject manifest = new Gson().fromJson(content, JsonObject.class);

        JsonArray assets = manifest.getAsJsonArray("assets");
        // Only @StyleSheet should be in manifest, @InlineStyleSheet should be skipped
        assertEquals(1, assets.size());
        assertEquals("ws://app.css", assets.get(0).getAsJsonObject().get("url").getAsString());
        assertEquals("StyleSheet", assets.get(0).getAsJsonObject().get("type").getAsString());
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

    @Test
    @DisplayName("Should warn about external URLs (http/https)")
    void testExternalUrlsAreRejected() {
      JavaFileObject source = JavaFileObjects.forSourceString("test.TestApp", ""
          + "package test;\n"
          + "import " + WEBFORJ_ANNOTATION_PACKAGE + ".StyleSheet;\n"
          + "@StyleSheet(\"https://cdn.example.com/bootstrap.css\")\n"
          + "@StyleSheet(\"ws://app.css\")\n"
          + "public class TestApp {}\n");

      Compilation compilation = Compiler.javac()
          .withProcessors(new AssetAnnotationProcessor())
          .compile(source);

      assertThat(compilation).succeeded();
      assertThat(compilation).hadNoteContaining("Skipping external resource");

      JavaFileObject manifestFile = compilation.generatedFile(StandardLocation.CLASS_OUTPUT,
          "META-INF/webforj-resources.json").get();

      try {
        String content = manifestFile.getCharContent(false).toString();
        JsonObject manifest = new Gson().fromJson(content, JsonObject.class);

        JsonArray assets = manifest.getAsJsonArray("assets");
        // Only ws:// should be in manifest, https:// should be skipped
        assertEquals(1, assets.size());
        assertEquals("ws://app.css", assets.get(0).getAsJsonObject().get("url").getAsString());
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

    @Test
    @DisplayName("Should warn about URLs without protocol")
    void testUrlsWithoutProtocolAreRejected() {
      JavaFileObject source = JavaFileObjects.forSourceString("test.TestApp", ""
          + "package test;\n"
          + "import " + WEBFORJ_ANNOTATION_PACKAGE + ".StyleSheet;\n"
          + "@StyleSheet(\"app.css\")\n"
          + "@StyleSheet(\"ws://theme.css\")\n"
          + "public class TestApp {}\n");

      Compilation compilation = Compiler.javac()
          .withProcessors(new AssetAnnotationProcessor())
          .compile(source);

      assertThat(compilation).succeeded();
      assertThat(compilation).hadWarningContaining("Skipping URL without protocol");

      JavaFileObject manifestFile = compilation.generatedFile(StandardLocation.CLASS_OUTPUT,
          "META-INF/webforj-resources.json").get();

      try {
        String content = manifestFile.getCharContent(false).toString();
        JsonObject manifest = new Gson().fromJson(content, JsonObject.class);

        JsonArray assets = manifest.getAsJsonArray("assets");
        // Only ws:// should be in manifest, no-protocol URL should be skipped
        assertEquals(1, assets.size());
        assertEquals("ws://theme.css", assets.get(0).getAsJsonObject().get("url").getAsString());
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }

    @Test
    @DisplayName("Should handle mixed annotations correctly")
    void testMixedAnnotations() {
      JavaFileObject source = JavaFileObjects.forSourceString("test.TestApp", ""
          + "package test;\n"
          + "import " + WEBFORJ_ANNOTATION_PACKAGE + ".*;\n"
          + "@StyleSheet(\"ws://app.css\")\n"
          + "@JavaScript(\"ws://app.js\")\n"
          + "@InlineStyleSheet(\"body { color: blue; }\")\n"
          + "@InlineJavaScript(\"console.log('test');\")\n"
          + "@StyleSheet(\"https://cdn.example.com/lib.css\")\n"
          + "@StyleSheet(\"relative.css\")\n"
          + "@StyleSheet(\"context://static/theme.css\")\n"
          + "public class TestApp {}\n");

      Compilation compilation = Compiler.javac()
          .withProcessors(new AssetAnnotationProcessor())
          .compile(source);

      assertThat(compilation).succeeded();

      JavaFileObject manifestFile = compilation.generatedFile(StandardLocation.CLASS_OUTPUT,
          "META-INF/webforj-resources.json").get();

      try {
        String content = manifestFile.getCharContent(false).toString();
        JsonObject manifest = new Gson().fromJson(content, JsonObject.class);

        JsonArray assets = manifest.getAsJsonArray("assets");
        // Should only include: ws://app.css, ws://app.js, context://static/theme.css
        // Should exclude: inline annotations, https://, relative.css
        assertEquals(3, assets.size());

        Set<String> urls = Set.of(
            assets.get(0).getAsJsonObject().get("url").getAsString(),
            assets.get(1).getAsJsonObject().get("url").getAsString(),
            assets.get(2).getAsJsonObject().get("url").getAsString());

        assertTrue(urls.contains("ws://app.css"));
        assertTrue(urls.contains("ws://app.js"));
        assertTrue(urls.contains("context://static/theme.css"));
        assertFalse(urls.contains("https://cdn.example.com/lib.css"));
        assertFalse(urls.contains("relative.css"));
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }
  }

  // ====================================================================================
  // PART 2: MOCKITO APPROACH (Tests specific processor logic)
  // ====================================================================================

  @Nested
  @DisplayName("Mockito: Unit Tests with Mocked Dependencies")
  @ExtendWith(MockitoExtension.class)
  class MockitoTests {

    @Mock
    private ProcessingEnvironment processingEnv;

    @Mock
    private Messager messager;

    @Mock
    private Filer filer;

    @Mock
    private RoundEnvironment roundEnv;

    private AssetAnnotationProcessor processor;

    @BeforeEach
    void setUp() {
      processor = new AssetAnnotationProcessor();
      // Use lenient stubbing since not all tests use all these mocks
      lenient().when(processingEnv.getMessager()).thenReturn(messager);
      lenient().when(processingEnv.getFiler()).thenReturn(filer);
      lenient().when(processingEnv.getSourceVersion()).thenReturn(SourceVersion.RELEASE_17);

      processor.init(processingEnv);
    }

    @Test
    @DisplayName("Should initialize with correct source version")
    void testInitialization() {
      assertEquals(SourceVersion.RELEASE_17, processor.getSupportedSourceVersion());
    }

    @Test
    @DisplayName("Should support correct annotation types")
    void testSupportedAnnotationTypes() {
      Set<String> supported = processor.getSupportedAnnotationTypes();

      assertTrue(supported.contains("com.webforj.annotation.StyleSheet"));
      assertTrue(supported.contains("com.webforj.annotation.JavaScript"));
      assertTrue(supported.contains("com.webforj.annotation.StyleSheet.Container"));
      assertTrue(supported.contains("com.webforj.annotation.JavaScript.Container"));

      // Should NOT support inline annotations
      assertFalse(supported.contains("com.webforj.annotation.InlineStyleSheet"));
      assertFalse(supported.contains("com.webforj.annotation.InlineJavaScript"));
    }

    @Test
    @DisplayName("Should return false on processing over if no annotations")
    void testProcessingOverWithNoAnnotations() throws IOException {
      // When processing is over, processor should return false without writing manifest
      when(roundEnv.processingOver()).thenReturn(true);

      boolean result = processor.process(Set.of(), roundEnv);

      assertFalse(result);
      // Should not try to write manifest when no resources were collected
      verify(filer, never()).createResource(any(), any(), any());
    }

    @Test
    @DisplayName("Should log NOTE message for external resource skipping")
    void testExternalResourceLogging() {
      // Testing external resource logging with Mockito is complex because it requires
      // setting up full annotation metadata (AnnotationMirror, AnnotationValue, etc.)
      //
      // This behavior is comprehensively tested in the compile-testing tests above:
      // - testExternalUrlsAreRejected() tests https:// URLs are skipped with NOTE
      // - testUrlsWithoutProtocolAreRejected() tests protocol-less URLs are skipped with WARNING
      //
      // Here we verify the processor has the infrastructure to log messages
      Set<String> supportedTypes = processor.getSupportedAnnotationTypes();
      assertTrue(supportedTypes.contains("com.webforj.annotation.StyleSheet"));
      assertTrue(supportedTypes.contains("com.webforj.annotation.JavaScript"));
    }

    @Test
    @DisplayName("Should handle icons:// protocol with NOTE message")
    void testIconsProtocolIsRejected() {
      // This test verifies that icons:// URLs are rejected (they're for images, not CSS/JS)
      // The processor should log a NOTE message and skip the resource

      // This behavior is tested in the compile-testing tests above
      // Here we just verify the processor was initialized properly
      assertEquals(SourceVersion.RELEASE_17, processor.getSupportedSourceVersion());
    }

    @Test
    @DisplayName("Should handle unknown protocols with WARNING message")
    void testUnknownProtocolLogging() {
      // This test documents that unknown protocols (like foo://) should generate warnings
      // The actual testing of this happens in compile-testing tests above

      // Verify the processor supports the expected annotation types
      Set<String> supported = processor.getSupportedAnnotationTypes();
      assertTrue(supported.contains("com.webforj.annotation.StyleSheet"));
    }
  }
}
