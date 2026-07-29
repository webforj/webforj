package com.webforj.devtools.craftforj.inspector.source.parser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.ast.CompilationUnit;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("SourceParserService")
class SourceParserServiceTest {

  private SourceParserService service;

  @BeforeEach
  void setUp() {
    service = new SourceParserService();
  }

  @Nested
  @DisplayName("parse(String)")
  class ParseString {

    @Test
    @DisplayName("parses valid Java code")
    void shouldParseValidCode() {
      String code = """
          package com.example;
          public class Test {
            private String name;
          }
          """;

      Optional<CompilationUnit> result = service.parse(code);

      assertTrue(result.isPresent());
      assertEquals("com.example", result.get().getPackageDeclaration().get().getNameAsString());
    }

    @Test
    @DisplayName("parses Java 17 features")
    void shouldParseJava17Features() {
      String code = """
          package com.example;
          public record Person(String name, int age) {}
          """;

      Optional<CompilationUnit> result = service.parse(code);

      assertTrue(result.isPresent());
    }
  }

  @Nested
  @DisplayName("parse(Path)")
  class ParsePath {

    @TempDir
    Path tempDir;

    @Test
    @DisplayName("parses file from path")
    void shouldParseFile() throws IOException {
      Path file = tempDir.resolve("Test.java");
      Files.writeString(file, """
          package com.example;
          public class Test {}
          """);

      Optional<CompilationUnit> result = service.parse(file);

      assertTrue(result.isPresent());
    }

    @Test
    @DisplayName("throws IOException for non-existent file")
    void shouldThrowForNonExistentFile() {
      Path file = tempDir.resolve("NonExistent.java");

      assertThrows(IOException.class, () -> service.parse(file));
    }

    @Test
    @DisplayName("reuses the parse while the file is unchanged")
    void shouldReuseUnchangedFile() throws IOException {
      Path file = tempDir.resolve("Cached.java");
      Files.writeString(file, """
          package com.example;
          public class Cached {}
          """);

      CompilationUnit first = service.parse(file).orElseThrow();
      CompilationUnit second = service.parse(file).orElseThrow();

      assertSame(first, second);
    }

    @Test
    @DisplayName("re-parses after the file changes on disk")
    void shouldReparseChangedFile() throws IOException {
      Path file = tempDir.resolve("Changed.java");
      Files.writeString(file, """
          package com.example;
          public class Changed {}
          """);
      CompilationUnit first = service.parse(file).orElseThrow();

      Files.writeString(file, """
          package com.example;
          public class Changed {
            private String added;
          }
          """);
      Files.setLastModifiedTime(file, FileTime.fromMillis(System.currentTimeMillis() + 2000));

      CompilationUnit second = service.parse(file).orElseThrow();

      assertNotSame(first, second);
      assertTrue(service.print(second).contains("added"));
    }

    @Test
    @DisplayName("evicts the eldest file past the cache capacity")
    void shouldEvictEldestFile() throws IOException {
      Path first = tempDir.resolve("First.java");
      Files.writeString(first, """
          package com.example;
          public class First {}
          """);
      CompilationUnit firstParse = service.parse(first).orElseThrow();

      for (int i = 0; i < 32; i++) {
        Path other = tempDir.resolve("Other" + i + ".java");
        Files.writeString(other, "package com.example;\npublic class Other" + i + " {}\n");
        service.parse(other);
      }

      assertNotSame(firstParse, service.parse(first).orElseThrow());
    }
  }

  @Nested
  @DisplayName("parseWithLexicalPreservation(String)")
  class ParseWithLexicalPreservation {

    @Test
    @DisplayName("parses with lexical preservation enabled")
    void shouldParseWithLexicalPreservation() {
      String code = """
          package com.example;

          // Comment preserved
          public class Test {
            private String name;
          }
          """;

      Optional<CompilationUnit> result = service.parseWithLexicalPreservation(code);

      assertTrue(result.isPresent());
    }

  }

  @Nested
  @DisplayName("parseWithLexicalPreservation(Path)")
  class ParseWithLexicalPreservationPath {

    @TempDir
    Path tempDir;

    @Test
    @DisplayName("parses file with lexical preservation")
    void shouldParseFileWithLexicalPreservation() throws IOException {
      Path file = tempDir.resolve("Test.java");
      Files.writeString(file, """
          package com.example;
          public class Test {}
          """);

      Optional<CompilationUnit> result = service.parseWithLexicalPreservation(file);

      assertTrue(result.isPresent());
    }
  }

  @Nested
  @DisplayName("print")
  class Print {

    @Test
    @DisplayName("prints compilation unit preserving formatting")
    void shouldPrintPreservingFormatting() {
      String code = """
          package com.example;

          // Important comment
          public class Test {
              private String name;
          }
          """;

      Optional<CompilationUnit> cu = service.parseWithLexicalPreservation(code);
      assertTrue(cu.isPresent());

      String printed = service.print(cu.get());

      assertNotNull(printed);
      assertTrue(printed.contains("Important comment"));
      assertTrue(printed.contains("package com.example"));
    }
  }

  @Nested
  @DisplayName("extractVariableName(Path, int, Set<String>)")
  class ExtractVariableNameTyped {

    @TempDir
    Path tempDir;

    private Path writeButtonField() throws IOException {
      Path file = tempDir.resolve("Test.java");
      Files.writeString(file, """
          package com.example;
          public class Test {
            private Button button = new Button();
          }
          """);

      return file;
    }

    @Test
    @DisplayName("returns the variable name when the declaration matches the acceptable type")
    void shouldReturnNameWhenTypeMatches() throws IOException {
      Path file = writeButtonField();

      String name = service.extractVariableName(file, 3, Set.of("Button"));

      assertEquals("button", name);
    }

    @Test
    @DisplayName("returns null when the declaration does not match the acceptable type")
    void shouldReturnNullWhenTypeMismatches() throws IOException {
      Path file = writeButtonField();

      String name = service.extractVariableName(file, 3, Set.of("TextField"));

      assertNull(name);
    }

    @Test
    @DisplayName("skips the type check when the acceptable type set is empty")
    void shouldSkipCheckWhenTypeSetIsEmpty() throws IOException {
      Path file = writeButtonField();

      String name = service.extractVariableName(file, 3, Set.of());

      assertEquals("button", name);
    }

    @Test
    @DisplayName("does not cross-poison the cache across different type sets on the same line")
    void shouldNotCrossPoisonCacheAcrossTypeSets() throws IOException {
      Path file = writeButtonField();

      assertEquals("button", service.extractVariableName(file, 3, Set.of("Button")));
      assertNull(service.extractVariableName(file, 3, Set.of("TextField")));
      assertEquals("button", service.extractVariableName(file, 3, Set.of("Button")));
    }
  }
}
