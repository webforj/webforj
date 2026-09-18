package com.webforj.devtools.craftforj.source;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("SourceFileEditor")
class SourceFileEditorTest {

  private static final String SOURCE = """
      package com.example;

      import java.util.List;

      public class MyView {

        // kept as written
        private   List<String> names;
      }
      """;

  private final SourceFileEditor editor = new SourceFileEditor(new SourceParserService());

  @TempDir
  Path tempDir;

  @Test
  @DisplayName("should write the edited file and keep untouched formatting")
  void shouldWriteEditedFile() throws IOException {
    Path file = createSource(SOURCE);

    FilePatch patch = editor.edit(file, new SourceImports(), false, cu -> {
      cu.getClassByName("MyView").ifPresent(type -> type.addAnnotation("Deprecated"));

      return true;
    });

    String written = Files.readString(file);
    assertEquals(SOURCE, patch.getOriginal());
    assertEquals(written, patch.getPatched());
    assertTrue(written.contains("@Deprecated"));
    assertTrue(written.contains("  private   List<String> names;"));
  }

  @Test
  @DisplayName("should leave the file alone on a dry run")
  void shouldNotWriteOnDryRun() throws IOException {
    Path file = createSource(SOURCE);

    FilePatch patch = editor.edit(file, new SourceImports(), true, cu -> {
      cu.getClassByName("MyView").ifPresent(type -> type.addAnnotation("Deprecated"));

      return true;
    });

    assertTrue(patch.getPatched().contains("@Deprecated"));
    assertEquals(SOURCE, Files.readString(file));
  }

  @Test
  @DisplayName("should report no patch when the edit changed nothing")
  void shouldReportNoPatchWhenUnchanged() throws IOException {
    Path file = createSource(SOURCE);

    FilePatch patch = editor.edit(file, new SourceImports(), false, cu -> false);

    assertNull(patch.getPatched());
    assertEquals(SOURCE, Files.readString(file));
  }

  @Test
  @DisplayName("should not write when the edit leaves the content the same")
  void shouldNotWriteWhenContentIsTheSame() throws IOException {
    Path file = createSource(SOURCE);
    FileTime before = FileTime.from(Instant.parse("2020-01-01T00:00:00Z"));
    Files.setLastModifiedTime(file, before);

    FilePatch patch = editor.edit(file, new SourceImports(), false, cu -> true);

    assertNull(patch.getPatched());
    assertEquals(before, Files.getLastModifiedTime(file));
  }

  @Test
  @DisplayName("should add required imports")
  void shouldAddRequiredImports() throws IOException {
    Path file = createSource(SOURCE);
    SourceImports imports = new SourceImports();
    imports.getRequired().add("java.util.Map");

    editor.edit(file, imports, false, cu -> true);

    assertTrue(Files.readString(file).contains(importLine("java.util.Map")));
  }

  @Test
  @DisplayName("should remove a tracked import the file no longer uses")
  void shouldRemoveUnusedTrackedImport() throws IOException {
    Path file = createSource(SOURCE);
    SourceImports imports = new SourceImports();

    editor.edit(file, imports, false, cu -> {
      cu.getClassByName("MyView").map(ClassOrInterfaceDeclaration::getFields)
          .ifPresent(fields -> fields.forEach(FieldDeclaration::remove));
      imports.setTracked(List.of("java.util.List"), List.of());

      return true;
    });

    assertFalse(Files.readString(file).contains(importLine("java.util.List")));
  }

  @Test
  @DisplayName("should wrap a statement the edit made too long")
  void shouldWrapLongStatement() throws IOException {
    Path file = createSource("""
        package com.example;

        public class MyView {

          public MyView() {
            init();
          }
        }
        """);

    editor.edit(file, new SourceImports(), false, cu -> {
      cu.findFirst(BlockStmt.class).ifPresent(block -> block.addStatement("layout.setSomething(\""
          + "a".repeat(40) + "\", \"" + "b".repeat(40) + "\", \"" + "c".repeat(20) + "\");"));

      return true;
    });

    assertTrue(Files.readString(file).lines().allMatch(line -> line.length() <= 100));
  }

  @Test
  @DisplayName("should fail when the parser yields no result")
  void shouldFailWhenUnparseable() throws IOException {
    Path file = createSource(SOURCE);
    SourceParserService parserService = mock(SourceParserService.class);
    when(parserService.parseWithLexicalPreservation(anyString())).thenReturn(Optional.empty());
    SourceFileEditor failing = new SourceFileEditor(parserService);
    SourceImports imports = new SourceImports();

    assertThrows(SourceModificationException.class,
        () -> failing.edit(file, imports, false, cu -> true));
    assertEquals(SOURCE, Files.readString(file));
  }

  @Test
  @DisplayName("should leave a file with a syntax error untouched")
  void shouldLeaveBrokenFileUntouched() throws IOException {
    String broken = """
        package com.example;

        public class MyView {
          private String name
        }
        """;
    Path file = createSource(broken);
    SourceImports imports = new SourceImports();

    assertThrows(SourceModificationException.class,
        () -> editor.edit(file, imports, false, cu -> true));
    assertEquals(broken, Files.readString(file));
  }

  @Test
  @DisplayName("should fail when the file does not exist")
  void shouldFailWhenMissing() {
    Path file = tempDir.resolve("Missing.java");
    SourceImports imports = new SourceImports();

    assertThrows(IOException.class, () -> editor.edit(file, imports, false, cu -> true));
  }

  @Test
  @DisplayName("should name the read step when the file cannot be read")
  void shouldNameReadFailure() {
    Path file = tempDir.resolve("Missing.java");
    SourceImports imports = new SourceImports();

    IOException error =
        assertThrows(IOException.class, () -> editor.edit(file, imports, false, cu -> true));

    assertEquals("Failed to read source file: " + file, error.getMessage());
  }

  @Test
  @DisplayName("should name the write step when the file cannot be written")
  void shouldNameWriteFailure() throws IOException {
    Path file = createSource(SOURCE);
    SourceImports imports = new SourceImports();
    imports.getRequired().add("java.util.Map");
    assertTrue(file.toFile().setWritable(false));

    try {
      IOException error =
          assertThrows(IOException.class, () -> editor.edit(file, imports, false, cu -> true));

      assertEquals("Failed to write source file: " + file, error.getMessage());
    } finally {
      file.toFile().setWritable(true);
    }
  }

  private String importLine(String qualifiedName) {
    return "import " + qualifiedName + ";";
  }

  private Path createSource(String content) throws IOException {
    Path file = tempDir.resolve("MyView.java");
    Files.writeString(file, content);

    return file;
  }
}
