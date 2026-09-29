package com.webforj.devtools.craftforj.source.site;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.ComponentSourceRegistry.SourceFrame;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.support.RuntimeSourceFixture;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("CompiledSites")
class CompiledSitesTest {

  @TempDir
  Path directory;

  private RuntimeSourceFixture fixture;

  @BeforeEach
  void setUp() {
    fixture = new RuntimeSourceFixture(directory);
  }

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @Test
  @DisplayName("should find nothing for a frame that recorded no instruction")
  void shouldFindNothingWithoutInstruction() {
    SourceFrame frame =
        SourceFrame.builder().setSourcePoint(new SourcePoint("app.View", "View.java", 8)).build();

    assertTrue(CompiledSites.find(frame, 0).isEmpty());
  }

  @Test
  @DisplayName("should refuse a class the application cannot hand out")
  void shouldRefuseUnknownClass() {
    SourceFrame frame = createFrame("app.Missing", "Missing.java");

    assertEquals("The class app.Missing cannot be read",
        assertThrows(SourceModificationException.class, () -> CompiledSites.find(frame, 0))
            .getMessage());
  }

  @Test
  @DisplayName("should refuse a class whose file holds no class")
  void shouldRefuseBrokenClassFile() throws IOException {
    fixture.addSource("app.View", "package app;\n\npublic class View {}\n");
    fixture.compile();
    fixture.create("app.View");
    Files.write(directory.resolve("classes/app/View.class"), new byte[] {1, 2, 3, 4, 5, 6, 7, 8});
    SourceFrame frame = createFrame("app.View", "View.java");

    assertEquals("The class app.View cannot be read",
        assertThrows(SourceModificationException.class, () -> CompiledSites.find(frame, 0))
            .getMessage());
  }

  private static SourceFrame createFrame(String className, String fileName) {
    return SourceFrame.builder().setSourcePoint(new SourcePoint(className, fileName, 8))
        .setMethod("<init>", "()V").setBytecodeIndex(4).build();
  }
}
