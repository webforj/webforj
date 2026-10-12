package com.webforj.plugin.foundation.hotswap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileTime;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class ContainerOptionsFileTest {

  @TempDir
  Path temporary;

  @Test
  void shouldWriteOneLinePerArgumentAndQuoteTheLinesWithWhitespace() throws IOException {
    Path directory = temporary.resolve("shared");

    boolean changed =
        ContainerOptionsFile.write(directory, List.of("-XX:+AllowEnhancedClassRedefinition",
            "-javaagent:/home/dev with space/agent.jar", "-Dwebforj.hotswap.tool=jrebel"));

    assertFalse(changed);
    assertEquals("""
        -XX:+AllowEnhancedClassRedefinition
        "-javaagent:/home/dev with space/agent.jar"
        -Dwebforj.hotswap.tool=jrebel
        """, Files.readString(directory.resolve("hotswap.options")));
  }

  @Test
  void shouldRefuseALineHoldingADoubleQuote() {
    Path directory = temporary.resolve("shared");
    List<String> lines = List.of("-javaagent:/home/dev/agent\".jar");

    IOException failure =
        assertThrows(IOException.class, () -> ContainerOptionsFile.write(directory, lines));

    assertEquals("the options file cannot contain a double quote: -javaagent:/home/dev/agent\".jar",
        failure.getMessage());
    assertFalse(Files.exists(directory.resolve("hotswap.options")));
  }

  @Test
  void shouldKeepTheFileWhenTheContentIsUnchanged() throws IOException {
    Path directory = temporary.resolve("shared");
    Path file = directory.resolve("hotswap.options");
    ContainerOptionsFile.write(directory, List.of("-Dwebforj.hotswap.tool=jrebel"));
    Files.setLastModifiedTime(file, FileTime.fromMillis(0));

    boolean changed =
        ContainerOptionsFile.write(directory, List.of("-Dwebforj.hotswap.tool=jrebel"));

    assertFalse(changed);
    assertEquals(FileTime.fromMillis(0), Files.getLastModifiedTime(file));
    assertEquals("""
        -Dwebforj.hotswap.tool=jrebel
        """, Files.readString(file));
  }

  @Test
  void shouldRewriteTheFileWhenTheContentChanged() throws IOException {
    Path directory = temporary.resolve("shared");
    ContainerOptionsFile.write(directory, List.of("-Dwebforj.hotswap.tool=jrebel"));

    boolean changed =
        ContainerOptionsFile.write(directory, List.of("-Dwebforj.hotswap.tool=hotswapAgent"));

    assertTrue(changed);
    assertEquals("""
        -Dwebforj.hotswap.tool=hotswapAgent
        """, Files.readString(directory.resolve("hotswap.options")));
  }

  @Test
  void shouldReadTheLinesTheFileWasWrittenWith() throws IOException {
    Path directory = temporary.resolve("shared");
    List<String> lines = List.of("-XX:+AllowEnhancedClassRedefinition",
        "-javaagent:/home/dev with space/agent.jar", "-Dwebforj.hotswap.tool=jrebel");
    ContainerOptionsFile.write(directory, lines);
    Files.writeString(directory.resolve("hotswap.options"), "\n", StandardOpenOption.APPEND);

    List<String> read = ContainerOptionsFile.readLines(directory);

    assertEquals(lines, read);
  }

  @Test
  void shouldNameTheFileInsideTheDirectory() {
    Path directory = temporary.resolve("shared");

    Path file = ContainerOptionsFile.getFile(directory);

    assertEquals(directory.resolve("hotswap.options"), file);
  }
}
