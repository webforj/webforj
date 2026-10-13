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

  private static final String SHARED_FOLDER = "shared";
  private static final String JREBEL_TOOL_ARGUMENT = "-Dwebforj.hotswap.tool=jrebel";
  private static final String OPTIONS_FILE = "hotswap.options";

  @TempDir
  Path temporary;

  @Test
  void shouldWriteOneLinePerArgumentAndQuoteTheLinesWithWhitespace() throws IOException {
    Path directory = temporary.resolve(SHARED_FOLDER);

    boolean changed =
        ContainerOptionsFile.write(directory, List.of("-XX:+AllowEnhancedClassRedefinition",
            "-javaagent:/home/dev with space/agent.jar", JREBEL_TOOL_ARGUMENT));

    assertFalse(changed);
    assertEquals("""
        -XX:+AllowEnhancedClassRedefinition
        "-javaagent:/home/dev with space/agent.jar"
        -Dwebforj.hotswap.tool=jrebel
        """, Files.readString(directory.resolve(OPTIONS_FILE)));
  }

  @Test
  void shouldRefuseLineHoldingDoubleQuote() {
    Path directory = temporary.resolve(SHARED_FOLDER);
    List<String> lines = List.of("-javaagent:/home/dev/agent\".jar");

    IOException failure =
        assertThrows(IOException.class, () -> ContainerOptionsFile.write(directory, lines));

    assertEquals("the options file cannot contain a double quote: -javaagent:/home/dev/agent\".jar",
        failure.getMessage());
    assertFalse(Files.exists(directory.resolve(OPTIONS_FILE)));
  }

  @Test
  void shouldKeepTheFileWhenTheContentIsUnchanged() throws IOException {
    Path directory = temporary.resolve(SHARED_FOLDER);
    Path file = directory.resolve(OPTIONS_FILE);
    ContainerOptionsFile.write(directory, List.of(JREBEL_TOOL_ARGUMENT));
    Files.setLastModifiedTime(file, FileTime.fromMillis(0));

    boolean changed = ContainerOptionsFile.write(directory, List.of(JREBEL_TOOL_ARGUMENT));

    assertFalse(changed);
    assertEquals(FileTime.fromMillis(0), Files.getLastModifiedTime(file));
    assertEquals("""
        -Dwebforj.hotswap.tool=jrebel
        """, Files.readString(file));
  }

  @Test
  void shouldRewriteTheFileWhenTheContentChanged() throws IOException {
    Path directory = temporary.resolve(SHARED_FOLDER);
    ContainerOptionsFile.write(directory, List.of(JREBEL_TOOL_ARGUMENT));

    boolean changed =
        ContainerOptionsFile.write(directory, List.of("-Dwebforj.hotswap.tool=hotswapAgent"));

    assertTrue(changed);
    assertEquals("""
        -Dwebforj.hotswap.tool=hotswapAgent
        """, Files.readString(directory.resolve(OPTIONS_FILE)));
  }

  @Test
  void shouldReadTheLinesTheFileWasWrittenWith() throws IOException {
    Path directory = temporary.resolve(SHARED_FOLDER);
    List<String> lines = List.of("-XX:+AllowEnhancedClassRedefinition",
        "-javaagent:/home/dev with space/agent.jar", JREBEL_TOOL_ARGUMENT);
    ContainerOptionsFile.write(directory, lines);
    Files.writeString(directory.resolve(OPTIONS_FILE), "\n", StandardOpenOption.APPEND);

    List<String> read = ContainerOptionsFile.readLines(directory);

    assertEquals(lines, read);
  }

  @Test
  void shouldNameTheFileInsideTheDirectory() {
    Path directory = temporary.resolve(SHARED_FOLDER);

    Path file = ContainerOptionsFile.getFile(directory);

    assertEquals(directory.resolve(OPTIONS_FILE), file);
  }
}
