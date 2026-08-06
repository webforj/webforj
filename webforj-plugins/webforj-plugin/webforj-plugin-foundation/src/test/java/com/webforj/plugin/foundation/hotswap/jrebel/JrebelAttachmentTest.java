package com.webforj.plugin.foundation.hotswap.jrebel;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class JrebelAttachmentTest {

  @Test
  void shouldAttachTheNativeLibraryThroughAgentpath(@TempDir Path tmp) throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));

    assertEquals(
        List.of("-agentpath:" + library.toAbsolutePath(), JrebelAttachment.TOOL_ARGUMENT,
            JrebelAttachment.LEVEL_ARGUMENT),
        JrebelAttachment.create().setPath(library).build().getArguments());
  }

  @Test
  void shouldAttachTheJarThroughJavaagent(@TempDir Path tmp) throws Exception {
    Path jar = Files.createFile(tmp.resolve("jrebel.jar"));

    assertEquals(
        List.of("-javaagent:" + jar.toAbsolutePath(), JrebelAttachment.TOOL_ARGUMENT,
            JrebelAttachment.LEVEL_ARGUMENT),
        JrebelAttachment.create().setPath(jar).build().getArguments());
  }

  @Test
  void shouldFailWhenTheAgentDoesNotExist(@TempDir Path tmp) {
    JrebelAttachment attachment =
        JrebelAttachment.create().setPath(tmp.resolve("missing.dylib")).build();

    assertThrows(IOException.class, attachment::getArguments);
  }
}
