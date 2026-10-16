package com.webforj.bbjship.foundation.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.google.gson.Gson;
import java.util.List;
import org.junit.jupiter.api.Test;

class VersionResponseTest {

  @Test
  void shouldReadTheVersionAnswer() {
    String json = """
        {
          "version": "26.03-SNAPSHOT"
        }
        """;

    VersionResponse response = new Gson().fromJson(json, VersionResponse.class);

    assertEquals("26.03-SNAPSHOT", response.getVersion());
  }

  @Test
  void shouldAnswerNoJavaRuntimeWhenTheAnswerCarriesNone() {
    String json = """
        {
          "version": "26.02"
        }
        """;

    VersionResponse response = new Gson().fromJson(json, VersionResponse.class);

    assertEquals(List.of(), response.getJavaArguments());
  }

  @Test
  void shouldReadTheJavaRuntimeOfTheAnswer() {
    String json = """
        {
          "version": "26.03-SNAPSHOT",
          "javaArguments": [
            "-Xmx1024m",
            "-XX:VMOptionsFile=/opt/hotswap/hotswap.options"
          ],
          "pid": 4242
        }
        """;

    VersionResponse response = new Gson().fromJson(json, VersionResponse.class);

    assertEquals(List.of("-Xmx1024m", "-XX:VMOptionsFile=/opt/hotswap/hotswap.options"),
        response.getJavaArguments());
    List<String> arguments = response.getJavaArguments();
    assertThrows(UnsupportedOperationException.class, () -> arguments.add("-Xms512m"));
  }
}
