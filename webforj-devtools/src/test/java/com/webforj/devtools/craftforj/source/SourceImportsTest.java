package com.webforj.devtools.craftforj.source;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("SourceImports")
class SourceImportsTest {

  @Test
  @DisplayName("should start empty")
  void shouldStartEmpty() {
    SourceImports imports = new SourceImports();

    assertTrue(imports.getRequired().isEmpty());
    assertTrue(imports.getCandidates().isEmpty());
    assertTrue(imports.getUsed().isEmpty());
  }

  @Test
  @DisplayName("should count required imports as managed and used")
  void shouldCountRequiredAsUsed() {
    SourceImports imports = new SourceImports();
    imports.getRequired().add("java.util.Map");

    assertEquals(Set.of("java.util.Map"), imports.getCandidates());
    assertEquals(Set.of("java.util.Map"), imports.getUsed());
  }

  @Test
  @DisplayName("should keep unused tracked imports out of the used set")
  void shouldSeparateUnusedTracked() {
    SourceImports imports = new SourceImports();
    imports.setTracked(List.of("a.One", "a.Two"), List.of("a.Two"));

    assertEquals(Set.of("a.One", "a.Two"), imports.getCandidates());
    assertEquals(Set.of("a.Two"), imports.getUsed());
  }

  @Test
  @DisplayName("should replace the tracked imports on every call")
  void shouldReplaceTracked() {
    SourceImports imports = new SourceImports();
    imports.setTracked(List.of("a.One"), List.of("a.One"));
    imports.setTracked(List.of("a.Two"), List.of());

    assertEquals(Set.of("a.Two"), imports.getCandidates());
    assertTrue(imports.getUsed().isEmpty());
  }

  @Test
  @DisplayName("should combine required and tracked imports")
  void shouldCombineRequiredAndTracked() {
    SourceImports imports = new SourceImports();
    imports.getRequired().add("java.util.Map");
    imports.setTracked(List.of("a.One"), List.of());

    assertEquals(Set.of("a.One", "java.util.Map"), imports.getCandidates());
    assertEquals(Set.of("java.util.Map"), imports.getUsed());
  }
}
