package com.webforj.bundle.bun.discovery;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;

class ClasspathPackageScannerTest {

  @Test
  void shouldCarryDebugDeclaredEntrySourcesOnTheResult() {
    ClasspathPackageScanner.Result result =
        new ClasspathPackageScanner.Result().setDebugSources(Set.of("panel/panel.ts"));

    assertEquals(Set.of("panel/panel.ts"), result.getDebugSources());
  }

  @Test
  void shouldPreserveFirstOccurrenceOrder() {
    List<BundlePackageDeclaration> raw =
        List.of(new BundlePackageDeclaration().setName("react").setVersion("^19.0.0"),
            new BundlePackageDeclaration().setName("lit").setVersion("^3.0.0"));

    ClasspathPackageScanner.Result result = ClasspathPackageScanner.merge(raw);

    assertEquals(2, result.getPackages().size());
    assertEquals("react", result.getPackages().get(0).getName());
    assertEquals("lit", result.getPackages().get(1).getName());
    assertTrue(result.getWarnings().isEmpty());
  }

  @Test
  void shouldDeduplicateIdenticalDeclarations() {
    List<BundlePackageDeclaration> raw =
        List.of(new BundlePackageDeclaration().setName("react").setVersion("^19.0.0"),
            new BundlePackageDeclaration().setName("react").setVersion("^19.0.0"));

    ClasspathPackageScanner.Result result = ClasspathPackageScanner.merge(raw);

    assertEquals(1, result.getPackages().size());
    assertTrue(result.getWarnings().isEmpty());
  }

  @Test
  void shouldWarnOnConflictingVersions() {
    List<BundlePackageDeclaration> raw =
        List.of(new BundlePackageDeclaration().setName("react").setVersion("^19.0.0"),
            new BundlePackageDeclaration().setName("react").setVersion("^18.0.0"));

    ClasspathPackageScanner.Result result = ClasspathPackageScanner.merge(raw);

    assertEquals(1, result.getPackages().size());
    assertEquals("^19.0.0", result.getPackages().get(0).getVersion());
    assertEquals(1, result.getWarnings().size());

    String warning = result.getWarnings().get(0);
    assertTrue(warning.contains("react"));
    assertTrue(warning.contains("^19.0.0"));
    assertTrue(warning.contains("^18.0.0"));
  }
}
