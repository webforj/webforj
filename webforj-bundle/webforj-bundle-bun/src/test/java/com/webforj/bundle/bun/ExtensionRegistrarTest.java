package com.webforj.bundle.bun;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bundle.BundleIndex;
import com.webforj.bundle.bun.discovery.BundlePackageDeclaration;
import com.webforj.bundle.bun.plugin.ScssBundleRegistrar;
import com.webforj.bundle.bun.plugin.TailwindBundleRegistrar;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class ExtensionRegistrarTest {

  private static BundleContext context(Path tmp, Set<String> sourceExtensions) throws IOException {
    Path frontend = Files.createDirectories(tmp.resolve("frontend"));
    Path java = Files.createDirectories(tmp.resolve("src/main/java"));
    BundleContext context = new BundleContext();
    context.setFrontendPath(frontend);
    context.setGeneratedPath(frontend.resolve("generated"));
    context.setSourcePaths(List.of(java));
    context.setSourceExtensions(sourceExtensions);
    context.setLog(BundleLogger.system());

    return context;
  }

  private static BundleContext withExtensions(Set<String> sourceExtensions) {
    BundleContext context = new BundleContext();
    context.setSourceExtensions(sourceExtensions);

    return context;
  }

  @Test
  void scssTurnsOnWhenScssFileIsPresent() {
    ScssBundleRegistrar scss = new ScssBundleRegistrar();

    assertEquals("webforj-scss", scss.getId());
    assertTrue(scss.isEnabledByDefault(withExtensions(Set.of("scss"))));
    assertTrue(scss.isEnabledByDefault(withExtensions(Set.of("sass"))));
    assertFalse(scss.isEnabledByDefault(withExtensions(Set.of("ts"))));
  }

  @Test
  void scssAddsItsPackageAndPlugin(@TempDir Path tmp) throws IOException {
    BundleContext context = context(tmp, Set.of("scss"));

    new ScssBundleRegistrar().onWillBundle(context);

    assertEquals(List.of("sass"),
        context.getPackages().stream().map(BundlePackageDeclaration::getName).toList());
    assertEquals(1, context.getPlugins().size());
    assertEquals("webforj-scss", context.getPlugins().get(0).getId());
    String wrapper =
        new String(context.getPlugins().get(0).getWrapperContent(), StandardCharsets.UTF_8);
    assertTrue(wrapper.contains("webforj-scss"), "the wrapper module is read from the classpath");
  }

  @Test
  void tailwindIsOffByDefault() {
    assertEquals("webforj-tailwind", new TailwindBundleRegistrar().getId());
    assertFalse(new TailwindBundleRegistrar().isEnabledByDefault(withExtensions(Set.of("css"))));
  }

  @Test
  void tailwindGeneratesGlobalEntryAndShipsItsPlugin(@TempDir Path tmp) throws IOException {
    BundleContext context = context(tmp, Set.of());

    new TailwindBundleRegistrar().onWillBundle(context);

    Path css = context.getGeneratedPath().resolve("tailwind").resolve("tailwind.css");
    assertTrue(Files.exists(css), "the stylesheet entry is generated");
    String content = Files.readString(css);
    assertTrue(content.contains("@import \"tailwindcss/theme.css\";"), "the theme is imported");
    assertTrue(content.contains("@import \"tailwindcss/utilities.css\" source(none);"),
        "the utilities are imported unlayered with source detection off");
    assertFalse(content.contains("preflight"), "the preflight base reset is not emitted");

    assertEquals(Set.of("generated/tailwind/tailwind.css"),
        context.getBindings().get(BundleIndex.GLOBAL_KEY), "an unowned entry loads for every view");
    assertEquals(List.of("tailwindcss", "bun-plugin-tailwind"),
        context.getPackages().stream().map(BundlePackageDeclaration::getName).toList());
    assertEquals("webforj-tailwind", context.getPlugins().get(0).getId());
  }
}
