package com.webforj.bundle.bun;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bundle.BundleIndex;
import com.webforj.bundle.bun.discovery.BundleEntryDeclaration;
import com.webforj.bundle.bun.discovery.BundlePackageDeclaration;
import com.webforj.bundle.bun.plugin.BunPlugin;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;

class BundleContextTest {

  @Test
  void shouldRoundTripTheBuildLocationsAndState() {
    BundleContext context = new BundleContext();
    Set<URI> classpath = Set.of(URI.create("file:/classes/"));
    Path frontend = Path.of("/app/src/main/frontend");
    List<Path> sources = List.of(Path.of("/app/src/main/java"));
    Path generated = frontend.resolve("generated");
    Path output = Path.of("/app/target/classes/static/frontend");

    context.setClasspath(classpath);
    context.setFrontendPath(frontend);
    context.setSourcePaths(sources);
    context.setGeneratedPath(generated);
    context.setOutputPath(output);
    context.setProduction(true);
    context.setRebuild(true);

    assertEquals(classpath, context.getClasspath());
    assertEquals(frontend, context.getFrontendPath());
    assertEquals(sources, context.getSourcePaths());
    assertEquals(generated, context.getGeneratedPath());
    assertEquals(output, context.getOutputPath());
    assertTrue(context.isProduction());
    assertTrue(context.isRebuild());
  }

  @Test
  void shouldDefendSourcePathsAgainstNull() {
    BundleContext context = new BundleContext();

    context.setSourcePaths(null);

    assertTrue(context.getSourcePaths().isEmpty());
  }

  @Test
  void shouldRoundTripTheSourceExtensions() {
    BundleContext context = new BundleContext();

    context.setSourceExtensions(Set.of("scss", "vue"));

    assertEquals(Set.of("scss", "vue"), context.getSourceExtensions());
  }

  @Test
  void shouldDefendSourceExtensionsAgainstNull() {
    BundleContext context = new BundleContext();

    context.setSourceExtensions(null);

    assertTrue(context.getSourceExtensions().isEmpty());
  }

  @Test
  void shouldCarryAnAddedPluginToTheBuild() {
    BundleContext context = new BundleContext();

    context.addPlugin("webforj-scss", "export default () => ({});");

    assertEquals(1, context.getPlugins().size());
    BunPlugin plugin = context.getPlugins().get(0);
    assertEquals("webforj-scss", plugin.getId());
    assertEquals("webforj-scss/webforj-scss.mjs", plugin.getWrapper());
    assertEquals("export default () => ({});",
        new String(plugin.getWrapperContent(), StandardCharsets.UTF_8));
  }

  @Test
  void shouldExposeTheBuildLog() {
    BundleContext context = new BundleContext();
    BundleLogger log = BundleLogger.system();

    context.setLog(log);

    assertSame(log, context.getLog());
  }

  @Test
  void shouldAddPackageDeclarationsDirectly() {
    BundleContext context = new BundleContext();
    BundlePackageDeclaration declaration =
        new BundlePackageDeclaration().setName("lit").setVersion("^3.0.0").setDev(true);

    context.addPackage(declaration);

    assertEquals(List.of(declaration), context.getPackages());
  }

  @Test
  void shouldBindAnOwnedEntryToEachOwner() {
    BundleContext context = new BundleContext();
    BundleEntryDeclaration declaration =
        new BundleEntryDeclaration().setSource("react/card.tsx").setBuildPath("react/card.tsx")
            .addOwner("com.acme.CardView").addOwner("com.acme.PanelView");

    context.addEntry(declaration);

    assertEquals(1, context.getEntries().size());
    assertEquals(Set.of("react/card.tsx"), context.getBindings().get("com.acme.CardView"));
    assertEquals(Set.of("react/card.tsx"), context.getBindings().get("com.acme.PanelView"));
    assertFalse(context.getBindings().containsKey(BundleIndex.GLOBAL_KEY));
  }

  @Test
  void shouldBindAnUnownedEntryUnderTheGlobalKey() {
    BundleContext context = new BundleContext();
    BundleEntryDeclaration declaration = new BundleEntryDeclaration()
        .setSource("generated/theme/theme.css").setBuildPath("generated/theme/theme.css");

    context.addEntry(declaration);

    assertEquals(Set.of("generated/theme/theme.css"),
        context.getBindings().get(BundleIndex.GLOBAL_KEY));
  }

  @Test
  void shouldKeepTheFirstEntryForASourceAndMergeOwners() {
    BundleContext context = new BundleContext();
    context.addEntry(
        new BundleEntryDeclaration().setSource("react/card.tsx").addOwner("com.acme.CardView"));
    context.addEntry(
        new BundleEntryDeclaration().setSource("react/card.tsx").addOwner("com.acme.PanelView"));

    assertEquals(1, context.getEntries().size());
    assertEquals(Set.of("react/card.tsx"), context.getBindings().get("com.acme.CardView"));
    assertEquals(Set.of("react/card.tsx"), context.getBindings().get("com.acme.PanelView"));
  }
}
