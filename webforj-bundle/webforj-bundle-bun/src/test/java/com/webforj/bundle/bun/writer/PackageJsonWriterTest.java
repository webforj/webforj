package com.webforj.bundle.bun.writer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.webforj.bundle.bun.discovery.BundlePackageDeclaration;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class PackageJsonWriterTest {

  private final PackageJsonWriter writer = new PackageJsonWriter();

  @Test
  void shouldBuildFreshModelWithNamePrivateAndPinnedDependencies() {
    Map<String, Object> model = writer.createModel("my.project",
        List.of(new BundlePackageDeclaration().setName("react").setVersion("^19.0.0"),
            new BundlePackageDeclaration().setName("lit").setVersion("^3.0.0")),
        new LinkedHashMap<>());

    assertEquals("my.project", model.get("name"));
    assertEquals(Boolean.TRUE, model.get("private"));
    assertEquals("^19.0.0", deps(model).get("react"));
    assertEquals("^3.0.0", deps(model).get("lit"));
  }

  @Test
  void shouldRouteDevDeclarationsToDevDependencies() {
    Map<String, Object> model = writer.createModel("demo",
        List.of(new BundlePackageDeclaration().setName("react").setVersion("^19.0.0").setDev(false),
            new BundlePackageDeclaration().setName("bun-plugin-vue3").setVersion("^1.1.0")
                .setDev(true)),
        new LinkedHashMap<>());

    assertEquals("^19.0.0", deps(model).get("react"));
    assertFalse(deps(model).containsKey("bun-plugin-vue3"),
        "a dev package must not be a runtime dependency");
    assertEquals("^1.1.0", devDeps(model).get("bun-plugin-vue3"));
  }

  @Test
  void shouldPreserveDeveloperDependencyOverManagedRange() {
    Map<String, Object> existing = model("""
        { "dependencies": { "lit": "3.2.1" }, "overrides": { "lit": "3.2.1" } }
        """);

    Map<String, Object> model = writer.createModel("demo",
        List.of(new BundlePackageDeclaration().setName("lit").setVersion("^3.0.0")), existing);

    assertEquals("3.2.1", deps(model).get("lit"),
        "developer pin must win over the annotation range");
    assertEquals("3.2.1", child(model, "overrides").get("lit"), "developer overrides must survive");
    assertFalse(model.containsKey("webforj"),
        "a developer owned dependency must not be claimed in the ledger");
  }

  @Test
  void shouldRecordManagedDependenciesInLedger() {
    Map<String, Object> model = writer.createModel("demo",
        List.of(new BundlePackageDeclaration().setName("react").setVersion("^19.0.0"),
            new BundlePackageDeclaration().setName("bun-plugin-vue3").setVersion("^1.1.0")
                .setDev(true)),
        new LinkedHashMap<>());

    Map<String, Object> ledger = child(model, "webforj");
    assertEquals("^19.0.0", child(ledger, "dependencies").get("react"));
    assertEquals("^1.1.0", child(ledger, "devDependencies").get("bun-plugin-vue3"));
  }

  @Test
  void shouldRemoveAManagedDependencyThatIsNoLongerDeclared() {
    Map<String, Object> first = writer.createModel("demo",
        List.of(new BundlePackageDeclaration().setName("react").setVersion("^19.0.0"),
            new BundlePackageDeclaration().setName("lit").setVersion("^3.0.0")),
        new LinkedHashMap<>());

    Map<String, Object> second = writer.createModel("demo",
        List.of(new BundlePackageDeclaration().setName("react").setVersion("^19.0.0")), first);

    assertEquals("^19.0.0", deps(second).get("react"));
    assertFalse(deps(second).containsKey("lit"), "a dropped managed dependency must be removed");
    assertFalse(child(second, "webforj").containsKey("lit"));
  }

  @Test
  void shouldKeepADeveloperDependencyWhenAManagedOneIsRemoved() {
    Map<String, Object> existing = model("""
        { "dependencies": { "my-own-lib": "1.4.0" } }
        """);
    Map<String, Object> first = writer.createModel("demo",
        List.of(new BundlePackageDeclaration().setName("lit").setVersion("^3.0.0")), existing);

    Map<String, Object> second = writer.createModel("demo", List.of(), first);

    assertEquals("1.4.0", deps(second).get("my-own-lib"), "developer dependency must survive");
    assertFalse(deps(second).containsKey("lit"), "managed dependency must be gone");
  }

  @Test
  void shouldKeepADeveloperVersionBumpOfAManagedDependency() {
    // The bundler pins tailwindcss, the developer bumps it in package.json, and a later build keeps
    // the developer version, even after the bundler default itself moves on.
    Map<String, Object> first =
        writer.createModel("demo", List.of(dev("tailwindcss", "^4.3.0")), new LinkedHashMap<>());
    devDeps(first).put("tailwindcss", "^4.5.0");

    Map<String, Object> second =
        writer.createModel("demo", List.of(dev("tailwindcss", "^4.6.0")), first);

    assertEquals("^4.5.0", devDeps(second).get("tailwindcss"),
        "a developer version bump must survive, even past a bundler default change");
    assertEquals("^4.6.0", child(child(second, "webforj"), "devDependencies").get("tailwindcss"),
        "the ledger records the bundler default so the override stays detectable");
  }

  @Test
  void shouldUpdateAManagedDependencyTheDeveloperHasNotTouched() {
    Map<String, Object> first =
        writer.createModel("demo", List.of(dev("tailwindcss", "^4.3.0")), new LinkedHashMap<>());

    Map<String, Object> second =
        writer.createModel("demo", List.of(dev("tailwindcss", "^4.6.0")), first);

    assertEquals("^4.6.0", devDeps(second).get("tailwindcss"),
        "a bundler version bump flows through when the developer has not overridden it");
  }

  @Test
  void shouldRoundTripAnExistingFile(@TempDir Path tmp) throws IOException {
    Path file = tmp.resolve("package.json");
    Files.writeString(file, "{ \"name\": \"x\", \"dependencies\": { \"lit\": \"3.2.1\" } }");

    Map<String, Object> read = writer.read(file);

    assertEquals("x", read.get("name"));
    assertEquals("3.2.1", child(read, "dependencies").get("lit"));
  }

  private BundlePackageDeclaration dev(String name, String version) {
    return new BundlePackageDeclaration().setName(name).setVersion(version).setDev(true);
  }

  private Map<String, Object> model(String json) {
    return new Gson().fromJson(json, new TypeToken<LinkedHashMap<String, Object>>() {}.getType());
  }

  private Map<String, Object> deps(Map<String, Object> model) {
    return child(model, "dependencies");
  }

  private Map<String, Object> devDeps(Map<String, Object> model) {
    return child(model, "devDependencies");
  }

  @SuppressWarnings("unchecked")
  private Map<String, Object> child(Map<String, Object> model, String key) {
    return (Map<String, Object>) model.getOrDefault(key, new LinkedHashMap<>());
  }
}
