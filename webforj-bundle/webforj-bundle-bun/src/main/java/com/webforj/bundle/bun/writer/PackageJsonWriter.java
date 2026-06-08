package com.webforj.bundle.bun.writer;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import com.webforj.bundle.bun.discovery.BundlePackageDeclaration;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Maintains the project package.json that pins the npm dependencies discovered by the classpath
 * scan.
 *
 * <p>
 * The file lives at the project root next to the installed node_modules, and a developer owns it.
 * Each build reads the existing file and merges the {@code @BundlePackage} declarations into it
 * without overwriting it, so a dependency or version a developer added by hand is preserved.
 * </p>
 *
 * <p>
 * The dependencies the bundler manages are mirrored under a {@code webforj} ledger block so the
 * next build can tell its own entries apart from the developer's. The managed version is a default,
 * a developer bumps it by editing package.json and the edit is kept, since the ledger shows the
 * value no longer matches what the bundler wrote. A managed dependency that is no longer declared
 * is removed, a developer dependency is left untouched, and the standard {@code dependencies} and
 * {@code devDependencies} keys are what Bun installs from.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class PackageJsonWriter {
  static final String DEPENDENCIES = "dependencies";
  static final String DEV_DEPENDENCIES = "devDependencies";
  static final String LEDGER = "webforj";

  private static final Gson GSON =
      new GsonBuilder().setPrettyPrinting().disableHtmlEscaping().create();

  /**
   * Read the existing package.json into a mutable model, or an empty model when it is absent.
   *
   * @param packageJsonFile the package.json path
   * @return the parsed model, never null
   */
  public Map<String, Object> read(Path packageJsonFile) {
    if (packageJsonFile == null || !Files.isRegularFile(packageJsonFile)) {
      return new LinkedHashMap<>();
    }

    try {
      String json = Files.readString(packageJsonFile, StandardCharsets.UTF_8);
      Map<String, Object> parsed =
          GSON.fromJson(json, new TypeToken<LinkedHashMap<String, Object>>() {}.getType());

      return parsed == null ? new LinkedHashMap<>() : parsed;
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  /**
   * Merge the managed dependencies into the existing model without overwriting developer content.
   *
   * @param projectArtifactId the Maven artifactId, used as the package name when absent
   * @param packages the dependencies to pin from {@code @BundlePackage} and curated plugins
   * @param existing the existing package.json model, possibly empty
   *
   * @return the merged model ready for serialization
   */
  public Map<String, Object> createModel(String projectArtifactId,
      List<BundlePackageDeclaration> packages, Map<String, Object> existing) {
    Map<String, Object> root = existing == null ? new LinkedHashMap<>() : existing;
    root.putIfAbsent("name", projectArtifactId);
    root.putIfAbsent("private", true);

    Map<String, Object> dependencies = getOrCreateChildObject(root, DEPENDENCIES);
    Map<String, Object> devDependencies = getOrCreateChildObject(root, DEV_DEPENDENCIES);
    Map<String, Object> ledger = getExistingLedger(root);
    Map<String, Object> oldRuntime = getOrCreateChildObject(ledger, DEPENDENCIES);
    Map<String, Object> oldDev = getOrCreateChildObject(ledger, DEV_DEPENDENCIES);

    Map<String, Object> newRuntime = new LinkedHashMap<>();
    Map<String, Object> newDev = new LinkedHashMap<>();
    for (BundlePackageDeclaration pkg : packages) {
      Map<String, Object> target = pkg.isDev() ? devDependencies : dependencies;
      Map<String, Object> managed = pkg.isDev() ? oldDev : oldRuntime;
      Map<String, Object> recordedDependencies = pkg.isDev() ? newDev : newRuntime;
      String name = pkg.getName();

      if (!target.containsKey(name)) {
        // A package the bundler declares and the project does not have yet, pin the bundler
        // version.
        target.put(name, pkg.getVersion());
        recordedDependencies.put(name, pkg.getVersion());
      } else if (managed.containsKey(name)) {
        // A package the bundler manages. The bundler version is a default a developer can override
        // by editing package.json. When the value still matches what the bundler last wrote the
        // developer has not touched it, so the bundler updates it, otherwise the developer edit is
        // left in place. Either way the package stays managed so a later removal still applies.
        if (Objects.equals(target.get(name), managed.get(name))) {
          target.put(name, pkg.getVersion());
        }
        recordedDependencies.put(name, pkg.getVersion());
      }
      // Else a developer pinned a package the bundler does not own, leave it untouched.
    }

    removeStaleManaged(dependencies, oldRuntime, newRuntime);
    removeStaleManaged(devDependencies, oldDev, newDev);

    pruneEmpty(root, DEV_DEPENDENCIES, devDependencies);
    writeLedger(root, newRuntime, newDev);

    return root;
  }

  /**
   * Serialize the model to a package.json under the given directory.
   *
   * @param directory the directory to write the package.json into
   * @param model the package.json model
   * @return the path of the written file
   * @throws IOException if the file cannot be written
   */
  public Path write(Path directory, Map<String, Object> model) throws IOException {
    Files.createDirectories(directory);
    Path target = directory.resolve("package.json");
    Files.writeString(target, GSON.toJson(model), StandardCharsets.UTF_8);

    return target;
  }

  @SuppressWarnings("unchecked")
  private Map<String, Object> getOrCreateChildObject(Map<String, Object> parent, String key) {
    Object value = parent.get(key);
    if (value instanceof Map<?, ?> map) {
      return (Map<String, Object>) map;
    }

    Map<String, Object> created = new LinkedHashMap<>();
    parent.put(key, created);

    return created;
  }

  @SuppressWarnings("unchecked")
  private Map<String, Object> getExistingLedger(Map<String, Object> root) {
    Object value = root.get(LEDGER);

    return value instanceof Map<?, ?> map ? (Map<String, Object>) map : new LinkedHashMap<>();
  }

  private void removeStaleManaged(Map<String, Object> target, Map<String, Object> previouslyManaged,
      Map<String, Object> stillManaged) {
    List<String> stale = new ArrayList<>();
    for (String name : previouslyManaged.keySet()) {
      if (!stillManaged.containsKey(name)) {
        stale.add(name);
      }
    }

    stale.forEach(target::remove);
  }

  private void pruneEmpty(Map<String, Object> root, String key, Map<String, Object> value) {
    if (value.isEmpty()) {
      root.remove(key);
    }
  }

  private void writeLedger(Map<String, Object> root, Map<String, Object> runtime,
      Map<String, Object> dev) {
    if (runtime.isEmpty() && dev.isEmpty()) {
      root.remove(LEDGER);

      return;
    }
    Map<String, Object> ledger = new LinkedHashMap<>();
    if (!runtime.isEmpty()) {
      ledger.put(DEPENDENCIES, runtime);
    }
    if (!dev.isEmpty()) {
      ledger.put(DEV_DEPENDENCIES, dev);
    }
    root.put(LEDGER, ledger);
  }
}
