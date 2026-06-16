package com.webforj.bundle.bun.plugin;

import com.webforj.bundle.bun.BundleContext;
import com.webforj.bundle.bun.BundleExtension;
import com.webforj.bundle.bun.discovery.BundleEntryDeclaration;
import com.webforj.bundle.bun.discovery.BundlePackageDeclaration;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Built in extension, off by default, that compiles the Tailwind utilities a project uses into a
 * stylesheet loaded for every view.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class TailwindBundleRegistrar implements BundleExtension {

  static final String ID = "webforj-tailwind";
  static final String WRAPPER = "tailwind.mjs";
  static final String SUBDIR = "tailwind";
  static final String ENTRY_FILE = "tailwind.css";

  /**
   * {@inheritDoc}
   */
  @Override
  public String getId() {
    return ID;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabledByDefault(BundleContext context) {
    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillBundle(BundleContext context) throws IOException {
    List<Path> sources = context.getSourcePaths();
    if (sources.isEmpty()) {
      context.getLog()
          .warn("Tailwind found no application sources to scan, no utilities will be generated");
    }

    // The utilities are generated from the classes the sources use, so editing a source changes the
    // output. The sources sit outside the frontend root the watch already follows, so register them
    // to rebuild on, which regenerates the utilities when a class is added or removed during a
    // watch.
    for (Path source : sources) {
      context.addWatchPath(source);
    }

    Path entryDir = context.getGeneratedPath().resolve(SUBDIR);
    Files.createDirectories(entryDir);
    Path entryFile = entryDir.resolve(ENTRY_FILE);
    Files.writeString(entryFile, stylesheet(entryDir, sources));

    String buildPath =
        context.getFrontendPath().relativize(entryFile).toString().replace('\\', '/');
    context.addEntry(new BundleEntryDeclaration().setSource(buildPath).setBuildPath(buildPath));

    context.addPackage(
        new BundlePackageDeclaration().setName("tailwindcss").setVersion("^4.3.0").setDev(true));
    context.addPackage(new BundlePackageDeclaration().setName("bun-plugin-tailwind")
        .setVersion("^0.1.2").setDev(true));
    context.addPlugin(ID, readWrapper());

    context.getLog().info("Tailwind scanning {} source path(s)", sources.size());
  }

  /**
   * Builds the stylesheet that imports Tailwind and points its scan at the application sources.
   *
   * <p>
   * Automatic content detection is turned off so only the declared source paths are scanned, which
   * keeps the build from reading the frontend output and the generated sources.
   * </p>
   *
   * @param entryDir the directory the stylesheet is written into
   * @param sources the application source roots to scan
   * @return the stylesheet content
   */
  private String stylesheet(Path entryDir, List<Path> sources) {
    // Import the theme and the utilities, but not the preflight base reset. Preflight restyles
    // every bare element across the page, which fights the styling webforJ already applies to its
    // components. The utilities stay unlayered so a utility class wins over a component style the
    // same as a hand written rule would. Automatic content detection is turned off so only the
    // declared source paths are scanned, which keeps the build from reading the frontend output and
    // the generated sources.
    StringBuilder css = new StringBuilder();
    css.append("@import \"tailwindcss/theme.css\";\n");
    css.append("@import \"tailwindcss/utilities.css\" source(none);\n");
    for (Path source : sources) {
      String relative = entryDir.relativize(source).toString().replace('\\', '/');
      css.append("@source \"").append(relative).append("\";\n");
    }

    return css.toString();
  }

  private String readWrapper() throws IOException {
    try (InputStream stream = getClass().getResourceAsStream(WRAPPER)) {
      if (stream == null) {
        throw new IOException("missing plugin wrapper " + WRAPPER);
      }

      return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
    }
  }
}
