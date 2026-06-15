package com.webforj.bundle.bun.plugin;

import com.webforj.bundle.bun.BundleContext;
import com.webforj.bundle.bun.BundleExtension;
import com.webforj.bundle.bun.discovery.BundlePackageDeclaration;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Set;

/**
 * Base for the bundler's built in extensions, each shipping a Bun build plugin and turning itself
 * on when a file it handles is authored under the frontend source root.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
abstract class CuratedBundleRegistrar implements BundleExtension {

  private final String id;
  private final Set<String> extensions;
  private final String wrapper;
  private final List<BundlePackageDeclaration> packages;

  CuratedBundleRegistrar(String id, Set<String> extensions, String wrapper,
      List<BundlePackageDeclaration> packages) {
    this.id = id;
    this.extensions = extensions;
    this.wrapper = wrapper;
    this.packages = packages;
  }

  static BundlePackageDeclaration pkg(String name, String version) {
    return new BundlePackageDeclaration().setName(name).setVersion(version).setDev(true);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getId() {
    return id;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabledByDefault(BundleContext context) {
    for (String extension : extensions) {
      if (context.getSourceExtensions().contains(extension)) {
        return true;
      }
    }

    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillBundle(BundleContext context) {
    for (BundlePackageDeclaration declaration : packages) {
      context.addPackage(declaration);
    }

    context.addPlugin(id, readWrapper());
  }

  private String readWrapper() {
    try (InputStream stream = getClass().getResourceAsStream(wrapper)) {
      if (stream == null) {
        throw new IOException("missing plugin wrapper " + wrapper);
      }

      return new String(stream.readAllBytes(), StandardCharsets.UTF_8);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }
}
