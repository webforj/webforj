package com.webforj.bbjship.assets;

import com.webforj.bbjship.ApplicationConfiguration;
import com.webforj.bbjship.DeploymentInstaller;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Reads resources and the asset configuration of one published application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class DeployedApplication implements AutoCloseable {

  private final ApplicationResourceLoader resources;
  private final ApplicationConfiguration configuration;

  private DeployedApplication(ApplicationResourceLoader resources,
      ApplicationConfiguration configuration) {
    this.resources = resources;
    this.configuration = configuration;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close() throws IOException {
    resources.close();
  }

  /**
   * Loads the application of a deployment directory.
   *
   * <p>
   * The application is read from the class folders of a development run in classpath order, else
   * from the application jar.
   * </p>
   *
   * @param directory the deployment directory
   *
   * @return the application
   * @throws IOException if the directory holds neither a development run nor the application jar,
   *         the dependency jars cannot be listed or the configuration cannot be read
   */
  static DeployedApplication load(Path directory) throws IOException {
    List<Path> application = DeploymentInstaller.findApplicationSources(directory);
    if (application.isEmpty()) {
      throw new IOException("BBj deployment application JAR is missing");
    }

    List<Path> sources = new ArrayList<>(application);
    sources.addAll(DeploymentInstaller.resolveDependencyJars(directory));
    ApplicationConfiguration configuration =
        ApplicationConfiguration.load(application.toArray(Path[]::new));
    URL[] urls = new URL[sources.size()];
    for (int i = 0; i < urls.length; i++) {
      urls[i] = sources.get(i).toUri().toURL();
    }

    return new DeployedApplication(new ApplicationResourceLoader(urls), configuration);
  }

  String getAssetsPrefix() {
    return configuration.getAssetsPrefix();
  }

  String getIconsPrefix() {
    return configuration.getIconsPrefix();
  }

  List<String> getIndexes() {
    return configuration.getIndexes();
  }

  List<String> getExtensions() {
    return configuration.getExtensions();
  }

  String getCacheControl() {
    return configuration.getCacheControl();
  }

  Map<String, String> getMimeExtensions() {
    return configuration.getMimeExtensions();
  }

  /**
   * Reads a resource from the application jar or class folders, then from the dependencies in
   * classpath order.
   *
   * @param name the resource name inside the application and its dependencies
   *
   * @return the resource bytes, an empty array for an empty file, or an empty optional when no
   *         source contains the resource, the name is a folder or the application is closed
   * @throws IOException if a jar or a file of a class folder cannot be read
   */
  Optional<byte[]> readResource(String name) throws IOException {
    if (isFolder(name)) {
      return Optional.empty();
    }

    try (InputStream input = resources.getResourceAsStream(name)) {
      return input == null ? Optional.empty() : Optional.of(input.readAllBytes());
    }
  }

  private boolean isFolder(String name) {
    if (name.endsWith("/")) {
      return true;
    }

    URL resource = resources.getResource(name);
    if (resource == null) {
      return false;
    }

    if ("file".equals(resource.getProtocol())) {
      return Files.isDirectory(Path.of(URI.create(resource.toExternalForm())));
    }

    return resources.getResource(name + "/") != null;
  }

  /**
   * Finds resources in the jars and the class folders of the application without loading any of its
   * classes.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  private static final class ApplicationResourceLoader extends URLClassLoader {

    ApplicationResourceLoader(URL[] sources) {
      super(sources, ClassLoader.getPlatformClassLoader());
    }

    /**
     * {@inheritDoc}
     *
     * <p>
     * Only the sources of the application are searched, never the parent class loader.
     * </p>
     */
    @Override
    public URL getResource(String name) {
      return findResource(name);
    }
  }
}
