package com.webforj.plugin.foundation.bbj;

import com.webforj.plugin.foundation.resolve.ApplicationClasspath;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * Writes the archive of a BBj application deployment.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class Deployment {

  private static final String ENGINE_ARTIFACT_ID = "webforj-engine";

  private final Path archive;
  private final ApplicationClasspath classpath;
  private final Path application;
  private final List<Path> classFolders;

  private Deployment(Builder builder) {
    this.archive = builder.archive;
    this.classpath = builder.classpath;
    this.application = builder.application;
    this.classFolders = builder.classFolders;
  }

  /**
   * Creates a new builder for a deployment at the given archive location.
   *
   * @param archive the output archive
   *
   * @return a new builder
   * @throws NullPointerException if archive is null
   */
  public static Builder create(Path archive) {
    return new Builder(archive);
  }

  /**
   * Writes the application archive without authentication credentials.
   *
   * @return the written archive
   * @throws IOException if the application or a dependency is invalid, the classpath carries
   *         webforj-engine, or the archive cannot be written
   */
  public Path writeArchive() throws IOException {
    Files.createDirectories(archive.getParent());
    Map<String, Path> files = new LinkedHashMap<>();
    int index = 0;
    Path framework = null;
    List<Path> programArchives = new ArrayList<>();
    for (ResolvedJar jar : classpath.getJars()) {
      if (isEngine(jar)) {
        throw new IOException("BBj deployment must not carry webforj-engine, BBj is the engine. "
            + "Exclude it from the webforj dependency");
      }

      if (!isResolved(jar)) {
        throw new IOException(
            "BBj deployment dependency is unresolved. Resolve the runtime classpath");
      }

      Path path = jar.file().toAbsolutePath().normalize();
      if (isFramework(jar)) {
        framework = path;
      }

      if (isProgramArchive(jar)) {
        programArchives.add(path);
      }

      String entry = String.format(Locale.ROOT, "%s/%05d-%s", DeploymentArchive.DEPENDENCIES_FOLDER,
          index++, path.getFileName());
      files.put(entry, path);
    }

    if (framework == null) {
      throw new IOException("BBj deployment requires webforj-foundation on the runtime classpath");
    }

    if (application == null && classFolders == null) {
      throw new IOException(
          "BBj deployment application JAR is missing. Package the application first");
    }

    if (application != null) {
      files.put(DeploymentArchive.APPLICATION_ENTRY, application);
    }

    writeArchiveFiles(framework, files, programArchives);

    return archive;
  }

  private static boolean isResolved(ResolvedJar jar) {
    return jar.file() != null && Files.isRegularFile(jar.file());
  }

  private static boolean isEngine(ResolvedJar jar) {
    return ApplicationClasspath.FRAMEWORK_GROUP_ID.equals(jar.groupId())
        && ENGINE_ARTIFACT_ID.equals(jar.artifactId());
  }

  private static boolean isFramework(ResolvedJar jar) {
    return ApplicationClasspath.FRAMEWORK_GROUP_ID.equals(jar.groupId())
        && ApplicationClasspath.FRAMEWORK_ARTIFACT_ID.equals(jar.artifactId());
  }

  private static boolean isProgramArchive(ResolvedJar jar) {
    return ApplicationClasspath.FRAMEWORK_GROUP_ID.equals(jar.groupId());
  }

  private void writeArchiveFiles(Path framework, Map<String, Path> files,
      List<Path> programArchives) throws IOException {
    Path launcher = Files.createTempFile(archive.getParent(), ".bbj-launcher-", ".bbj");
    List<Path> staged = new ArrayList<>();
    try {
      if (classFolders != null) {
        files.put(DeploymentArchive.DEVELOPMENT_PROPERTIES, writeDevelopmentProperties(staged));
      }

      try (JarFile jar = new JarFile(framework.toFile())) {
        JarEntry entry = jar.getJarEntry(DeploymentArchive.LAUNCHER_ENTRY);
        if (entry == null) {
          throw new IOException(
              "The webforj-foundation dependency does not contain the BBj launcher");
        }

        try (InputStream input = jar.getInputStream(entry)) {
          Files.copy(input, launcher, StandardCopyOption.REPLACE_EXISTING);
        }
      }

      files.put(DeploymentArchive.LAUNCHER_ENTRY, launcher);
      for (Path programArchive : programArchives) {
        addProgramFiles(programArchive, files, staged);
      }

      DeploymentArchive.write(archive, files);
    } finally {
      Files.deleteIfExists(launcher);
      for (Path file : staged) {
        Files.deleteIfExists(file);
      }
    }
  }

  private Path writeDevelopmentProperties(List<Path> staged) throws IOException {
    Path file = Files.createTempFile(archive.getParent(), ".bbj-development-", ".properties");
    staged.add(file);
    DeploymentArchive.writeClassFolders(file, classFolders);

    return file;
  }

  private void addProgramFiles(Path source, Map<String, Path> files, List<Path> staged)
      throws IOException {
    try (JarFile jar = new JarFile(source.toFile())) {
      Enumeration<JarEntry> entries = jar.entries();
      while (entries.hasMoreElements()) {
        JarEntry entry = entries.nextElement();
        String name = entry.getName();
        if (entry.isDirectory() || !name.toLowerCase(Locale.ROOT).endsWith(".bbj")
            || DeploymentArchive.LAUNCHER_ENTRY.equals(name)) {
          continue;
        }

        if (files.containsKey(name)) {
          throw new IOException("Duplicate BBj deployment program entry: " + name);
        }

        Path program = Files.createTempFile(archive.getParent(), ".bbj-program-", ".bbj");
        staged.add(program);
        try (InputStream input = jar.getInputStream(entry)) {
          Files.copy(input, program, StandardCopyOption.REPLACE_EXISTING);
        }

        files.put(name, program);
      }
    }
  }

  /**
   * Builds a {@link Deployment}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public static final class Builder {

    private final Path archive;
    private ApplicationClasspath classpath = new ApplicationClasspath(List.of());
    private Path application;
    private List<Path> classFolders;

    private Builder(Path archive) {
      this.archive = archive.toAbsolutePath().normalize();
    }

    /**
     * Sets the packaged application JAR.
     *
     * @param application the application JAR
     *
     * @return this builder
     * @throws NullPointerException if application is null
     */
    public Builder setApplication(Path application) {
      this.application = Objects.requireNonNull(application);
      return this;
    }

    /**
     * Sets the class folders a development run loads the application from, in classpath order, in
     * place of the packaged application JAR.
     *
     * @param classFolders the class folders as the BBj machine sees them
     *
     * @return this builder
     * @throws NullPointerException if classFolders or one of its folders is null
     * @throws IllegalArgumentException if classFolders is empty
     */
    public Builder setClassFolders(List<Path> classFolders) {
      if (classFolders.isEmpty()) {
        throw new IllegalArgumentException("a development run needs at least one class folder");
      }

      this.classFolders = classFolders.stream()
          .map(folder -> Objects.requireNonNull(folder).toAbsolutePath().normalize()).toList();
      return this;
    }

    /**
     * Sets the build tool's resolved application classpath.
     *
     * @param classpath the resolved classpath
     *
     * @return this builder
     * @throws NullPointerException if classpath is null
     */
    public Builder setClasspath(ApplicationClasspath classpath) {
      this.classpath = Objects.requireNonNull(classpath);
      return this;
    }

    /**
     * Builds the deployment.
     *
     * @return the deployment
     * @throws IllegalStateException if both the application JAR and the class folders are set
     */
    public Deployment build() {
      if (application != null && classFolders != null) {
        throw new IllegalStateException(
            "a deployment carries an application jar or class folders, not both");
      }

      return new Deployment(this);
    }
  }
}
