package com.webforj.bbjship;

import com.webforj.bbjship.DeploymentException.Reason;
import com.webforj.bbjship.model.InstalledDeployment;
import com.webforj.bbjship.model.PublishedApplication;
import com.webforj.plugin.foundation.bbj.BBjShipCommandLine;
import com.webforj.plugin.foundation.bbj.BBjShipPlugin;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.DeploymentArchive.ArchiveSizeException;
import com.webforj.plugin.foundation.bbj.FileTree;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.zip.ZipException;

/**
 * Writes an uploaded archive into a new deployment directory once its content is valid.
 *
 * <p>
 * The archive is unpacked into a staging folder beside the deployments and moved to
 * {@code deployments/{name}/{id}} only when the launcher, the application, the class folders of a
 * development run and the entry class are valid. Nothing of a refused archive stays.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class DeploymentStaging {

  private static final String CLASS_FOLDER_REFUSAL =
      "BBj development class folder must be an absolute directory on the BBj machine";

  private DeploymentStaging() {}

  /**
   * Writes an uploaded archive into a new deployment directory of a publish name.
   *
   * @param archive the uploaded archive
   * @param deployments the deployment root path
   * @param name the publish name
   * @param published the applications published by BBjShip
   *
   * @return the written deployment
   * @throws DeploymentException if the archive is invalid or too large, development runs are not
   *         enabled or another name publishes the same entry class
   * @throws IOException if the files cannot be written
   */
  static InstalledDeployment writeDeployment(InputStream archive, Path deployments, String name,
      List<PublishedApplication> published) throws IOException {
    Files.createDirectories(deployments);
    String id = UUID.randomUUID().toString();
    Path upload = deployments.resolve("." + id + ".zip");
    Path staging = deployments.resolve("." + id);
    Path directory = deployments.resolve(name).resolve(id);
    ApplicationConfiguration configuration;
    List<Path> classFolders;
    try {
      writeStaging(archive, upload, staging);
      classFolders = resolveClassFolders(staging, deployments);
      configuration = loadConfiguration(staging);
      applyEntryValidation(configuration, deployments, name, published);
      Files.createDirectories(directory.getParent());
      Files.move(staging, directory, StandardCopyOption.ATOMIC_MOVE);
    } catch (IOException | RuntimeException failure) {
      try {
        FileTree.removeDirectory(staging);
        DeploymentFolder.removeEmptyFolder(directory.getParent());
      } catch (IOException cleanup) {
        failure.addSuppressed(cleanup);
      }

      throw failure;
    } finally {
      Files.deleteIfExists(upload);
    }

    try {
      List<Path> classpath = new ArrayList<>(DeploymentFolder.resolveDependencyJars(directory));
      classpath.addAll(DeploymentFolder.readApplicationSources(directory));

      return InstalledDeployment.Builder.create().setDirectory(directory).setClasspath(classpath)
          .setConfiguration(configuration).setClassFolders(classFolders).build();
    } catch (IOException | RuntimeException failure) {
      try {
        DeploymentFolder.removeDeploymentDirectory(directory);
      } catch (IOException cleanup) {
        failure.addSuppressed(cleanup);
      }

      throw failure;
    }
  }

  private static ApplicationConfiguration loadConfiguration(Path staging) throws IOException {
    if (!Files.isRegularFile(staging.resolve(DeploymentArchive.LAUNCHER_ENTRY))) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE, "BBj deployment launcher is missing");
    }

    List<Path> sources = DeploymentFolder.readApplicationSources(staging);
    if (sources.isEmpty()) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE,
          "BBj deployment application JAR is missing");
    }

    return ApplicationConfiguration.load(sources.toArray(Path[]::new));
  }

  private static List<Path> resolveClassFolders(Path staging, Path deployments) throws IOException {
    Optional<List<String>> listed = DeploymentArchive.findClassFolders(staging);
    if (listed.isEmpty()) {
      return List.of();
    }

    if (!Files.isRegularFile(BBjShipPlugin.resolveDevelopmentSwitch(deployments))) {
      throw new DeploymentException(Reason.DEVELOPMENT_DISABLED,
          "Development runs are not enabled on this BBj machine. Run " + BBjShipCommandLine.BBJSHIP
              + " " + BBjShipCommandLine.DEVELOPMENT + " " + BBjShipCommandLine.ENABLE);
    }

    if (Files.exists(staging.resolve(DeploymentArchive.APPLICATION_ENTRY))) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE,
          "BBj development archive must not contain application.jar");
    }

    List<String> entries = listed.get();
    if (entries.isEmpty()) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE, CLASS_FOLDER_REFUSAL);
    }

    List<Path> folders =
        entries.stream().map(DeploymentStaging::findClassFolder).flatMap(Optional::stream).toList();
    if (folders.size() != entries.size()) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE,
          CLASS_FOLDER_REFUSAL + ": " + String.join(File.pathSeparator, entries));
    }

    return folders;
  }

  private static Optional<Path> findClassFolder(String entry) {
    try {
      Path folder = Path.of(entry);

      return folder.isAbsolute() && Files.isDirectory(folder) ? Optional.of(folder)
          : Optional.empty();
    } catch (InvalidPathException invalid) {
      return Optional.empty();
    }
  }

  private static void writeStaging(InputStream archive, Path upload, Path staging)
      throws IOException {
    try {
      DeploymentArchive.save(archive, upload);
      DeploymentArchive.read(upload, staging);
    } catch (ArchiveSizeException tooLarge) {
      throw new DeploymentException(Reason.ARCHIVE_TOO_LARGE, tooLarge.getMessage(), tooLarge);
    } catch (ZipException invalid) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE, invalid.getMessage(), invalid);
    }
  }

  private static void applyEntryValidation(ApplicationConfiguration configuration, Path deployments,
      String name, List<PublishedApplication> published) throws IOException {
    Optional<String> entryClass = configuration.getEntryClass();
    if (entryClass.isEmpty()) {
      return;
    }

    for (PublishedApplication application : published) {
      String other = application.getName();
      List<Path> sources = List.of();
      if (!other.equals(name)) {
        Optional<Path> directory = DeploymentFolder.findCurrentDirectory(deployments, other);
        if (directory.isPresent()) {
          sources = DeploymentFolder.readApplicationSources(directory.get());
        }
      }

      if (!sources.isEmpty() && entryClass
          .equals(ApplicationConfiguration.load(sources.toArray(Path[]::new)).getEntryClass())) {
        throw new DeploymentException(Reason.ENTRY_CONFLICT, "Entry class " + entryClass.get()
            + " is already published as " + other + ". Undeploy " + other + " first");
      }
    }
  }
}
