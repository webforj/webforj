package com.webforj.bbjship;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBBjProcess;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.DeploymentException.Reason;
import com.webforj.bbjship.model.DeploymentRegistration;
import com.webforj.bbjship.model.InstalledDeployment;
import com.webforj.bbjship.model.PublishedApplication;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.DeploymentArchive.ArchiveSizeException;
import com.webforj.plugin.foundation.bbj.FileTree;
import com.webforj.plugin.foundation.bbj.PublishNamePolicy;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipException;

/**
 * Publishes, lists and removes the applications of BBjShip.
 *
 * <p>
 * Each application lives in {@code deployments/{name}/{id}}, where the id is a random UUID. A
 * replacement unpacks into a new id beside the previous one and switches the registration to it.
 * The file {@code deployments/{name}/current} names the id the registration uses.
 * </p>
 *
 * <p>
 * A development run carries {@code development.properties} in place of {@code application.jar} and
 * loads the application from the class folders it names on this machine, in classpath order. Its
 * deployment directory holds a {@code development} file naming those class folders, one per line.
 * Development runs are accepted only while the {@link DeploymentArchive#DEVELOPMENT_SWITCH} file
 * exists in the BBjShip runtime folder.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class DeploymentInstaller {

  /**
   * Names the outcome of a completed deployment.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public enum DeploymentResult {
    CREATED, REPLACED
  }

  private static final String CLASSPATH_PREFIX = "webforj_bbjship_";
  private static final String DEFAULT_CLASSPATH = "(_webforj_bbjship)";
  private static final String LAUNCHER = "bbj/webforj.bbj";
  private static final String APPLICATION = "application.jar";
  private static final String CURRENT = "current";
  private static final String DEVELOPMENT = "development";
  private static final String CLASS_FOLDER_REFUSAL =
      "BBj development class folder must be an absolute directory on the BBj machine";
  private static final String LOCK = ".lock";
  private static final String APPLICATION_URL = "/webapp/";
  private static final Logger LOGGER = System.getLogger(DeploymentInstaller.class.getName());

  private DeploymentInstaller() {}

  /**
   * Publishes an uploaded archive and replaces a previous deployment owned by BBjShip.
   *
   * @param archive the uploaded archive
   * @param name the publish name
   * @param root the deployment root path
   * @param admin the authenticated administration API
   *
   * @return whether the application was created or replaced
   * @throws DeploymentException if the name or the archive is invalid, the archive is too large,
   *         the name belongs to another application, another name publishes the same entry class,
   *         the name is published in the other mode or development runs are not enabled
   * @throws IOException if the registration fails or cleanup fails
   */
  public static synchronized DeploymentResult deploy(InputStream archive, String name, String root,
      BBjAdminBase admin) throws IOException {
    return deploy(archive, name, root, admin, ClasspathRelease.NONE);
  }

  /**
   * Publishes an uploaded archive and replaces a previous deployment owned by BBjShip, releasing
   * the files the process holds open for the previous deployment before its folder is removed.
   *
   * @param archive the uploaded archive
   * @param name the publish name
   * @param root the deployment root path
   * @param admin the authenticated administration API
   * @param release the release of the files this process holds open for a classpath
   *
   * @return whether the application was created or replaced
   * @throws DeploymentException if the name or the archive is invalid, the archive is too large,
   *         the name belongs to another application, another name publishes the same entry class,
   *         the name is published in the other mode or development runs are not enabled
   * @throws IOException if the registration fails or cleanup fails
   */
  @SuppressWarnings("try")
  public static synchronized DeploymentResult deploy(InputStream archive, String name, String root,
      BBjAdminBase admin, ClasspathRelease release) throws IOException {
    applyNameValidation(name);
    Path deployments = Path.of(root).toAbsolutePath().normalize();
    try (FileChannel lock = openLock(deployments)) {
      return applyDeployment(archive, name, deployments, admin, release);
    }
  }

  /**
   * Undeploys an application and removes the files owned by BBjShip. The application is always
   * unregistered, a folder whose files cannot be removed stays.
   *
   * @param publishName the published application name
   * @param root the deployment root path
   * @param admin the authenticated administration API
   *
   * @return the report naming the folder that stayed and why, empty when it is removed
   * @throws DeploymentException if the name is invalid or BBjShip published no application of that
   *         name
   * @throws IOException if the deployment root cannot be locked or the unregistration fails
   */
  public static synchronized Optional<String> undeploy(String publishName, String root,
      BBjAdminBase admin) throws IOException {
    return undeploy(publishName, root, admin, ClasspathRelease.NONE);
  }

  /**
   * Undeploys an application and removes the files owned by BBjShip, releasing the files the
   * process holds open for its classpath first. The application is always unregistered, a folder
   * whose files cannot be removed stays.
   *
   * @param publishName the published application name
   * @param root the deployment root path
   * @param admin the authenticated administration API
   * @param release the release of the files this process holds open for a classpath
   *
   * @return the report naming the folder that stayed and why, empty when it is removed
   * @throws DeploymentException if the name is invalid or BBjShip published no application of that
   *         name
   * @throws IOException if the deployment root cannot be locked or the unregistration fails
   */
  @SuppressWarnings("try")
  public static synchronized Optional<String> undeploy(String publishName, String root,
      BBjAdminBase admin, ClasspathRelease release) throws IOException {
    applyNameValidation(publishName);
    Path deployments = Path.of(root).toAbsolutePath().normalize();
    try (FileChannel lock = openLock(deployments)) {
      return applyRemoval(publishName, deployments, admin, release);
    }
  }

  /**
   * Lists the applications published by BBjShip, sorted by name.
   *
   * @param root the deployment root path
   * @param admin the authenticated administration API
   *
   * @return the owned applications
   * @throws IOException if the deployment root cannot be read or the administration API fails
   */
  public static List<PublishedApplication> findApplications(String root, BBjAdminBase admin)
      throws IOException {
    Path deployments = Path.of(root).toAbsolutePath().normalize();
    if (!Files.isDirectory(deployments)) {
      return new ArrayList<>();
    }

    try {
      return findOwnedApplications(admin.getRemoteConfiguration(), deployments);
    } catch (BBjAdminException | RuntimeException failure) {
      throw toDeploymentFailure("the BBjShip applications", failure);
    }
  }

  /**
   * Finds the deployment directory named by the {@code current} file of a publish name.
   *
   * @param root the deployment root path
   * @param name the publish name
   *
   * @return the current deployment directory, empty when the name is invalid, the file is missing
   *         or it names no deployment directory
   * @throws IOException if the {@code current} file cannot be read
   */
  public static Optional<Path> findCurrentDirectory(Path root, String name) throws IOException {
    if (!PublishNamePolicy.isValid(name)) {
      return Optional.empty();
    }

    Path current = root.resolve(name).resolve(CURRENT);
    if (!Files.isRegularFile(current, LinkOption.NOFOLLOW_LINKS)) {
      return Optional.empty();
    }

    String id = Files.readString(current).strip();
    if (!isDeploymentId(id)) {
      return Optional.empty();
    }

    Path directory = root.resolve(name).resolve(id);

    return Files.isDirectory(directory, LinkOption.NOFOLLOW_LINKS) ? Optional.of(directory)
        : Optional.empty();
  }

  /**
   * Finds the class folders of a development run from its deployment directory.
   *
   * @param directory the deployment directory
   *
   * @return the class folders in classpath order, empty when the directory holds no development run
   * @throws IOException if the {@code development} file cannot be read
   */
  public static List<Path> findClassFolders(Path directory) throws IOException {
    Path marker = directory.resolve(DEVELOPMENT);
    if (!Files.isRegularFile(marker, LinkOption.NOFOLLOW_LINKS)) {
      return List.of();
    }

    return Files.readAllLines(marker).stream().map(String::strip).filter(line -> !line.isEmpty())
        .map(Path::of).toList();
  }

  /**
   * Finds the sources a deployment directory loads the application from.
   *
   * @param directory the deployment directory
   *
   * @return the class folders of a development run in classpath order, else the application jar,
   *         empty when the directory holds neither
   * @throws IOException if the development marker cannot be read
   */
  public static List<Path> findApplicationSources(Path directory) throws IOException {
    List<Path> classFolders = findClassFolders(directory);
    if (!classFolders.isEmpty()) {
      return classFolders;
    }

    Path jar = directory.resolve(APPLICATION);

    return Files.isRegularFile(jar) ? List.of(jar) : List.of();
  }

  /**
   * Resolves the dependency jars of a deployment directory in classpath order.
   *
   * <p>
   * Only the jars directly inside the {@code dependencies} folder count, sorted by file name.
   * </p>
   *
   * @param directory the deployment directory
   *
   * @return the dependency jars
   * @throws DeploymentException if the dependencies folder is missing
   * @throws IOException if the {@code dependencies} folder cannot be read
   */
  public static List<Path> resolveDependencyJars(Path directory) throws IOException {
    Path dependencies = directory.resolve("dependencies");
    if (!Files.isDirectory(dependencies)) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE,
          "BBj deployment dependencies folder is missing");
    }

    try (Stream<Path> files = Files.list(dependencies)) {
      return files.filter(Files::isRegularFile)
          .filter(path -> path.getFileName().toString().endsWith(".jar")).sorted().toList();
    }
  }

  /**
   * Gets the path BBjServices publishes an application under.
   *
   * @param name the publish name
   *
   * @return the path of the application
   */
  public static String getApplicationUrl(String name) {
    return APPLICATION_URL + name;
  }

  private static DeploymentResult applyDeployment(InputStream archive, String name,
      Path deployments, BBjAdminBase admin, ClasspathRelease release) throws IOException {
    String classpathName = CLASSPATH_PREFIX + name;
    BBjAdminAppDeploymentConfiguration configuration;
    BBjAdminAppDeploymentApplication previous;
    List<PublishedApplication> published;
    Path previousDirectory = null;
    boolean claimed;
    try {
      configuration = admin.getRemoteConfiguration();
      applyCaseValidation(configuration, deployments, name);
      published = findOwnedApplications(configuration, deployments);
      previous = findApplication(configuration, name);
      if (previous != null) {
        previousDirectory = resolveOwnedDirectory(previous, name, deployments);
      }

      claimed = previous == null && admin.getClasspathNames().contains(classpathName);
    } catch (BBjAdminException | RuntimeException failure) {
      throw toDeploymentFailure(name, failure);
    }

    InstalledDeployment deployment = load(archive, deployments, name, published);
    List<String> previousClasspath = null;
    DeploymentRegistration previousRegistration = null;
    boolean mutationStarted = false;
    Closeable hold = () -> {
    };
    try {
      if (previousDirectory != null) {
        applyModeValidation(deployment, previousDirectory, name);
      }

      if (previous != null || claimed) {
        previousClasspath = new ArrayList<>(admin.getClasspath(classpathName));
        // The previous classpath still resolves to the loader that holds the previous files,
        // it is closed once the previous sessions are stopped and before the new classpath replaces
        // it
        hold = release.find(classpathName);
      }

      if (previous != null) {
        previousRegistration = new DeploymentRegistration(previous.getProperties(),
            previous.getArguments(), previous.getJavaArguments());
      }

      final BBjAdminAppDeploymentApplication application =
          createApplication(configuration, deployment, name, classpathName);
      mutationStarted = true;
      if (previous != null) {
        configuration.removeApplication(name);
        stopProcesses(admin, classpathName);
      }

      closeHold(hold, classpathName);
      admin.setClasspath(classpathName, createClasspath(deployment));
      admin.commit();
      application.commit();
      writeCurrent(deployment);
    } catch (BBjAdminException | IOException | RuntimeException failure) {
      closeHold(hold, classpathName);
      IOException problem = toDeploymentFailure(name, failure);
      if (mutationStarted) {
        applyRegistrationRollback(admin, configuration, name, previousClasspath,
            previousRegistration, problem);
      }

      try {
        removeDeploymentDirectory(deployment.getDirectory());
      } catch (IOException cleanup) {
        problem.addSuppressed(cleanup);
      }

      throw problem;
    }

    removeLeftoverDirectories(deployment.getDirectory());

    return previousDirectory == null ? DeploymentResult.CREATED : DeploymentResult.REPLACED;
  }

  private static Optional<String> applyRemoval(String publishName, Path deployments,
      BBjAdminBase admin, ClasspathRelease release) throws IOException {
    String classpathName = CLASSPATH_PREFIX + publishName;
    Optional<Path> directory;
    Closeable hold;
    try {
      BBjAdminAppDeploymentConfiguration configuration = admin.getRemoteConfiguration();
      BBjAdminAppDeploymentApplication application = findApplication(configuration, publishName);
      directory = application == null ? Optional.empty()
          : findOwnedDirectory(application, publishName, deployments);
      boolean claimed = application == null && admin.getClasspathNames().contains(classpathName);
      if (directory.isEmpty() && !claimed) {
        throw new DeploymentException(Reason.NOT_FOUND, "No BBj application named " + publishName
            + " was deployed by BBjShip. Check the publish name");
      }

      List<String> previousClasspath = new ArrayList<>(admin.getClasspath(classpathName));
      DeploymentRegistration previousRegistration = application == null ? null
          : new DeploymentRegistration(application.getProperties(), application.getArguments(),
              application.getJavaArguments());

      // The hold is found while the classpath is still registered and closed once every session
      // of the application has ended and the classpath is gone, so no session loads from it
      hold = release.find(classpathName);
      try {
        applyUnregistration(admin, configuration, publishName, previousClasspath,
            previousRegistration);
      } catch (IOException | RuntimeException failure) {
        closeHold(hold, classpathName);
        throw failure;
      }
    } catch (BBjAdminException | RuntimeException failure) {
      throw toDeploymentFailure(publishName, failure);
    }

    closeHold(hold, classpathName);
    if (directory.isEmpty()) {
      return Optional.empty();
    }

    Path folder = directory.get().getParent();
    try {
      FileTree.removeDirectory(directory.get());
      Files.deleteIfExists(folder.resolve(CURRENT));
      removeEmptyFolder(folder);
    } catch (IOException held) {
      // Windows refuses to delete a jar the BBjServices virtual machine has open, and BBjServices
      // keeps the jars of a removed classpath open until it restarts
      String report = formatKeptFolder(folder, held);
      LOGGER.log(Level.DEBUG, report, held);
      return Optional.of(report);
    }

    return Optional.empty();
  }

  private static void closeHold(Closeable hold, String classpathName) {
    try {
      hold.close();
    } catch (IOException | RuntimeException failure) {
      // The removal is attempted either way, the folder report names what stayed
      LOGGER.log(Level.DEBUG,
          "The files of the BBj classpath " + classpathName + " could not be released", failure);
    }
  }

  private static FileChannel openLock(Path deployments) throws IOException {
    Files.createDirectories(deployments);
    FileChannel channel = FileChannel.open(deployments.resolve(LOCK), StandardOpenOption.CREATE,
        StandardOpenOption.WRITE);
    try {
      channel.lock();
    } catch (IOException | RuntimeException failure) {
      channel.close();
      throw failure;
    }

    return channel;
  }

  private static InstalledDeployment load(InputStream archive, Path deployments, String name,
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

      classFolders = readClassFolders(staging, deployments);
      configuration = loadConfiguration(staging,
          classFolders.isEmpty() ? List.of(staging.resolve(APPLICATION)) : classFolders);
      applyEntryValidation(configuration, deployments, name, published);
      Files.createDirectories(directory.getParent());
      Files.move(staging, directory, StandardCopyOption.ATOMIC_MOVE);
    } catch (IOException | RuntimeException failure) {
      try {
        FileTree.removeDirectory(staging);
        removeEmptyFolder(directory.getParent());
      } catch (IOException cleanup) {
        failure.addSuppressed(cleanup);
      }

      throw failure;
    } finally {
      Files.deleteIfExists(upload);
    }

    try {
      List<Path> classpath = new ArrayList<>(resolveDependencyJars(directory));
      if (classFolders.isEmpty()) {
        classpath.add(directory.resolve(APPLICATION));
      }

      return InstalledDeployment.Builder.create().setDirectory(directory).setClasspath(classpath)
          .setConfiguration(configuration).setClassFolders(classFolders).build();
    } catch (IOException | RuntimeException failure) {
      try {
        removeDeploymentDirectory(directory);
      } catch (IOException cleanup) {
        failure.addSuppressed(cleanup);
      }

      throw failure;
    }
  }

  private static ApplicationConfiguration loadConfiguration(Path staging, List<Path> sources)
      throws IOException {
    if (!Files.isRegularFile(staging.resolve(LAUNCHER))) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE, "BBj deployment launcher is missing");
    }

    if (!sources.stream().allMatch(Files::exists)) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE,
          "BBj deployment application JAR is missing");
    }

    return ApplicationConfiguration.load(sources.toArray(Path[]::new));
  }

  private static List<Path> readClassFolders(Path staging, Path deployments) throws IOException {
    Path properties = staging.resolve(DeploymentArchive.DEVELOPMENT_PROPERTIES);
    if (!Files.isRegularFile(properties)) {
      return List.of();
    }

    if (!Files.isRegularFile(deployments.resolveSibling(DeploymentArchive.DEVELOPMENT_SWITCH))) {
      throw new DeploymentException(Reason.DEVELOPMENT_DISABLED,
          "Development runs are not enabled on this BBj machine. Run bbjship development enable");
    }

    if (Files.exists(staging.resolve(APPLICATION))) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE,
          "BBj development archive must not contain application.jar");
    }

    Properties development = new Properties();
    try (Reader reader = Files.newBufferedReader(properties, StandardCharsets.UTF_8)) {
      development.load(reader);
    }

    String value = development.getProperty(DeploymentArchive.CLASS_FOLDER_KEY, "").strip();
    if (value.isEmpty()) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE, CLASS_FOLDER_REFUSAL);
    }

    List<String> entries =
        Arrays.stream(value.split(File.pathSeparator, -1)).map(String::strip).toList();
    List<Path> folders = entries.stream().map(DeploymentInstaller::findClassFolder)
        .flatMap(Optional::stream).toList();
    if (folders.size() != entries.size()) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE, CLASS_FOLDER_REFUSAL + ": " + value);
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
      writeUpload(archive, upload);
      DeploymentArchive.read(upload, staging);
    } catch (ArchiveSizeException tooLarge) {
      throw new DeploymentException(Reason.ARCHIVE_TOO_LARGE, tooLarge.getMessage(), tooLarge);
    } catch (ZipException invalid) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE, invalid.getMessage(), invalid);
    }
  }

  private static void writeUpload(InputStream archive, Path upload) throws IOException {
    try (OutputStream output = Files.newOutputStream(upload)) {
      byte[] buffer = new byte[8192];
      long total = 0;
      int size;
      while ((size = archive.read(buffer)) != -1) {
        total += size;
        if (total > DeploymentArchive.MAX_BYTES) {
          throw new DeploymentException(Reason.ARCHIVE_TOO_LARGE,
              DeploymentArchive.TOO_LARGE_MESSAGE);
        }

        output.write(buffer, 0, size);
      }
    }
  }

  private static void writeCurrent(InstalledDeployment deployment) throws IOException {
    Path directory = deployment.getDirectory();
    List<Path> classFolders = deployment.getClassFolders();
    if (!classFolders.isEmpty()) {
      Files.writeString(directory.resolve(DEVELOPMENT), classFolders.stream().map(Path::toString)
          .collect(Collectors.joining(System.lineSeparator())));
    }

    Path folder = directory.getParent();
    Path written = Files.createTempFile(folder, "." + CURRENT, ".tmp");
    try {
      Files.writeString(written, directory.getFileName().toString());
      Files.move(written, folder.resolve(CURRENT), StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING);
    } finally {
      Files.deleteIfExists(written);
    }
  }

  private static void applyNameValidation(String name) throws DeploymentException {
    if (!PublishNamePolicy.isValid(name)) {
      throw new DeploymentException(Reason.INVALID_NAME,
          "BBj deployment requires a valid publish name");
    }
  }

  private static void applyCaseValidation(BBjAdminAppDeploymentConfiguration configuration,
      Path deployments, String name) throws BBjAdminException, IOException {
    for (BBjAdminAppDeploymentApplication application : configuration.getApplications()) {
      String other = application.getString(BBjAdminAppDeploymentApplication.NAME);
      if (isCaseConflict(other, name)) {
        throw createCaseConflict(other, name);
      }
    }

    if (!Files.isDirectory(deployments)) {
      return;
    }

    Optional<String> conflict;
    try (Stream<Path> folders = Files.list(deployments)) {
      conflict = folders.map(folder -> folder.getFileName().toString())
          .filter(other -> isCaseConflict(other, name)).findFirst();
    }

    if (conflict.isPresent()) {
      throw createCaseConflict(conflict.get(), name);
    }
  }

  private static boolean isCaseConflict(String other, String name) {
    return other != null && !other.equals(name) && other.equalsIgnoreCase(name);
  }

  private static DeploymentException createCaseConflict(String other, String name) {
    return new DeploymentException(Reason.NAME_CONFLICT, "Published BBj application " + other
        + " differs from " + name + " only by letter case. Choose another publish name");
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
        Optional<Path> directory = findCurrentDirectory(deployments, other);
        if (directory.isPresent()) {
          sources = findApplicationSources(directory.get());
        }
      }

      if (!sources.isEmpty() && entryClass
          .equals(ApplicationConfiguration.load(sources.toArray(Path[]::new)).getEntryClass())) {
        throw new DeploymentException(Reason.ENTRY_CONFLICT, "Entry class " + entryClass.get()
            + " is already published as " + other + ". Undeploy " + other + " first");
      }
    }
  }

  private static void applyModeValidation(InstalledDeployment deployment, Path previousDirectory,
      String name) throws IOException {
    boolean development = !deployment.getClassFolders().isEmpty();
    boolean previousDevelopment = !findClassFolders(previousDirectory).isEmpty();
    if (previousDevelopment && !development) {
      throw new DeploymentException(Reason.MODE_CONFLICT,
          name + " is a development run. Stop the watch first");
    }

    if (development && !previousDevelopment) {
      throw new DeploymentException(Reason.MODE_CONFLICT,
          name + " is deployed from an archive. Undeploy it first");
    }
  }

  private static List<PublishedApplication> findOwnedApplications(
      BBjAdminAppDeploymentConfiguration configuration, Path deployments)
      throws BBjAdminException, IOException {
    List<PublishedApplication> applications = new ArrayList<>();
    if (!Files.isDirectory(deployments)) {
      return applications;
    }

    List<String> names;
    try (Stream<Path> folders = Files.list(deployments)) {
      names = folders.filter(folder -> Files.isDirectory(folder, LinkOption.NOFOLLOW_LINKS))
          .map(folder -> folder.getFileName().toString()).filter(PublishNamePolicy::isValid)
          .sorted().toList();
    }

    for (String name : names) {
      BBjAdminAppDeploymentApplication application = findApplication(configuration, name);
      Optional<Path> directory = application == null ? Optional.empty()
          : findOwnedDirectory(application, name, deployments);
      if (directory.isPresent()) {
        applications.add(new PublishedApplication(name, getApplicationUrl(name), directory.get(),
            findClassFolders(directory.get())));
      }
    }

    return applications;
  }

  private static BBjAdminAppDeploymentApplication findApplication(
      BBjAdminAppDeploymentConfiguration configuration, String name) throws BBjAdminException {
    for (BBjAdminAppDeploymentApplication application : configuration.getApplications()) {
      if (name.equals(application.getString(BBjAdminAppDeploymentApplication.NAME))) {
        return application;
      }
    }

    return null;
  }

  private static Optional<Path> findOwnedDirectory(BBjAdminAppDeploymentApplication application,
      String name, Path deployments) throws BBjAdminException {
    try {
      return Optional.of(resolveOwnedDirectory(application, name, deployments));
    } catch (IOException notOwned) {
      return Optional.empty();
    }
  }

  private static Path resolveOwnedDirectory(BBjAdminAppDeploymentApplication application,
      String name, Path deployments) throws BBjAdminException, IOException {
    if (!(CLASSPATH_PREFIX + name)
        .equals(application.getString(BBjAdminAppDeploymentApplication.CLASSPATH))) {
      throw createOwnershipFailure(name);
    }

    String configuredProgram = application.getString(BBjAdminAppDeploymentApplication.PROGRAM);
    if (configuredProgram == null || configuredProgram.isBlank()) {
      throw createOwnershipFailure(name);
    }

    Path program = Path.of(configuredProgram).toAbsolutePath().normalize();
    Path directory = program.getParent() == null ? null : program.getParent().getParent();
    Path folder = deployments.resolve(name);
    if (directory == null || !folder.equals(directory.getParent())
        || !program.equals(directory.resolve(LAUNCHER)) || !isDeploymentId(directory)) {
      throw createOwnershipFailure(name);
    }

    if (!Files.exists(directory, LinkOption.NOFOLLOW_LINKS) && !Files.isSymbolicLink(folder)) {
      return directory;
    }

    if (Files.isSymbolicLink(folder) || Files.isSymbolicLink(directory)
        || Files.isSymbolicLink(program)) {
      throw createOwnershipFailure(name);
    }

    Path realDirectory = directory.toRealPath();
    if (!folder.toRealPath().equals(realDirectory.getParent())) {
      throw createOwnershipFailure(name);
    }

    if (!Files.exists(program, LinkOption.NOFOLLOW_LINKS)) {
      return realDirectory;
    }

    if (!program.toRealPath().equals(realDirectory.resolve(LAUNCHER))
        || !Files.isRegularFile(program, LinkOption.NOFOLLOW_LINKS)) {
      throw createOwnershipFailure(name);
    }

    return realDirectory;
  }

  private static boolean isDeploymentId(Path directory) {
    return isDeploymentId(directory.getFileName().toString());
  }

  private static boolean isDeploymentId(String id) {
    try {
      return UUID.fromString(id).toString().equals(id);
    } catch (IllegalArgumentException invalid) {
      return false;
    }
  }

  private static BBjAdminAppDeploymentApplication createApplication(
      BBjAdminAppDeploymentConfiguration configuration, InstalledDeployment deployment, String name,
      String classpathName) throws BBjAdminException, IOException {
    BBjAdminAppDeploymentApplication application = configuration.createApplication();
    Path program = deployment.getDirectory().resolve(LAUNCHER);
    application.setString(BBjAdminAppDeploymentApplication.NAME, name);
    application.setString(BBjAdminAppDeploymentApplication.PROGRAM, program.toString());
    application.setString(BBjAdminAppDeploymentApplication.CLASSPATH, classpathName);
    application.setString(BBjAdminAppDeploymentApplication.WORKING_DIRECTORY,
        program.getParent().toString());
    application.setBoolean(BBjAdminAppDeploymentApplication.EXE_ENABLED, false);
    application.setBoolean(BBjAdminAppDeploymentApplication.BUI_ENABLED, false);
    application.setBoolean(BBjAdminAppDeploymentApplication.DWC_ENABLED, true);
    applyConfiguration(application, deployment);

    return application;
  }

  private static void applyConfiguration(BBjAdminAppDeploymentApplication application,
      InstalledDeployment deployment) throws BBjAdminException, IOException {
    ApplicationConfiguration configuration = deployment.getConfiguration();
    Optional<Boolean> quiet = configuration.getQuietMode();
    if (quiet.isPresent()) {
      application.setBoolean(BBjAdminAppDeploymentApplication.QUIET, quiet.get());
    }

    Optional<Boolean> warning = configuration.getShowBrowserWarning();
    if (warning.isPresent()) {
      application.setBoolean(BBjAdminAppDeploymentApplication.SHOW_BROWSER_WARNING, warning.get());
      application.setBoolean(BBjAdminAppDeploymentApplication.SHOW_BROWSER_WARNING_USE_DEFAULT,
          false);
    }

    Optional<Boolean> history = configuration.getManageBrowserHistory();
    if (history.isPresent()) {
      application.setBoolean(BBjAdminAppDeploymentApplication.MANAGE_BROWSER_HISTORY,
          history.get());
      application.setBoolean(BBjAdminAppDeploymentApplication.MANAGE_BROWSER_HISTORY_USE_DEFAULT,
          false);
    }

    application.setBoolean(BBjAdminAppDeploymentApplication.SHOW_CONFIRM_CLOSE_DIALOG,
        configuration.isClientConfirmationDialogShown());
    application.setBoolean(BBjAdminAppDeploymentApplication.SHOW_CONFIRM_CLOSE_DIALOG_USE_DEFAULT,
        false);
    application.setBoolean(BBjAdminAppDeploymentApplication.DISALLOW_CONSOLE,
        configuration.isConsoleDisallowed());
    application.setBoolean(BBjAdminAppDeploymentApplication.DISALLOW_CONSOLE_USE_DEFAULT, false);
    Optional<Boolean> secure = configuration.getSecure();
    if (secure.isPresent()) {
      application.setBoolean(BBjAdminAppDeploymentApplication.SECURE, secure.get());
      application.setBoolean(BBjAdminAppDeploymentApplication.SECURE_USE_DEFAULT, false);
    }

    Optional<String> configFile = configuration.getConfigFile();
    if (configFile.isPresent()) {
      application.setString(BBjAdminAppDeploymentApplication.CONFIG_FILE, configFile.get());
    }

    List<String> arguments = configuration.getProgramArguments();
    if (!arguments.isEmpty()) {
      application.getArguments().clear();
      application.getArguments().addAll(arguments);
    }
  }

  private static List<String> createClasspath(InstalledDeployment deployment) {
    List<String> entries = new ArrayList<>();
    entries.add(DEFAULT_CLASSPATH);
    for (Path path : deployment.getClasspath()) {
      entries.add(path.toString());
    }

    for (Path folder : deployment.getClassFolders()) {
      entries.add(folder.toString());
    }

    return entries;
  }

  private static void stopProcesses(BBjAdminBase admin, String classpathName)
      throws BBjAdminException {
    for (BBjAdminBBjProcess process : admin.getBBjProcesses()) {
      if (classpathName.equals(process.getString(BBjAdminBBjProcess.SSCP_NAME))) {
        process.terminate();
      }
    }
  }

  private static void applyUnregistration(BBjAdminBase admin,
      BBjAdminAppDeploymentConfiguration configuration, String name, List<String> classpath,
      DeploymentRegistration registration) throws IOException {
    String classpathName = CLASSPATH_PREFIX + name;
    try {
      if (registration != null) {
        configuration.removeApplication(name);
      }

      stopProcesses(admin, classpathName);
      admin.setClasspath(classpathName, null);
      admin.commit();
    } catch (BBjAdminException | RuntimeException failure) {
      IOException problem = toDeploymentFailure(name, failure);
      applyRegistrationRollback(admin, configuration, name, classpath, registration, problem);
      throw problem;
    }
  }

  private static void applyRegistrationRollback(BBjAdminBase admin,
      BBjAdminAppDeploymentConfiguration configuration, String name, List<String> classpath,
      DeploymentRegistration registration, IOException failure) {
    try {
      configuration.removeApplication(name);
    } catch (BBjAdminException | RuntimeException cleanup) {
      failure.addSuppressed(cleanup);
    }

    try {
      admin.setClasspath(CLASSPATH_PREFIX + name, classpath);
    } catch (BBjAdminException | RuntimeException cleanup) {
      failure.addSuppressed(cleanup);
    }

    try {
      admin.commit();
    } catch (BBjAdminException | RuntimeException cleanup) {
      failure.addSuppressed(cleanup);
    }

    if (registration == null) {
      return;
    }

    try {
      BBjAdminAppDeploymentApplication restored = configuration.createApplication();
      restored.setProperties(registration.getProperties());
      restored.getArguments().clear();
      restored.getArguments().addAll(registration.getArguments());
      restored.getJavaArguments().clear();
      restored.getJavaArguments().addAll(registration.getJavaArguments());
      restored.commit();
    } catch (BBjAdminException | RuntimeException cleanup) {
      failure.addSuppressed(cleanup);
    }
  }

  private static DeploymentException createOwnershipFailure(String name) {
    return new DeploymentException(Reason.NAME_CONFLICT, "Published BBj application " + name
        + " is not owned by BBjShip. Choose another publish name");
  }

  private static IOException toDeploymentFailure(String name, Exception failure) {
    return failure instanceof IOException io ? io
        : new IOException("BBj deployment failed for " + name
            + ". Check deployment settings and the BBjServices log", failure);
  }

  private static void removeLeftoverDirectories(Path current) throws IOException {
    List<Path> leftovers;
    try (Stream<Path> entries = Files.list(current.getParent())) {
      leftovers = entries.filter(entry -> !entry.equals(current))
          .filter(entry -> Files.isDirectory(entry, LinkOption.NOFOLLOW_LINKS))
          .filter(DeploymentInstaller::isDeploymentId).toList();
    }

    for (Path leftover : leftovers) {
      try {
        FileTree.removeDirectory(leftover);
      } catch (IOException failure) {
        LOGGER.log(Level.WARNING, "Cannot remove the previous BBj deployment folder " + leftover
            + ". The next deployment of this name removes it", failure);
      }
    }
  }

  /**
   * Formats the report of a folder that stayed because its files could not be removed.
   *
   * @param folder the folder that stayed
   * @param failure the removal failure
   *
   * @return the report
   */
  public static String formatKeptFolder(Path folder, IOException failure) {
    return "Not removed: " + folder + ". " + failure.getMessage();
  }

  private static void removeDeploymentDirectory(Path directory) throws IOException {
    FileTree.removeDirectory(directory);
    removeEmptyFolder(directory.getParent());
  }

  private static void removeEmptyFolder(Path folder) throws IOException {
    if (!Files.isDirectory(folder, LinkOption.NOFOLLOW_LINKS)) {
      return;
    }

    try (Stream<Path> children = Files.list(folder)) {
      if (children.findAny().isPresent()) {
        return;
      }
    }

    Files.delete(folder);
  }
}
