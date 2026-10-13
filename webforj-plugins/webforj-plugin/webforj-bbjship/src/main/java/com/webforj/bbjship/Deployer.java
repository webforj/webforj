package com.webforj.bbjship;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.DeploymentException.Reason;
import com.webforj.bbjship.cli.admin.BBjShipClasspaths;
import com.webforj.bbjship.model.DeploymentRegistration;
import com.webforj.bbjship.model.InstalledDeployment;
import com.webforj.bbjship.model.PublishedApplication;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.PollingWait;
import com.webforj.plugin.foundation.bbj.PublishNamePolicy;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.nio.channels.FileChannel;
import java.nio.channels.OverlappingFileLockException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Publishes, lists and removes the applications of BBjShip.
 *
 * <p>
 * A replacement unpacks into a new deployment directory beside the previous one and switches the
 * registration to it, see {@link DeploymentFolder}. When a step fails, the registration and the
 * classpath get back what they had.
 * </p>
 *
 * <p>
 * A development run carries {@code development.properties} in place of {@code application.jar} and
 * loads the application from the class folders it names on this machine, in classpath order.
 * Development runs are accepted only while the development switch of BBjShip exists in the BBjShip
 * runtime folder.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class Deployer {

  /**
   * Names the outcome of a completed deployment.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public enum DeploymentResult {
    CREATED, REPLACED
  }

  static final Duration LOCK_TIMEOUT = Duration.ofSeconds(30);
  private static final Duration LOCK_INTERVAL = Duration.ofMillis(100);
  private static final String LOCK_SUBJECT = "Waiting for another deployment";
  private static final String LOCK = ".lock";
  private static final String APPLICATION_URL = "/webapp/";
  private static final Logger LOGGER = System.getLogger(Deployer.class.getName());

  private Deployer() {}

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
   * @throws IOException if the registration fails, cleanup fails or another deployment holds the
   *         deployment root longer than {@link #LOCK_TIMEOUT}
   */
  @SuppressWarnings("try")
  public static synchronized DeploymentResult deploy(InputStream archive, String name, String root,
      BBjAdminBase admin, ClasspathRelease release) throws IOException {
    applyNameValidation(name);
    Path deployments = Path.of(root).toAbsolutePath().normalize();
    try (FileChannel lock = openDeploymentLock(deployments)) {
      return applyDeployment(archive, name, deployments, admin, release);
    }
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
    try (FileChannel lock = openDeploymentLock(deployments)) {
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
  public static List<PublishedApplication> getApplications(String root, BBjAdminBase admin)
      throws IOException {
    Path deployments = Path.of(root).toAbsolutePath().normalize();
    if (!Files.isDirectory(deployments)) {
      return new ArrayList<>();
    }

    try {
      return readOwnedApplications(admin.getRemoteConfiguration(), deployments);
    } catch (BBjAdminException | RuntimeException failure) {
      throw toDeploymentFailure("the BBjShip applications", failure);
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

  /**
   * Locks the deployment root, waiting while another deployment of the process or of another
   * process holds the lock.
   *
   * @param deployments the deployment root
   * @param wait the wait for the lock
   *
   * @return the channel that holds the lock, closing it releases the lock
   * @throws IOException if the root cannot be locked or the wait ends before the lock is free
   */
  static FileChannel openLock(Path deployments, PollingWait wait) throws IOException {
    Files.createDirectories(deployments);
    FileChannel channel = FileChannel.open(deployments.resolve(LOCK), StandardOpenOption.CREATE,
        StandardOpenOption.WRITE);
    try {
      // The command line and the servlet load this class twice in one process, so a lock held by
      // the other loader surfaces as OverlappingFileLockException instead of blocking
      boolean locked = wait.apply(() -> {
        try {
          return channel.tryLock() != null;
        } catch (OverlappingFileLockException held) {
          return false;
        }
      }).isPresent();
      if (!locked) {
        throw new IOException("Another deployment is in progress");
      }
    } catch (IOException | RuntimeException failure) {
      channel.close();
      throw failure;
    }

    return channel;
  }

  private static DeploymentResult applyDeployment(InputStream archive, String name,
      Path deployments, BBjAdminBase admin, ClasspathRelease release) throws IOException {
    String classpathName = BBjShipClasspaths.APPLICATION_CLASSPATH_PREFIX + name;
    BBjAdminAppDeploymentConfiguration configuration;
    Optional<BBjAdminAppDeploymentApplication> previous;
    List<PublishedApplication> published;
    Path previousDirectory = null;
    boolean claimed;
    try {
      configuration = admin.getRemoteConfiguration();
      applyCaseValidation(configuration, deployments, name);
      published = readOwnedApplications(configuration, deployments);
      previous = findApplication(configuration, name);
      if (previous.isPresent()) {
        previousDirectory = resolveOwnedDirectory(previous.get(), name, deployments);
      }

      claimed = previous.isEmpty() && admin.getClasspathNames().contains(classpathName);
    } catch (BBjAdminException | RuntimeException failure) {
      throw toDeploymentFailure(name, failure);
    }

    InstalledDeployment deployment =
        DeploymentStaging.writeDeployment(archive, deployments, name, published);
    List<String> previousClasspath = null;
    DeploymentRegistration previousRegistration = null;
    boolean mutationStarted = false;
    Optional<Closeable> hold = Optional.empty();
    try {
      if (previousDirectory != null) {
        applyModeValidation(deployment, previousDirectory, name);
      }

      if (previous.isPresent() || claimed) {
        previousClasspath = new ArrayList<>(admin.getClasspath(classpathName));
        // The previous classpath still resolves to the loader that holds the previous files,
        // it is closed once the previous sessions are stopped and before the new classpath replaces
        // it
        hold = release.find(classpathName);
      }

      if (previous.isPresent()) {
        previousRegistration = RegistrationMapping.createRegistration(previous.get());
      }

      final BBjAdminAppDeploymentApplication application =
          RegistrationMapping.createApplication(configuration, deployment, name, classpathName);
      mutationStarted = true;
      if (previous.isPresent()) {
        configuration.removeApplication(name);
        SessionTermination.stopProcesses(admin, classpathName);
      }

      closeHold(hold, classpathName);
      admin.setClasspath(classpathName, RegistrationMapping.createClasspath(deployment));
      admin.commit();
      application.commit();
      DeploymentFolder.writeCurrent(deployment.getDirectory());
    } catch (BBjAdminException | IOException | RuntimeException failure) {
      closeHold(hold, classpathName);
      IOException problem = toDeploymentFailure(name, failure);
      if (mutationStarted) {
        applyRegistrationRollback(admin, configuration, name, previousClasspath,
            previousRegistration, problem);
      }

      try {
        DeploymentFolder.removeDeploymentDirectory(deployment.getDirectory());
      } catch (IOException cleanup) {
        problem.addSuppressed(cleanup);
      }

      throw problem;
    }

    DeploymentFolder.removeLeftoverDirectories(deployment.getDirectory());

    return previousDirectory == null ? DeploymentResult.CREATED : DeploymentResult.REPLACED;
  }

  private static Optional<String> applyRemoval(String publishName, Path deployments,
      BBjAdminBase admin, ClasspathRelease release) throws IOException {
    String classpathName = BBjShipClasspaths.APPLICATION_CLASSPATH_PREFIX + publishName;
    Optional<Path> directory;
    Optional<Closeable> hold;
    try {
      BBjAdminAppDeploymentConfiguration configuration = admin.getRemoteConfiguration();
      Optional<BBjAdminAppDeploymentApplication> application =
          findApplication(configuration, publishName);
      directory = application.isEmpty() ? Optional.empty()
          : findOwnedDirectory(application.get(), publishName, deployments);
      boolean claimed = application.isEmpty() && admin.getClasspathNames().contains(classpathName);
      if (directory.isEmpty() && !claimed) {
        throw new DeploymentException(Reason.NOT_FOUND, "No BBj application named " + publishName
            + " was deployed by BBjShip. Check the publish name");
      }

      hold = removeApplication(admin, configuration, application, publishName, release);
    } catch (BBjAdminException | RuntimeException failure) {
      throw toDeploymentFailure(publishName, failure);
    }

    closeHold(hold, classpathName);
    if (directory.isEmpty()) {
      return Optional.empty();
    }

    return DeploymentFolder.removeDeployment(directory.get());
  }

  private static Optional<Closeable> removeApplication(BBjAdminBase admin,
      BBjAdminAppDeploymentConfiguration configuration,
      Optional<BBjAdminAppDeploymentApplication> application, String publishName,
      ClasspathRelease release) throws BBjAdminException, IOException {
    String classpathName = BBjShipClasspaths.APPLICATION_CLASSPATH_PREFIX + publishName;
    List<String> previousClasspath = new ArrayList<>(admin.getClasspath(classpathName));
    DeploymentRegistration previousRegistration =
        application.isEmpty() ? null : RegistrationMapping.createRegistration(application.get());

    // The hold is found while the classpath is still registered and closed once every session
    // of the application has ended and the classpath is gone, so no session loads from it
    Optional<Closeable> hold = release.find(classpathName);
    try {
      applyUnregistration(admin, configuration, publishName, previousClasspath,
          previousRegistration);
    } catch (IOException | RuntimeException failure) {
      closeHold(hold, classpathName);
      throw failure;
    }

    return hold;
  }

  private static void closeHold(Optional<Closeable> hold, String classpathName) {
    if (hold.isEmpty()) {
      return;
    }

    try {
      hold.get().close();
    } catch (IOException | RuntimeException failure) {
      // The removal is attempted either way, the folder report names what stayed
      LOGGER.log(Level.DEBUG,
          "The files of the BBj classpath " + classpathName + " could not be released", failure);
    }
  }

  private static FileChannel openDeploymentLock(Path deployments) throws IOException {
    return openLock(deployments,
        PollingWait.create(LOCK_SUBJECT, LOCK_TIMEOUT, LOCK_INTERVAL).build());
  }

  private static void applyNameValidation(String name) throws DeploymentException {
    Optional<String> refusal = PublishNamePolicy.findRefusal(name);
    if (refusal.isPresent()) {
      throw new DeploymentException(Reason.INVALID_NAME, refusal.get());
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

  private static void applyModeValidation(InstalledDeployment deployment, Path previousDirectory,
      String name) throws IOException {
    boolean development = !deployment.getClassFolders().isEmpty();
    boolean previousDevelopment = !DeploymentFolder.readClassFolders(previousDirectory).isEmpty();
    if (previousDevelopment && !development) {
      throw new DeploymentException(Reason.MODE_CONFLICT,
          name + " is a development run. Stop the watch first");
    }

    if (development && !previousDevelopment) {
      throw new DeploymentException(Reason.MODE_CONFLICT,
          name + " is deployed from an archive. Undeploy it first");
    }
  }

  private static List<PublishedApplication> readOwnedApplications(
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
      Optional<BBjAdminAppDeploymentApplication> application = findApplication(configuration, name);
      Optional<Path> directory = application.isEmpty() ? Optional.empty()
          : findOwnedDirectory(application.get(), name, deployments);
      if (directory.isPresent()) {
        applications.add(new PublishedApplication(name, getApplicationUrl(name), directory.get(),
            DeploymentFolder.readClassFolders(directory.get())));
      }
    }

    return applications;
  }

  private static Optional<BBjAdminAppDeploymentApplication> findApplication(
      BBjAdminAppDeploymentConfiguration configuration, String name) throws BBjAdminException {
    for (BBjAdminAppDeploymentApplication application : configuration.getApplications()) {
      if (name.equals(application.getString(BBjAdminAppDeploymentApplication.NAME))) {
        return Optional.of(application);
      }
    }

    return Optional.empty();
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
    if (!(BBjShipClasspaths.APPLICATION_CLASSPATH_PREFIX + name)
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
        || !program.equals(directory.resolve(DeploymentArchive.LAUNCHER_ENTRY))
        || !DeploymentFolder.isDeploymentId(directory)) {
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

    if (!program.toRealPath().equals(realDirectory.resolve(DeploymentArchive.LAUNCHER_ENTRY))
        || !Files.isRegularFile(program, LinkOption.NOFOLLOW_LINKS)) {
      throw createOwnershipFailure(name);
    }

    return realDirectory;
  }

  private static void applyUnregistration(BBjAdminBase admin,
      BBjAdminAppDeploymentConfiguration configuration, String name, List<String> classpath,
      DeploymentRegistration registration) throws IOException {
    String classpathName = BBjShipClasspaths.APPLICATION_CLASSPATH_PREFIX + name;
    try {
      if (registration != null) {
        configuration.removeApplication(name);
      }

      SessionTermination.stopProcesses(admin, classpathName);
      admin.setClasspath(classpathName, null);
      admin.commit();
    } catch (BBjAdminException | IOException | RuntimeException failure) {
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
      admin.setClasspath(BBjShipClasspaths.APPLICATION_CLASSPATH_PREFIX + name, classpath);
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
}
