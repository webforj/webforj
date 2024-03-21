package com.webforj.installer;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static org.apache.commons.io.FilenameUtils.normalize;

import com.webforj.installer.processor.BBjDeploymentProcessor;
import com.webforj.installer.processor.MavenProcessors;
import com.webforj.installer.processor.PomProcessor;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.maven.shared.invoker.MavenInvocationException;


/**
 * perform the installation of a Webforj app based on its JAR.
 */
public final class WebforjInstaller {

  Logger log = LogManager.getLogger(WebforjInstaller.class);

  private final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
  private final PrintStream printStream = new PrintStream(outputStream);



  void deleteDirectory(File directoryToBeDeleted) throws IOException {
    File[] allContents = directoryToBeDeleted.listFiles();
    if (allContents != null) {
      for (File file : allContents) {
        deleteDirectory(file);
      }
    }
    Files.delete(directoryToBeDeleted.toPath());
  }

  /**
   * installs the DWC project based on its JAR zipFile.
   *
   * @param sourceFilePath the path of the JAR zipFile - it's considered a temporary location. The
   *   zipFile will be removed from here.
   * @param jarFileName the name of the zipFile.
   * @param bbxdir the home directory of BBj.
   * @param deployroot The directory to which the deployment should be unpacked and installed.
   * @return The log of the activities.
   * @throws MavenInvocationException if there is a problem.
   * @throws IOException              if there is a problem.
   */
  public String install(String sourceFilePath, String jarFileName, String bbxdir, String deployroot)
    throws MavenInvocationException, IOException {
    return install(sourceFilePath, jarFileName, bbxdir, deployroot, "");
  }

  /**
   * installs the DWC project based on its JAR zipFile.
   *
   * @param sourceFilePath the path of the JAR zipFile - it's considered a temporary location. The
   *   zipFile will be removed from here.
   * @param jarFileName the name of the zipFile.
   * @param bbxdir the home directory of BBj.
   * @param deployroot The directory to which the deployment should be unpacked and installed.
   * @param requestUrl The URL from which this was invoked. Used to display the resulting link
   * @return The log of the activities.
   * @throws MavenInvocationException if there is a problem.
   * @throws IOException              if there is a problem.
   */
  public String install(String sourceFilePath, String jarFileName, String bbxdir, String deployroot,
    String requestUrl) throws MavenInvocationException, IOException {
    log.info("""
      install arguments:
        sourceFilePath = {}
        jarFileName = {}
        bbxdir = {}
        deployroot = {}
        requestUrl = {}
      """, sourceFilePath, jarFileName, bbxdir, deployroot, requestUrl);

    // Name of the jar file minus the extension, the basename.
    String normalizedJarFileName = FilenameUtils.getBaseName(normalize(jarFileName));

    // destination directory under the deployments.
    String installationDirectorySpec =
      normalize(deployroot + File.separator + normalizedJarFileName + File.separator);

    log.info("attempting to create directories for installationDirectorySpec {}",
      installationDirectorySpec);

    File installationDirectory = new File(installationDirectorySpec);
    Files.createDirectories(installationDirectory.toPath());

    // Location of the uploaded jar file.
    Path uploadedFilepath = Path.of(sourceFilePath);
    // Destination of the copied jar file
    String jarFilePathName =
      copyReplaceInstallFile(uploadedFilepath, installationDirectorySpec, jarFileName);

    // find the pom, process it.
    PomProcessor pomProcessor = new PomProcessor(jarFilePathName);
    String checksum = pomProcessor.process();

    MavenProcessors mavenProcessors = new MavenProcessors(printStream);

    String targetDependency = "target/dependency/";
    // Where to write the jars dependencies
    File jarsDependencies =
      new File("%s%s/%s".formatted(installationDirectorySpec, checksum, targetDependency));
    log.info("writing jars dependencies to {}", jarsDependencies);

    mavenProcessors.invokeMavenDependencies(jarsDependencies, pomProcessor.getPomFile(),
      deployroot);
    mavenProcessors.extractBbjProgs(jarsDependencies, installationDirectorySpec);

    // parse the discovered pom file, get the configuration details.
    Map<String, String> configuration = pomProcessor.parse();
    BBjDeploymentProcessor deploymentProcessor = new BBjDeploymentProcessor(printStream);

    deploymentProcessor.createBBjDeployment(configuration, jarsDependencies, jarFilePathName,
      installationDirectorySpec, requestUrl);

    return outputStream.toString();
  }



  /**
   * Refactored from install method, reducing cyclomatic complexity, attempt to copy the install
   * file from the uploaded location into an expected jar file destination, if unable to do that
   * instead create a sibling directory with timestamp and try there.
   *
   * @param installFilePath the source of the uploaded file.
   * @param basedir the installationDirectorySpec of the destination.
   * @param jarFileName the jar file name (minus extension).
   * @return which jarFileName we used.
   * @throws IOException normal IOException problems.
   */
  private String copyReplaceInstallFile(Path installFilePath, String basedir, String jarFileName)
    throws IOException {
    String jarFilePathName = basedir + jarFileName;
    Path destinationPath = Path.of(jarFilePathName);
    log.info("attempting to copy file {} to {} with REPLACE_EXISTING", installFilePath,
      destinationPath);
    try {
      Files.copy(installFilePath, destinationPath, REPLACE_EXISTING);
      return jarFilePathName;
    } catch (IOException e) {
      log.warn("Failed to copy source file to zip file, reason {}", e.getMessage());
      String timestampedJarFilePathName = basedir + System.currentTimeMillis() + "_" + jarFileName;
      Path timestampedJarFilePath = Path.of(timestampedJarFilePathName);
      printStream.printf("WARNING: %s was locked!%n", destinationPath);
      log.info("Trying Again! attempting to copy {} to {} with REPLACE_EXISTING", installFilePath,
        timestampedJarFilePath);
      Files.copy(installFilePath, timestampedJarFilePath, REPLACE_EXISTING);
      printStream.printf("WARNING: Using %s instead %n", timestampedJarFilePath);
      destinationPath.toFile().deleteOnExit();
      return timestampedJarFilePathName;
    }
  }

}
