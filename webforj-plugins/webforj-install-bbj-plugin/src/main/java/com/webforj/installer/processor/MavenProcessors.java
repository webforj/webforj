package com.webforj.installer.processor;

import com.webforj.installer.MavenBinaryInstaller;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.maven.shared.invoker.DefaultInvocationRequest;
import org.apache.maven.shared.invoker.DefaultInvoker;
import org.apache.maven.shared.invoker.InvocationRequest;
import org.apache.maven.shared.invoker.Invoker;
import org.apache.maven.shared.invoker.MavenInvocationException;
import org.apache.poi.openxml4j.util.ZipSecureFile;

/**
 * A class for dealing with our maven needs, such as invoking the maven install when needed,
 * invoking the dependency plugin on the uploaded jar's pom.xml, and so on.
 *
 * @author Kevin Hagel
 * @since 24.00
 */
public class MavenProcessors {

  Logger log = LogManager.getLogger(MavenProcessors.class);
  private final PrintStream printStream;

  /**
   * Constructor receives an output buffer we send to maven.
   *
   * @param printStream the print stream.
   */
  public MavenProcessors(PrintStream printStream) {
    this.printStream = printStream;
  }

  /**
   * Build a maven invoation request to obtain the dependencies in the pom file.
   *
   * @param depdir teh deploment di
   * @param pomFile the full path of the pom file.
   * @param deployroot the location to find the mvn binary.
   * @throws MavenInvocationException anything maven doesn't like.
   * @throws IOException any other exception wrapped here.
   */
  public void invokeMavenDependencies(File depdir, Path pomFile, String deployroot)
      throws MavenInvocationException, IOException {
    InvocationRequest request = new DefaultInvocationRequest();
    request.setPomFile(pomFile.toFile());
    MavenOutputHandler mavenOutputHandler = new MavenOutputHandler(printStream, true);
    log.info("Preparing maven invoication of copy-dependencies...");
    request.setOutputHandler(mavenOutputHandler).setErrorHandler(mavenOutputHandler) //
        .setGoals(List.of("dependency:copy-dependencies")) //
        .addArg("-U") //
        .addArg("-DincludeScope=compile") //
        .addArg("-DoutputDirectory=" + depdir);
    log.info("Created invocation request: args = {}", request.getArgs());
    printStream.printf("request %s%n", request.getArgs());
    String mvn = MavenBinaryInstaller.getMavenBinary(deployroot);
    printStream.printf("INFO: Using Maven %s in location %s%n", mvn, deployroot);
    Invoker invoker = new DefaultInvoker();
    invoker.setMavenHome(new File(mvn));
    printStream.printf("INFO: %s%n", request);
    log.info("invoking request {}", request);
    invoker.execute(request);
  }

  /**
   * After invoking the maven dependencies, extract the bbj files from the jars dependencies.
   *
   * @param jarsDependencies where the jars dependencies are.
   * @param installationDirectorySpec where to write the bbj files
   * @throws IOException any problems.
   */
  public void extractBbjProgs(File jarsDependencies, String installationDirectorySpec)
      throws IOException {
    log.info("extracting BBjProgs to {}", installationDirectorySpec);
    Set<String> deps = getWebforjDeps(jarsDependencies.getAbsolutePath());
    for (String nextFile : deps) {
      if ("jar".equalsIgnoreCase(FilenameUtils.getExtension(nextFile))) {
        log.info("calling unzipBBjProgs on webforj dependency {} ...", nextFile);
        unzipBbjProgs(jarsDependencies.getAbsolutePath() + File.separator + nextFile,
            installationDirectorySpec);
      }
    }
  }

  /**
   * extract the BBj progs from the JAR (they would be in bbj/ inside the jar if any are to be
   * found).
   *
   * @param zipFileSpec the JAR zipFile.
   * @param directory the target directory where they should go.
   * @throws IOException an exception with IO.
   */
  public void unzipBbjProgs(String zipFileSpec, String directory) throws IOException {
    log.info("unzipBbjProgs: zipFileSpec = {}, directory = {}", zipFileSpec, directory);
    File zipFile = new File(zipFileSpec);
    // Ensure valid path ends with system separator character for zip slip issues.
    // Normalize to deal with windows nonsense.
    String targetDirectory = FilenameUtils.normalize(directory + File.separator);
    log.info("targetDirectory = {}", targetDirectory);
    Predicate<ZipArchiveEntry> bbjFilter = zipArchiveEntry -> "bbj"
        .equalsIgnoreCase(FilenameUtils.getExtension(zipArchiveEntry.getName()));
    try (ZipSecureFile zipSecureFile = new ZipSecureFile(zipFile)) {
      ArchiveProcessor.unzip(zipSecureFile, new File(targetDirectory), bbjFilter);
    }
  }

  /**
   * the maven dependency plugin has downloaded various dependencies, some or even many of them
   * being "webforj-*.jar" dependencies. These may contain .bbj files which must be extracted.
   *
   * @param dir the directory containing the dependencies.
   * @return a set of file names matching "webforj-*.jar
   */
  public Set<String> getWebforjDeps(String dir) {
    log.info("getWebforjDeps: dir = {}", dir);
    Pattern pattern = Pattern.compile("webforj-*");
    final File[] filesList = new File(dir).listFiles();
    if (filesList == null || filesList.length == 0) {
      log.warn("No dependencies found in dir {}!  Check write permissions", dir);
      return new HashSet<>();
    }
    return Stream.of(filesList) //
        .filter(file -> !file.isDirectory()) //
        .map(File::getName) //
        .filter(pattern.asPredicate()) //
        .collect(Collectors.toSet());
  }


}
