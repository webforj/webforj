package com.webforj.installer;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminFactory;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.maven.shared.invoker.DefaultInvocationRequest;
import org.apache.maven.shared.invoker.DefaultInvoker;
import org.apache.maven.shared.invoker.InvocationRequest;
import org.apache.maven.shared.invoker.Invoker;
import org.apache.maven.shared.invoker.MavenInvocationException;
import org.apache.maven.shared.utils.StringUtils;


/**
 * perform the installation of a Webforj app based on its JAR.
 */
public final class WebforjInstaller {

  Logger log = LogManager.getLogger(WebforjInstaller.class);
  // buffer size
  public static final String APP_HANDLE_PREFIX = "webforj_";

  public static final String POM_XML = "pom.xml";

  // a central StringBuilder instance to obtain the log
  private final StringBuilder out = new StringBuilder();

  /**
   * extract the POM file from the jar.
   *
   * @param zipFile the project's jar
   * @throws IOException when io fails.
   */
  private String unzipPom(String zipFile) throws IOException {
    log.info("extracting pom! zipFile = {}", zipFile);
    var checksum = "";
    out.append("webforj-installer: extracting pom.xml\n");

    File file = new File(zipFile);
    ZipFile zip = new ZipFile(file);
    Path path = Paths.get(zipFile);
    String directory = path.getParent().toString();
    Enumeration<? extends ZipEntry> zipFileEntries = zip.entries();

    log.info("Searching zipfile for pom.xml");
    // Process each entry
    while (zipFileEntries.hasMoreElements()) {

      // grab a zip file entry
      ZipEntry entry = zipFileEntries.nextElement();

      String currentEntry = entry.getName();
      if (!currentEntry.endsWith(POM_XML)) {
        continue;
      }
      log.info("found pom.xml in entry {}", currentEntry);
      File destFile = new File(directory, POM_XML);

      // write the current file to disk
      log.info("Writing entry to disk...");
      copyZipEntryToFile(zip, entry, destFile);
    }
    zip.close();

    try {
      byte[] data = Files.readAllBytes(Paths.get(directory + "/" + POM_XML));
      byte[] hash = MessageDigest.getInstance("MD5").digest(data);
      checksum = new BigInteger(1, hash).toString(16);
    } catch (Exception e) {
      log.error("Exception handling unzip pom", e);
      throw new IOException(e);
    }
    return checksum;
  }

  /**
   * extract the BBj progs from the JAR (they would be in /bbj inside).
   *
   * @param zipFile the JAR file.
   * @param directory the directors where they should go.
   * @throws IOException an exception with IO.
   */
  private void unzipBbjProgs(String zipFile, String directory) throws IOException {
    log.info("uzipBbjProgs: zipFile = {}, directory = {}", zipFile, directory);
    File file = new File(zipFile);
    // Ensure valid path ends with system separator character for zip slip issues.
    String targetDirectory = StringUtils.endsWithIgnoreCase(directory, File.separator) ? directory
        : directory.concat(File.separator);
    log.info("directory = {}, targetDirectory = {}", directory, targetDirectory);

    try (ZipFile zip = new ZipFile(file)) {
      Enumeration<? extends ZipEntry> zipFileEntries = zip.entries();
      log.info("processing zip entries ...");
      // Process each entry
      while (zipFileEntries.hasMoreElements()) {
        // grab a zip file entry
        ZipEntry entry = zipFileEntries.nextElement();

        String currentEntry = FilenameUtils.normalize(entry.getName());
        if (!"bbj".equalsIgnoreCase(FilenameUtils.getExtension(currentEntry))) {
          continue;
        }

        File destFile = new File(targetDirectory + currentEntry);
        String canonicalDestinationPath = destFile.getCanonicalPath();
        if (canonicalDestinationPath.startsWith(targetDirectory)) {
          log.info("extracting {}", canonicalDestinationPath);
          out.append("webforj-installer: extracting %s%n".formatted(canonicalDestinationPath));
          Files.deleteIfExists(destFile.toPath());
          File parent = destFile.getParentFile();
          log.info("testing parent directory {}, attempting to create if not exists", parent);
          if (!parent.exists() && !parent.mkdirs()) {
            throw new IOException("Could not create parent directories " + parent);
          }

          log.info("writing current file {} to disk", destFile);
          copyZipEntryToFile(zip, entry, destFile);
        }

      }
    }
  }

  /**
   * Copy the zip entry from the zip file to the destFile location.
   *
   * @param zip the zip file.
   * @param entry the zip entry.
   * @param destFile the destination of the zip entry.
   * @throws IOException input stream or zip errors.
   */
  private void copyZipEntryToFile(ZipFile zip, ZipEntry entry, File destFile) throws IOException {
    log.info("copying entry = {} destFile = {}}", entry, destFile);
    try (InputStream is = zip.getInputStream(entry)) {
      Files.copy(is, destFile.toPath(), REPLACE_EXISTING);
    }

  }

  private Set<String> getWebforjDeps(String dir) {
    log.info("getWebforjDeps: dir = {}", dir);
    Pattern pattern = Pattern.compile("webforj-*");
    out.append("webforj-installer: Scanning Dependencies in %s!%n".formatted(dir));
    final File[] filesList = new File(dir).listFiles();
    if (filesList == null || filesList.length == 0) {
      log.warn("No dependencies found in dir {}!  Check write permissions", dir);
      out.append("ERROR: No Dependencies found in dir %s!%n".formatted(dir));
      out.append("ERROR: Check write permissions? \n");
      return new HashSet<>();
    }
    return Stream.of(filesList).filter(file -> !file.isDirectory()).map(File::getName)
        .filter(pattern.asPredicate()).collect(Collectors.toSet());
  }


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
   * installs the DWC project based on its JAR file.
   *
   * @param sourceFilePath the path of the JAR file - it's considered a temporary location. The file
   *        will be removed from here.
   * @param jarFileName the name of the file.
   * @param bbxdir the home directory of BBj.
   * @param deployroot The directory to which the deployment should be unpacked and installed.
   * @return The log of the activities.
   * @throws MavenInvocationException if there is a problem.
   * @throws IOException if there is a problem.
   */
  public String install(String sourceFilePath, String jarFileName, String bbxdir, String deployroot)
      throws MavenInvocationException, IOException {
    return install(sourceFilePath, jarFileName, bbxdir, deployroot, "");
  }

  /**
   * installs the DWC project based on its JAR file.
   *
   * @param sourceFilePath the path of the JAR file - it's considered a temporary location. The file
   *        will be removed from here.
   * @param jarFileName the name of the file.
   * @param bbxdir the home directory of BBj.
   * @param deployroot The directory to which the deployment should be unpacked and installed.
   * @param requestUrl The URL from which this was invoked. Used to display the resulting link
   * @return The log of the activities.
   * @throws MavenInvocationException if there is a problem.
   * @throws IOException if there is a problem.
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


    String basedir =
        deployroot + jarFileName.substring(0, jarFileName.toLowerCase().indexOf(".jar")) + "/";

    log.info("attempting to create directories for basedir {}", basedir);

    File basdirFile = new File(basedir);
    if (!basdirFile.exists() && (!basdirFile.mkdirs())) {
      throw new IOException("Unable to create basedir " + basdirFile);
    }

    String zipFilePath = basedir + jarFileName;
    Path p0 = Path.of(sourceFilePath);
    log.info("attempting to copy {} to {} with REPLACE_EXISTING", p0, Path.of(zipFilePath));

    try {
      Files.copy(p0, Path.of(zipFilePath), REPLACE_EXISTING);
    } catch (IOException e) {
      log.warn("Failed to copy source file to zip file, reason {}", e.getMessage());
      String tmp = zipFilePath;
      zipFilePath = basedir + System.currentTimeMillis() + "_" + jarFileName;
      out.append("WARNING: %s was locked!%n".formatted(tmp));
      log.info("Trying Again! attempting to copy {} to {} with REPLACE_EXISTING", p0,
          Path.of(zipFilePath));
      Files.copy(p0, Path.of(zipFilePath), REPLACE_EXISTING);
      out.append("WARNING: Using %s instead %n".formatted(zipFilePath));
      new File(tmp).deleteOnExit();
    }


    String pomFile = basedir + POM_XML;
    String checksum = unzipPom(zipFilePath);

    String targetDependency = "target/dependency/";
    File depdir = new File("%s%s/%s".formatted(basedir, checksum, targetDependency));
    InvocationRequest request = new DefaultInvocationRequest();
    request.setPomFile(new File(pomFile));
    request.setOutputHandler(new MavenOutputHandler(out))
        .setErrorHandler(new MavenOutputHandler(out))
        .setGoals(List.of("dependency:copy-dependencies")).addArg("-U")
        .addArg("-DincludeScope=compile").addArg("-DoutputDirectory=" + depdir);
    log.info("Created request: args = {}", request.getArgs());
    out.append("request ").append(request.getArgs()).append("\n");
    String mvn = MavenBinaryInstaller.getMavenBinary(deployroot);
    out.append("INFO: Using Maven in location ").append(mvn).append("\n");
    Invoker invoker = new DefaultInvoker();
    invoker.setMavenHome(new File(mvn));

    out.append("INFO: %s%n".formatted(request));
    log.info("invoking request {}", request);
    invoker.execute(request);

    try {
      Set<String> deps = getWebforjDeps(depdir.getAbsolutePath());
      for (String nextFile : deps) {
        if (nextFile.toLowerCase().endsWith(".jar")) {
          log.info("processing dependency {}", nextFile);
          unzipBbjProgs(depdir.getAbsolutePath() + "/" + nextFile, basedir);
        }
      }

      PomParser pomParser = new PomParser(pomFile);
      Map<String, String> configuration = pomParser.parse();
      String appname = configuration.get("publishname");

      if (appname == null) {
        appname = jarFileName.substring(0, jarFileName.toLowerCase().indexOf(".jar"));
      }
      String user = Optional.ofNullable(configuration.get("username")).orElse("admin");
      String password = Optional.ofNullable(configuration.get("password")).orElse("admin123");
      String token = configuration.get("token");

      BBjAdminBase api;
      if (token != null) {
        api = BBjAdminFactory.getBBjAdmin(token);
      } else {
        api = BBjAdminFactory.getBBjAdmin(user, password);
      }


      // create BBj classpath
      log.info("Creating BBj classpath");
      ArrayList<String> cpEntries = new ArrayList<>();
      cpEntries.add("(bbj_default)");
      cpEntries.add(depdir.getAbsolutePath() + "/*");
      cpEntries.add(zipFilePath);

      String apphandle = APP_HANDLE_PREFIX + appname.toLowerCase();
      log.info("set entries {}", cpEntries);
      api.setClasspath(apphandle, cpEntries);
      api.commit();
      out.append(
          "INFO: Written Classpath %s with %d entries".formatted(apphandle, cpEntries.size()));

      // create DWC app entry
      BBjAdminAppDeploymentConfiguration config = api.getRemoteConfiguration();
      // see
      // https://documentation.basis.com/BASISHelp/WebHelp/javadocs/com/basis/api/admin/BBjAdminAppDeploymentApplication.html

      BBjAdminAppDeploymentApplication newApp = config.createApplication();
      newApp.setString(BBjAdminAppDeploymentApplication.NAME, appname);
      newApp.setString(BBjAdminAppDeploymentApplication.PROGRAM, basedir + "bbj/webforj.bbj");
      newApp.setString(BBjAdminAppDeploymentApplication.CLASSPATH, apphandle);
      // newApp.setString(BBjAdminAppDeploymentApplication.CONFIG_FILE, "/path/to/config.bbx")
      newApp.setString(BBjAdminAppDeploymentApplication.WORKING_DIRECTORY, basedir + "bbj/");
      newApp.setBoolean(BBjAdminAppDeploymentApplication.EXE_ENABLED, false);
      newApp.setBoolean(BBjAdminAppDeploymentApplication.BUI_ENABLED, false);
      newApp.setBoolean(BBjAdminAppDeploymentApplication.DWC_ENABLED, true);


      Boolean debug =
          configuration.get("debug") != null && configuration.get("debug").equals("true");
      if (Boolean.TRUE.equals(debug)) {
        newApp.getArguments().add("DEBUG");
      }

      String classname = configuration.get("classname");
      if (classname != null && !classname.isEmpty()) {
        newApp.getArguments().add("class=" + classname);
      }

      log.info("committing newApp {}", newApp);
      newApp.commit();

      String deployUrl = "http://localhost:8888/webapp/" + appname;
      if (!requestUrl.isEmpty()) {
        deployUrl = requestUrl + "/webapp/" + appname;
      }
      out.append("INFO: Created App Deployment for %s%n ".formatted(deployUrl));
    } catch (Exception e) {
      log.error("Error during processing!", e);
      out.append("ERROR : %s%n".formatted(e.toString()));
      StringWriter sw = new StringWriter();
      e.printStackTrace(new PrintWriter(sw));
      out.append(sw);
      return out.toString();
    }

    return out.toString();
  }

}
