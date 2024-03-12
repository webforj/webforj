package com.webforj.installer;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminFactory;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.maven.shared.invoker.DefaultInvocationRequest;
import org.apache.maven.shared.invoker.DefaultInvoker;
import org.apache.maven.shared.invoker.InvocationRequest;
import org.apache.maven.shared.invoker.Invoker;
import org.apache.maven.shared.invoker.MavenInvocationException;
import org.xml.sax.SAXException;

/**
 * perform the installation of a Webforj app based on its JAR.
 */
public final class WebforjInstaller {

  Logger log = LogManager.getLogger(WebforjInstaller.class);
  // buffer size
  public static final int BUFFER = 2048;
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

      zipBufferedInputStream(zip, entry, destFile);


    }

    zip.close();

    try {
      byte[] data = Files.readAllBytes(Paths.get(directory + "/" + POM_XML));
      byte[] hash = MessageDigest.getInstance("MD5").digest(data);
      checksum = new BigInteger(1, hash).toString(16);
    } catch (Exception e) {
      log.error("Exception handling unzip pom", e);
      throw new RuntimeException(e);
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
    try (ZipFile zip = new ZipFile(file)) {

      Enumeration<? extends ZipEntry> zipFileEntries = zip.entries();

      log.info("processing entries");
      // Process each entry
      while (zipFileEntries.hasMoreElements()) {
        // grab a zip file entry
        ZipEntry entry = zipFileEntries.nextElement();

        String currentEntry = entry.getName();
        if (!currentEntry.endsWith(".bbj")) {
          continue;
        }

        File destFile = new File(directory, entry.getName());
        log.info("extracting {}", destFile.getAbsoluteFile());
        out.append("webforj-installer: extracting ").append(destFile.getAbsolutePath())
            .append("\n");
        Files.deleteIfExists(destFile.toPath());
        Paths.get(destFile.getPath()).getParent().toFile().mkdirs();

        log.info("writing current file {} to disk", destFile);
        zipBufferedInputStream(zip, entry, destFile);

      }
    }
  }

  private void zipBufferedInputStream(ZipFile zip, ZipEntry entry, File destFile)
      throws IOException {
    try (BufferedInputStream bis = new BufferedInputStream(zip.getInputStream(entry));
        FileOutputStream fos = new FileOutputStream(destFile);
        BufferedOutputStream dest = new BufferedOutputStream(fos, BUFFER)) {
      int currentByte;
      // establish buffer for writing file
      byte[] data = new byte[BUFFER];
      // read and write until last byte is encountered
      while ((currentByte = bis.read(data, 0, BUFFER)) != -1) {
        dest.write(data, 0, currentByte);
      }
      dest.flush();
    }
  }

  private Set<String> getWebforjDeps(String dir) {
    log.info("getWebforjDeps: dir = {}", dir);
    Pattern pattern = Pattern.compile("webforj-*");
    out.append("webforj-installer: Scanning Dependencies in " + dir + "!\n");
    final File[] filesList = new File(dir).listFiles();
    if (filesList == null || filesList.length == 0) {
      log.warn("No dependencies found in dir {}!  Check write permissions", dir);
      out.append("ERROR: No Dependencies found in " + dir + "!\n");
      out.append("ERROR: Check write permissions? \n");
      return new HashSet<>();
    }
    return Stream.of(filesList).filter(file -> !file.isDirectory()).map(File::getName)
        .filter(pattern.asPredicate()).collect(Collectors.toSet());
  }


  boolean deleteDirectory(File directoryToBeDeleted) {
    File[] allContents = directoryToBeDeleted.listFiles();
    if (allContents != null) {
      for (File file : allContents) {
        deleteDirectory(file);
      }
    }
    return directoryToBeDeleted.delete();
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
   * @throws ParserConfigurationException if there is a problem.
   * @throws SAXException if there is a problem.
   * @throws NoSuchAlgorithmException if there is a problem.
   */
  public String install(String sourceFilePath, String jarFileName, String bbxdir, String deployroot)
      throws MavenInvocationException, IOException, ParserConfigurationException, SAXException,
      NoSuchAlgorithmException {
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

    new File(basedir).mkdirs();

    String zipFilePath = basedir + jarFileName;
    log.info("attempting to copy {} to {} with REPLACE_EXISTING", Path.of(sourceFilePath),
        Path.of(zipFilePath));

    try {
      Files.copy(Path.of(sourceFilePath), Path.of(zipFilePath), REPLACE_EXISTING);
    } catch (IOException e) {
      log.warn("Failed to copy source file to zip file, reason {}", e.getMessage());
      String tmp = zipFilePath;
      zipFilePath = basedir + String.valueOf(System.currentTimeMillis()) + "_" + jarFileName;
      out.append("WARNING: " + tmp + " was locked!\n");
      log.info("Trying Again! attempting to copy {} to {} with REPLACE_EXISTING",
          Path.of(sourceFilePath), Path.of(zipFilePath));
      Files.copy(Path.of(sourceFilePath), Path.of(zipFilePath), REPLACE_EXISTING);
      out.append("WARNING: Using " + zipFilePath + " instead.\n");
      new File(tmp).deleteOnExit();
    }


    String pomFile = basedir + POM_XML;
    String checksum = unzipPom(zipFilePath);

    String targetDependency = "target/dependency/";
    File depdir = new File("%s%s/%s".formatted(basedir, checksum, targetDependency));
    if (depdir.exists()) {
      log.info("pom.xml not changed. No need to run Maven but running anyway.");
      out.append(
          "webforj-installer: pom.xml not changed. No need to run Maven but running anyway.\n");
    }
    InvocationRequest request = new DefaultInvocationRequest();
    request.setPomFile(new File(pomFile));
    request.setOutputHandler(new MavenOutputHandler(out))
        .setErrorHandler(new MavenOutputHandler(out))
        .setGoals(List.of("dependency:copy-dependencies")).addArg("-U")
        .addArg("-DincludeScope=compile").addArg("-DoutputDirectory=" + depdir);
    Invoker invoker = new DefaultInvoker();
    log.info("Created request: args = {}", request.getArgs());
    out.append("request " + request.getArgs() + "\n");
    String mvn = MavenBinaryInstaller.getMavenBinary(deployroot);
    out.append("INFO: Using Maven in location " + mvn + "\n");
    invoker.setMavenHome(new File(mvn));

    out.append("INFO: " + request + "\n");
    log.info("invoking request {}", request);
    invoker.execute(request);

    try {
      Set<String> deps = getWebforjDeps(depdir.getAbsolutePath());
      Iterator<String> it = deps.iterator();
      while (it.hasNext()) {
        String nextFile = it.next();
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
          "INFO: Written Classpath " + apphandle + " with " + cpEntries.size() + " entries\n");

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
      out.append("INFO: Created App Deployment for  " + deployUrl + "\n");
    } catch (Exception e) {
      log.error("Error during processing!", e);
      out.append("ERROR : " + e.toString());
      StringWriter sw = new StringWriter();
      e.printStackTrace(new PrintWriter(sw));
      out.append(sw.toString());
      return out.toString();
    }

    return out.toString();
  }

}
