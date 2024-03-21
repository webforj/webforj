package com.webforj.installer.processor;

import static org.apache.commons.io.FilenameUtils.normalize;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminFactory;
import java.io.File;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Map;
import java.util.Optional;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Prepares for and creates the BBj services deployment for the installer.
 */
public class BBjDeploymentProcessor {
  public static final String APP_HANDLE_PREFIX = "webforj_";
  private final PrintStream printStream;

  Logger log = LogManager.getLogger(BBjDeploymentProcessor.class);

  /**
   * Required constructor.
   *
   * @param printStream the print stream shared with the maven output handler the the installer.
   */
  public BBjDeploymentProcessor(PrintStream printStream) {
    this.printStream = printStream;
  }

  /**
   * Configures and creates the webforj application in bbj services.
   *
   * @param configuration the configuration loaded from the jar's pom.xml
   * @param jarsDependencies where the jars dependencies were written.
   * @param jarFilePathName the full path of the downloaded jar copied into the deployments.
   * @param installationDirectorySpec the root directory of the deployment.
   * @param requestUrl the post request url as given to us by the BBj services container.
   */
  public void createBBjDeployment(Map<String, String> configuration, File jarsDependencies,
      String jarFilePathName, String installationDirectorySpec, String requestUrl) {
    String appname = configuration.get("publishname");
    log.info("creating bbj deployment: jarFilePathName = {}", jarFilePathName);

    if (appname == null) {
      String normalizedJarFileName = FilenameUtils.getBaseName(normalize(jarFilePathName));
      log.info("appname is null, setting appname to normalizedJarFileName {}",
          normalizedJarFileName);
      appname = normalizedJarFileName;
    }
    String user = Optional.ofNullable(configuration.get("username")).orElse("admin");
    String password = Optional.ofNullable(configuration.get("password")).orElse("admin123");
    String token = configuration.get("token");

    try {
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
      cpEntries.add(normalize(jarsDependencies.getAbsolutePath() + File.separator, true) + "*");
      cpEntries.add(normalize(jarFilePathName, true));

      String apphandle = APP_HANDLE_PREFIX + appname.toLowerCase();
      log.info("set entries {}", cpEntries);
      api.setClasspath(apphandle, cpEntries);
      api.commit();
      printStream.printf("INFO: Written Classpath %s with %d entries", apphandle, cpEntries.size());

      // create DWC app entry
      BBjAdminAppDeploymentConfiguration config = api.getRemoteConfiguration();
      // see
      // https://documentation.basis.com/BASISHelp/WebHelp/javadocs/com/basis/api/admin/BBjAdminAppDeploymentApplication.html

      BBjAdminAppDeploymentApplication newApp = config.createApplication();
      newApp.setString(BBjAdminAppDeploymentApplication.NAME, appname);
      newApp.setString(BBjAdminAppDeploymentApplication.PROGRAM,
          installationDirectorySpec + "bbj/webforj.bbj");
      newApp.setString(BBjAdminAppDeploymentApplication.CLASSPATH, apphandle);
      newApp.setString(BBjAdminAppDeploymentApplication.WORKING_DIRECTORY,
          installationDirectorySpec + "bbj/");
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
      printStream.printf("INFO: Created App Deployment for %s%n", deployUrl);
    } catch (Exception e) {
      log.error("Error during processing!", e);
      printStream.printf("ERROR : %s%n", e);
      StringWriter sw = new StringWriter();
      e.printStackTrace(new PrintWriter(sw));
      printStream.println(sw);
    }
  }
}
