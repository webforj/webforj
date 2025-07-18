package com.webforj.installer;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermission;
import java.util.HashSet;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Installs a copy of maven into the basedir, or passes a handle to its bin dir when it's there.
 */
public class MavenBinaryInstaller {
  static Logger log = LogManager.getLogger(MavenBinaryInstaller.class);

  private MavenBinaryInstaller() {
    // this class has only static methods
  }

  static final String MVN_URL = "https://cdn.webforj.com/webforj/apache-maven-3.9.11-bin.zip";
  static final String BINDIR = "apache-maven-3.9.11";

  /**
   * Fetches a copy of maven and installs it under the directory.
   *
   * @param directory where to install maven binaries.
   * @throws IOException in case of a problem.
   */
  static void installMaven(String directory) throws IOException, URISyntaxException {
    log.info("receiving directory {}", directory);
    String fileZip = FilenameUtils.normalize(directory + "/mvn.zip");


    Path fileZipPath = Path.of(fileZip);
    if (Files.exists(fileZipPath)) {
      Files.delete(fileZipPath);
    }
    URL mavenUrl = new URI(MVN_URL).toURL();

    log.info("copying {} to {}", mavenUrl, fileZipPath);
    Files.copy(mavenUrl.openStream(), fileZipPath);

    File destDir = new File(directory);
    log.info("attempting to open zip input stream in {}", fileZip);
    try (ZipInputStream zis = new ZipInputStream(new FileInputStream(fileZip))) {
      ZipEntry zipEntry = zis.getNextEntry();
      while (zipEntry != null) {
        File newFile = new File(destDir, zipEntry.getName());
        log.info("processing zipEntry {} as {}", zipEntry, newFile);
        if (zipEntry.isDirectory()) {
          if (!newFile.isDirectory() && !newFile.mkdirs()) {
            log.error("Failed to create directory {}", newFile);
            throw new IOException("Failed to create directory " + newFile);
          }
        } else {
          // fix for Windows-created archives
          File parent = newFile.getParentFile();
          if (!parent.isDirectory() && !parent.mkdirs()) {
            log.error("Failed to create directory {}", newFile);
            throw new IOException("Failed to create directory " + parent);
          }

          // write file content
          try (FileOutputStream fos = new FileOutputStream(newFile)) {
            IOUtils.copy(zis, fos);
          }
        }
        zipEntry = zis.getNextEntry();
      }
      zis.closeEntry();
    }

    // fix the execution flags for Mac and Linux
    if (SystemUtils.IS_OS_MAC || SystemUtils.IS_OS_LINUX) {
      log.info("Fixing execution flags for Mac and linux");

      Set<PosixFilePermission> perms = new HashSet<>();
      // user permission
      perms.add(PosixFilePermission.OWNER_READ);
      perms.add(PosixFilePermission.OWNER_WRITE);
      perms.add(PosixFilePermission.OWNER_EXECUTE);
      // group permissions
      perms.add(PosixFilePermission.GROUP_READ);
      perms.add(PosixFilePermission.GROUP_EXECUTE);
      // others permissions removed
      perms.add(PosixFilePermission.OTHERS_READ); // Non-Compliant
      perms.remove(PosixFilePermission.OTHERS_WRITE); // Compliant
      perms.add(PosixFilePermission.OTHERS_EXECUTE); // Compliant

      Files.setPosixFilePermissions(Path.of(directory, BINDIR, "bin", "mvn"), perms);
    }


    Files.delete(fileZipPath);
  }

  /**
   * Returns the maven binary location. This method internally installs maven binaries if not
   * presen.
   *
   * @param directory where the binaries should be.
   * @return the location of maven on disk as a String containing the path.
   * @throws IOException when something's wrong.
   */
  static String getMavenBinary(String directory) throws IOException {
    String d = directory + "/" + BINDIR + "/bin/mvn";
    if (!Files.exists(Path.of(d))) {
      try {
        installMaven(directory);
      } catch (URISyntaxException e) {
        throw new IOException(e);
      }
    }
    return d;
  }


}
