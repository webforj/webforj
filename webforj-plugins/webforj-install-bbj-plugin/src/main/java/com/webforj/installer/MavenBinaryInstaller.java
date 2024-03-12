package com.webforj.installer;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import org.apache.commons.io.IOUtils;
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

  static final String MVN_URL =
      "https://dlcdn.apache.org/maven/maven-3/3.8.8/binaries/apache-maven-3.8.8-bin.zip";
  static final String BINDIR = "apache-maven-3.8.8";

  /**
   * Fetches a copy of maven and installs it under the directory.
   *
   * @param directory where to install maven binaries.
   * @throws IOException in case of a problem.
   */
  static void installMaven(String directory) throws IOException {
    log.info("receiving directory {}", directory);
    String fileZip = directory + "/mvn.zip";


    if (Files.exists(Path.of(fileZip))) {
      Files.delete(Path.of(fileZip));
    }
    log.info("copying {} to {}", new URL(MVN_URL), Paths.get(fileZip));
    Files.copy(new URL(MVN_URL).openStream(), Paths.get(fileZip));

    File destDir = new File(directory);
    byte[] buffer = new byte[1024];
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
            // int len;
            // while ((len = zis.read(buffer)) > 0) {
            // fos.write(buffer, 0, len);
            // }
          }
        }
        zipEntry = zis.getNextEntry();
      }
      zis.closeEntry();
    }

    // fix the execution flags for Mac and Linux
    log.info("Fixing execution flags for Mac and linux");
    Files.setPosixFilePermissions(Path.of(directory + "/" + BINDIR + "/bin/mvn.cmd"),
        PosixFilePermissions.fromString("rwxrwxr-x"));

    Files.setPosixFilePermissions(Path.of(directory + "/" + BINDIR + "/bin/mvn"),
        PosixFilePermissions.fromString("rwxrwxr-x"));

    Files.delete(Path.of(fileZip));
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
      installMaven(directory);
    }
    return d;
  }


}
