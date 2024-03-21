package com.webforj.installer.processor;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import java.util.function.Predicate;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.openxml4j.util.ZipSecureFile;

/**
 * A processor for extracting a pom.xml from an archive, with zip bomb detection and zip slip
 * detection.
 */
public class PomProcessor {
  public static final String POM_XML = "pom.xml";

  private final Logger log = LogManager.getLogger(PomProcessor.class);
  private final Path jarFileSpec;

  /**
   * Filter used when extracting the pom.xml file from a zip archive.
   */
  private final Predicate<ZipArchiveEntry> pomFilter =
      zipArchiveEntry -> POM_XML.equalsIgnoreCase(FilenameUtils.getName(zipArchiveEntry.getName()));


  /**
   * Keep track of the extracted pom file, full path to it.
   */
  private Path pomFile;


  /**
   * This pom processor only operates once on the file, but we must track what we have done.
   *
   * @param jarFileSpec the specific location of the jar file.
   */
  public PomProcessor(Path jarFileSpec) {
    this.jarFileSpec = jarFileSpec;
  }

  /**
   * This pom processor only operates once on the file, but we must track what we have done.
   *
   * @param jarFileSpec the specific location of the jar file.
   */
  public PomProcessor(String jarFileSpec) {
    this(Path.of(jarFileSpec));
  }

  /**
   * Parse the pom file, discovering webforj-install-maven-plugin declaration and scraping the
   * configuration children.
   *
   * @return a map whose keys are the name of the config child and value is the text content.
   * @throws IOException wrapping any other exception that happens.
   */
  public Map<String, String> parse() throws IOException {
    if (pomFile == null) {
      throw new IOException("No valid pom file has been processed, cannot extract configuration");
    }
    PomParser pomParser = new PomParser(pomFile.toFile());
    return pomParser.parse();
  }

  /**
   * Public method for processing the zip file, extracting one pom.xml from it, writing it to the
   * indicated target directory.
   *
   * @return the checksum of the written pom file, if successfully extracted.
   * @throws IOException describing any exception condition, consider it fatal.
   */
  public String process() throws IOException {
    log.info("extracting pom! jarFileSpec = {}", jarFileSpec);
    File zipFile = jarFileSpec.toFile();

    Path pomFileDirectory = zipFile.getParentFile().toPath();
    pomFile = pomFileDirectory.resolve(POM_XML);
    log.info("pomFile = {}", pomFile);

    try (ZipSecureFile zipSecureFile = new ZipSecureFile(zipFile)) {
      Collection<ZipArchiveEntry> poms = ArchiveProcessor.extract(zipSecureFile, pomFilter);
      if (poms.size() != 1) {
        throw new IOException("Missing or multiple pom.xml files in archive");
      }
      ZipArchiveEntry zipArchiveEntry = poms.iterator().next();
      ArchiveProcessor.copyZipArchiveEntryToFile(zipSecureFile, zipArchiveEntry, pomFile.toFile());
      return new DigestUtils(MessageDigestAlgorithms.MD5).digestAsHex(pomFile);
    }
  }

  /**
   * Return the path of the created pom file.
   *
   * @return the full path of the pom file.
   */
  public Path getPomFile() {
    return pomFile;
  }
}
