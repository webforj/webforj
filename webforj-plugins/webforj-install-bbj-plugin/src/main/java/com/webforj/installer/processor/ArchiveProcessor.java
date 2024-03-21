package com.webforj.installer.processor;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;
import java.util.function.Predicate;
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.openxml4j.util.ZipSecureFile;

/**
 * Static methods provide a processor for safely reading zip and jar archive files, with zip bomb
 * detection and other zip attack protections. This class uses the Apache poi-ooxml library.
 *
 * @author Kevin Hagel
 * @see <a href="https://en.wikipedia.org/wiki/Zip_bomb">Zip Bomb</a>
 * @see <a href="https://security.snyk.io/research/zip-slip-vulnerability">Zip Slip
 *   Vulnerability</a>
 * @since 24.00
 */
public abstract class ArchiveProcessor {

  /**
   * Maximum number of entries allowed.
   */
  private static final int DEFAULT_MAX_FILE_COUNT = 10000;
  static final int MAX_FILE_COUNT = DEFAULT_MAX_FILE_COUNT;

  private static final Logger log = LogManager.getLogger(ArchiveProcessor.class);

  /**
   * Protect us from instnatiation.
   */
  private ArchiveProcessor() {
  }

  /**
   * Extract elements from the zip archive passing your filter, returning the elements in a
   * collection.
   *
   * @param zipSecureFile the source zip file.
   * @param filter the filter.
   * @return a collection of extracted zip archive entry elements pssing your filter.
   */
  public static Collection<ZipArchiveEntry> extract(ZipSecureFile zipSecureFile,
    Predicate<ZipArchiveEntry> filter) {
    List<ZipArchiveEntry> extracted = new ArrayList<>();
    Enumeration<ZipArchiveEntry> zipSecureFileEntries = zipSecureFile.getEntries();
    while (zipSecureFileEntries.hasMoreElements()) {
      ZipArchiveEntry zipArchiveEntry = zipSecureFileEntries.nextElement();
      if (filter.test(zipArchiveEntry)) {
        extracted.add(zipArchiveEntry);
      }
    }
    return extracted;
  }


  /**
   * Unzip the given zip file, writing filtered elements to the destination folder.
   *
   * @param zipSecureFile the zip file.
   * @param targetDirectory the destination folder.
   * @param filter your filter, this method will inovke to and only those elements passing will be
   *   written.
   * @throws IOException describing any exception.
   */
  public static void unzip(ZipSecureFile zipSecureFile, File targetDirectory,
    Predicate<ZipArchiveEntry> filter) throws IOException {
    Long numberEntries = 0L;
    log.info("Unzipping zip file {} to folder {}", zipSecureFile.getName(), targetDirectory);
    Enumeration<ZipArchiveEntry> zipSecureFileEntries = zipSecureFile.getEntries();
    while (zipSecureFileEntries.hasMoreElements()) {
      if (MAX_FILE_COUNT <= ++numberEntries) {
        throw new IOException(
          "Total number entries %d exceeds maximum allowed ".formatted(numberEntries));
      }
      ZipArchiveEntry zipArchiveEntry = zipSecureFileEntries.nextElement();
      if (filter.test(zipArchiveEntry)) {
        persistArchiveEntry(zipSecureFile, zipArchiveEntry, targetDirectory);
      }
    }
  }


  /**
   * Unzips the entries in the given zip file, inflating and writing to the destination folder. Zip
   * Slip tests are done on entries to prevent them from being written to the file system. The zip
   * archive stream measures total size of each entry, expansion ratio, and total number of entries
   * to protect from the Zip Bomb.
   *
   * @param srcZipFile the source zip file, the archive.
   * @param destFolder the destination for the contents of the archive.
   * @throws IOException wraps any problems.
   */
  public static void unzip(String srcZipFile, String destFolder) throws IOException {
    File targetDirectory = new File(FilenameUtils.normalize(destFolder + File.separator));
    Long numberEntries = 0L;
    log.info("Unzipping zip file {} to folder {}", srcZipFile, destFolder);
    try (ZipSecureFile zipSecureFile = new ZipSecureFile(srcZipFile)) {
      Enumeration<ZipArchiveEntry> zipSecureFileEntries = zipSecureFile.getEntries();
      while (zipSecureFileEntries.hasMoreElements()) {
        ZipArchiveEntry zipArchiveEntry = zipSecureFileEntries.nextElement();
        if (MAX_FILE_COUNT <= ++numberEntries) {
          throw new IOException(
            "Total number entries %d exceeds maximum allowed ".formatted(numberEntries));
        }
        persistArchiveEntry(zipSecureFile, zipArchiveEntry, targetDirectory);
      }
    } catch (Exception e) {
      log.error("error!", e);
      throw e;
    }
  }

  /**
   * Persist an archive entry in the target directory, this method works for directory archive
   * entries as well as file archive entries.
   *
   * @param zipSecureFile the source zip secure file.
   * @param zipArchiveEntry the specific archive entry.
   * @param targetDirectory the destination target diretory.
   * @throws IOException any IO difficulties.
   */
  public static void persistArchiveEntry(ZipSecureFile zipSecureFile,
    ZipArchiveEntry zipArchiveEntry, File targetDirectory) throws IOException {
    String currentEntry = zipArchiveEntry.getName();
    File destFile = safeGetFile(targetDirectory, currentEntry);
    try (InputStream inputStream = zipSecureFile.getInputStream(zipArchiveEntry)) {
      if (zipArchiveEntry.isDirectory()) {
        log.info("creating directory {}", destFile);
        Files.createDirectories(destFile.toPath());
      } else {
        log.info("copying zipArchiveEntry {} to file {}", zipArchiveEntry, destFile);
        File parent = destFile.getParentFile();
        Files.createDirectories(parent.toPath());
        Files.copy(inputStream, destFile.toPath(), REPLACE_EXISTING);
      }
    }
  }



  /**
   * Create a file/directory object, with zip slip detection.
   *
   * @param targetDirectory the target directory, the parent.
   * @param currentEntry the current zip entry.
   * @return the file or directory.
   * @throws IOException normal IO problems as well as Zip Slip detected.
   */
  public static File safeGetFile(File targetDirectory, String currentEntry) throws IOException {
    File destFile = new File(targetDirectory + File.separator + currentEntry);
    String canonicalDestinationPath = FilenameUtils.normalize(destFile.getCanonicalPath());
    if (!FilenameUtils.directoryContains(targetDirectory.getCanonicalPath(),
      canonicalDestinationPath)) {
      throw new IOException("ZIP SLIP Detected.  The currentEntry " + currentEntry + " is invalid");
    }
    return destFile;
  }

  /**
   * Copy the zip archive entry from the zip secure file to the destFile location, creating any
   * parent directories needed.
   *
   * @param zip the zip file.
   * @param entry the zip entry.
   * @param destFile the destination of the zip entry.
   * @throws IOException input stream or zip errors.
   */
  public static void copyZipArchiveEntryToFile(ZipSecureFile zip, ZipArchiveEntry entry,
    File destFile) throws IOException {
    log.info("copying entry = {} destFile = {}}", entry, destFile);
    try (InputStream is = zip.getInputStream(entry)) {
      Files.createDirectories(destFile.getParentFile().toPath());
      Files.copy(is, destFile.toPath(), REPLACE_EXISTING);
    }
  }


}
