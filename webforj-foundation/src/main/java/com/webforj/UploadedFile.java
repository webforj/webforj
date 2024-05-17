package com.webforj;

import com.basis.bbj.proxies.BBjClientFile;
import com.basis.bbj.proxies.BBjClientFileSystem;
import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import java.io.File;
import java.io.IOException;
import java.text.Normalizer;
import java.util.Objects;

/**
 * Represents an uploaded file.
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
public final class UploadedFile {
  private final String name;
  private static final int MAX_FILENAME_LENGTH = 255;

  /**
   * Creates a new uploaded file with the given client file name.
   *
   * @param name the file name
   */
  public UploadedFile(String name) {
    this.name = name;
  }

  /**
   * Gets the client file name as it was on the client machine.
   *
   * <p>
   * It is extracted from the request from which the file has been uploaded. This should not be
   * considered as a safe value to use for a file name on the server. For a safe file name, use
   * {@link #getSanitizedClientName()} instead.
   * </p>
   *
   * @return the client file name
   * @see #getSanitizedClientName()
   */
  public String getClientName() {
    return name;
  }

  /**
   * Gets a sanitized client file name.
   *
   * <p>
   * The method will return a normalized version of the client file name which suitable to be used
   * as a file name on the server.
   * </p>
   *
   * @return A sanitized version of the client file name
   */
  public String getSanitizedClientName() {
    // Normalize the file name
    String normalized = Normalizer.normalize(getClientName(), Normalizer.Form.NFKD);

    // Remove all non-ASCII characters
    String ascii = normalized.replaceAll("[^\\p{ASCII}]", "");

    // Remove all characters that are not alphanumeric, underscores, dashes, or periods
    String sanitized = ascii.replaceAll("[^a-zA-Z0-9_.-]", "_");

    // Truncate the name to the maximum length allowed by the file system
    if (sanitized.length() > MAX_FILENAME_LENGTH) {
      sanitized = sanitized.substring(0, MAX_FILENAME_LENGTH);
    }

    // Ensure the name is not empty
    if (sanitized.isBlank()) {
      // generate a random name
      sanitized = "file_" + System.currentTimeMillis();
    }

    // Ensure the extension is preserved
    String extension = getClientExtension();
    if (!extension.isBlank() && !sanitized.endsWith("." + extension)) {
      sanitized += "." + extension;
    }

    return sanitized;
  }

  /**
   * Gets the client file extension.
   *
   * <p>
   * It is extracted from the original file name that was uploaded. This should not be considered as
   * a safe value to use for a file name on the server.
   * </p>
   *
   * @return the client file extension
   */
  public String getClientExtension() {
    String extension = "";
    String fileName = getClientName();

    int indexOfLastExtension = fileName.lastIndexOf(".");
    int lastSeparatorPosWindows = fileName.lastIndexOf("\\");
    int lastSeparatorPosUnix = fileName.lastIndexOf("/");
    int indexOfLastSeparator = Math.max(lastSeparatorPosWindows, lastSeparatorPosUnix);

    if (indexOfLastExtension > indexOfLastSeparator) {
      extension = fileName.substring(indexOfLastExtension + 1);
    }

    return extension;
  }

  /**
   * Gets the uploaded file size in bytes.
   *
   * @return the file size in bytes
   */
  public long size() {
    try {
      return getClientFile().size();
    } catch (BBjException e) {
      throw new IllegalStateException("Failed to get the file size", e);
    }
  }

  /**
   * Deletes the file from the cache.
   *
   * @return {@code true} if the file was successfully deleted; {@code false} otherwise
   */
  public boolean delete() {
    try {
      return getClientFile().delete();
    } catch (BBjException e) {
      return false;
    }
  }

  /**
   * Checks whether the file exists in the cache.
   *
   * @return {@code true} if the file exists; {@code false} otherwise
   */
  public boolean exists() {
    try {
      return getClientFile().exists();
    } catch (BBjException e) {
      return false;
    }
  }

  /**
   * Moves the file cache to a new location with the given new name.
   *
   * <p>
   * t must correspond to an existing directory, or to a filename that does not already exist. If
   * either condition is not met, this method will fail and throw an IOException.
   * </p>
   *
   * @param name the new file name
   *
   * @return the new file
   * @throws IOException if failed to move the file
   */
  public File move(String name) throws IOException {
    Objects.requireNonNull(name, "The new file name must not be null");
    if (name.isBlank()) {
      throw new IllegalArgumentException("The new file name must not be blank");
    }

    if (!exists()) {
      throw new IOException("The file does not exist");
    }

    try {
      String serverPath = getClientFile().copyFromClient(name);
      return new File(serverPath);

    } catch (BBjException e) {
      throw new IOException("Failed to move the file", e);
    }
  }

  /**
   * Moves the file from cache to a new location with the sanitized client name.
   *
   * @see #move(String)
   * @see #getSanitizedClientName()
   *
   * @return the new file
   * @throws IOException if failed to move the file
   */
  public File move() throws IOException {
    return move(getSanitizedClientName());
  }

  Environment getEnvironment() {
    return Environment.getCurrent();
  }

  BBjClientFile getClientFile() {
    try {
      BBjThinClient tc = getEnvironment().getBBjAPI().getThinClient();
      BBjClientFileSystem fs = tc.getClientFileSystem();
      return fs.getClientFile(getClientName());
    } catch (BBjException e) {
      throw new IllegalStateException("Failed to get the client file", e);
    }
  }
}
