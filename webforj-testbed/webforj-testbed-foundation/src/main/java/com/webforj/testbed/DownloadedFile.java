package com.webforj.testbed;

import java.util.Objects;

/**
 * A file the application sent to the user for download.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class DownloadedFile { // NOSONAR
  private final String name;
  private final byte[] contents;

  /**
   * Creates a downloaded file.
   *
   * @param name the name the file carries on the client
   * @param contents the bytes of the file
   */
  public DownloadedFile(String name, byte[] contents) {
    this.name = Objects.requireNonNull(name, "The file name must not be null");
    this.contents = Objects.requireNonNull(contents, "The file contents must not be null").clone();
  }

  /**
   * Returns the name the file carries on the client.
   *
   * @return the file name
   */
  public String getName() {
    return name;
  }

  /**
   * Returns the bytes of the file.
   *
   * @return a copy of the bytes
   */
  public byte[] getContents() {
    return contents.clone();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return name + " (" + contents.length + " bytes)";
  }
}
