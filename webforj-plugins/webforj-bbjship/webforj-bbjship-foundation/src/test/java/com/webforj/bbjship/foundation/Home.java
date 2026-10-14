package com.webforj.bbjship.foundation;

import java.io.IOException;
import java.nio.file.Path;

/**
 * The bbj-home fixture, a BBj installation the setup installs BBjShip into.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class Home {

  private Home() {}

  /**
   * Copies the fixture into a folder with bin/bbj executable as the repository holds it.
   *
   * @param target the folder to copy into
   *
   * @return the folder holding the copy
   * @throws IOException if the fixture cannot be read or the copy cannot be written
   */
  public static Path createCopy(Path target) throws IOException {
    FixtureFolder.createCopy(FixtureFolder.getFolder("bbj-home"), target);
    if (!target.resolve("bin/bbj").toFile().setExecutable(true)) {
      throw new IOException("Cannot make the bbj-home interpreter executable");
    }

    return target;
  }
}
