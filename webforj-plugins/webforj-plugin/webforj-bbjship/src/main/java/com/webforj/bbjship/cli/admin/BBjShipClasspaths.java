package com.webforj.bbjship.cli.admin;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Adds the classpaths of BBjShip to BBjServices.
 *
 * <p>
 * The servlet classpath holds the BBj defaults and the BBjShip library. The base classpath holds
 * the BBj libraries and is the first entry of every application classpath.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipClasspaths {

  static final String SERVLET_CLASSPATH = "_webforj_bbjship_cli";
  private static final String BASE_CLASSPATH = "_webforj_bbjship";
  private static final String BBJ_DEFAULT = "(bbj_default)";
  private static final String UNOWNED_SERVLET_CLASSPATH =
      "The BBjShip command classpath belongs to another application";
  private static final String UNOWNED_BASE_CLASSPATH =
      "The BBjShip base classpath contains an unowned library";

  private final BBjAdminBase admin;
  private final BBjShipPlugin plugin;

  /**
   * Creates the classpaths of BBjShip.
   *
   * @param admin the administration session
   * @param plugin the BBjShip plugin of the BBj installation
   */
  public BBjShipClasspaths(BBjAdminBase admin, BBjShipPlugin plugin) {
    this.admin = admin;
    this.plugin = plugin;
  }

  /**
   * Adds the servlet and base classpaths that are missing and leaves the ones BBjShip owns as they
   * are, removing what was added when a write fails.
   *
   * @throws IOException if the BBjShip library or the BBj libraries are missing, or a BBjShip
   *         classpath belongs to another application
   * @throws BBjAdminException if BBjServices refuses the registration
   */
  void addMissing() throws IOException, BBjAdminException {
    List<String> names = admin.getClasspathNames();
    Map<String, List<String>> missing = new LinkedHashMap<>();
    if (names.contains(SERVLET_CLASSPATH)) {
      applyServletValidation(admin.getClasspath(SERVLET_CLASSPATH));
    } else {
      missing.put(SERVLET_CLASSPATH, List.of(BBJ_DEFAULT, readLibrary()));
    }

    if (names.contains(BASE_CLASSPATH)) {
      applyBaseValidation(admin.getClasspath(BASE_CLASSPATH));
    } else {
      List<String> libraries = readBBjLibraries();
      applyBaseValidation(libraries);
      missing.put(BASE_CLASSPATH, libraries);
    }

    if (missing.isEmpty()) {
      return;
    }

    try {
      for (Map.Entry<String, List<String>> classpath : missing.entrySet()) {
        admin.setClasspath(classpath.getKey(), classpath.getValue());
      }

      admin.commit();
    } catch (BBjAdminException | RuntimeException failure) {
      applyRollback(missing.keySet(), failure);
      throw failure;
    }
  }

  /**
   * Removes the servlet classpath and adds it again, so BBjServices loads the BBjShip library as it
   * is on disk.
   *
   * @throws IOException if the BBjShip library is missing
   * @throws BBjAdminException if BBjServices refuses the registration
   */
  public void resetServlet() throws IOException, BBjAdminException {
    List<String> entries = List.of(BBJ_DEFAULT, readLibrary());
    admin.setClasspath(SERVLET_CLASSPATH, null);
    admin.commit();
    admin.setClasspath(SERVLET_CLASSPATH, entries);
    admin.commit();
  }

  /**
   * Removes the servlet and base classpaths BBjShip owns.
   *
   * @throws IOException if a BBjShip classpath belongs to another application
   * @throws BBjAdminException if BBjServices refuses the removal
   */
  public void removeAll() throws IOException, BBjAdminException {
    List<String> names = admin.getClasspathNames();
    if (names.contains(SERVLET_CLASSPATH)) {
      applyServletValidation(admin.getClasspath(SERVLET_CLASSPATH));
    }

    if (names.contains(BASE_CLASSPATH)) {
      applyBaseValidation(admin.getClasspath(BASE_CLASSPATH));
    }

    for (String name : List.of(SERVLET_CLASSPATH, BASE_CLASSPATH)) {
      if (names.contains(name)) {
        admin.setClasspath(name, null);
      }
    }

    admin.commit();
  }

  private String readLibrary() throws IOException {
    Path library = plugin.getLibrary();
    if (!Files.isRegularFile(library)) {
      throw new IOException("The BBjShip library is missing. Check the plugin installation");
    }

    return library.toString();
  }

  private List<String> readBBjLibraries() throws IOException {
    Path folder = plugin.getBBjLibraries();
    if (!Files.isDirectory(folder)) {
      throw new IOException("Cannot list BBj libraries. Check the BBj library directory");
    }

    List<String> libraries;
    try (Stream<Path> files = Files.list(folder)) {
      libraries = files.filter(Files::isRegularFile)
          .filter(file -> file.getFileName().toString().endsWith(".jar")).sorted()
          .map(Path::toString).toList();
    }

    if (libraries.isEmpty()) {
      throw new IOException("No BBj libraries were found. Check the BBj library directory");
    }

    return libraries;
  }

  private void applyServletValidation(List<String> entries) throws IOException {
    if (entries.size() != 2 || !BBJ_DEFAULT.equals(entries.get(0))
        || !plugin.getLibrary().equals(Path.of(entries.get(1)).toAbsolutePath().normalize())) {
      throw new IOException(UNOWNED_SERVLET_CLASSPATH);
    }
  }

  private void applyBaseValidation(List<String> entries) throws IOException {
    Path owned = plugin.getBBjLibraries().toRealPath();
    for (String entry : entries) {
      Path path = Path.of(entry);
      if (entry.contains("*") || entry.contains("?") || !Files.isRegularFile(path)
          || !path.getFileName().toString().endsWith(".jar")
          || !owned.equals(path.toRealPath().getParent())) {
        throw new IOException(UNOWNED_BASE_CLASSPATH);
      }
    }
  }

  private void applyRollback(Set<String> names, Exception failure) {
    for (String name : names) {
      try {
        admin.setClasspath(name, null);
      } catch (BBjAdminException | RuntimeException cleanup) {
        failure.addSuppressed(cleanup);
      }
    }

    try {
      admin.commit();
    } catch (BBjAdminException | RuntimeException cleanup) {
      failure.addSuppressed(cleanup);
    }
  }
}
