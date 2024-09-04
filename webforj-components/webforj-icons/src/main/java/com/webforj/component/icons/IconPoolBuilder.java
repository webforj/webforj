package com.webforj.component.icons;

import com.webforj.Page;
import com.webforj.utilities.Assets;
import io.github.classgraph.ClassGraph;
import io.github.classgraph.Resource;
import io.github.classgraph.ScanResult;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * A utility class for registering icon pools.
 *
 * <p>
 * This class provides methods to register icons from different sources, allowing them to be used in
 * the application.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class IconPoolBuilder {

  private IconPoolBuilder() {
    // no instance
  }

  /**
   * Registers an icon pool with the specified name using a map of icons.
   *
   * <p>
   * Each entry in the map should have the icon name as the key and the SVG content as the value.
   * </p>
   *
   * @param name the name of the icon pool to register. This name must not be null or empty.
   * @param icons a map where the keys are icon names and the values are the corresponding SVG
   *        content as strings. This map must not be null.
   *
   * @throws NullPointerException if either {@code name} or {@code icons} is null
   * @throws IllegalArgumentException if {@code name} is empty
   */
  public static void fromMap(String name, Map<String, String> icons) {
    Objects.requireNonNull(name, "The icon pool name must not be null");
    Objects.requireNonNull(icons, "The icons map must not be null");

    // validate the icon pool name
    if (name.isEmpty()) {
      throw new IllegalArgumentException("The icon pool name must not be empty");
    }

    // Transform the pool name
    String validName = buildValidName(name);

    StringBuilder sb = new StringBuilder();
    sb.append(String.format("const %s = {", validName));
    for (Map.Entry<String, String> entry : icons.entrySet()) {
      sb.append(String.format("'%s': `%s`,", entry.getKey(), entry.getValue()));
    }
    sb.append("};");

    // register the icon pool
    // @see https://dwc.style/docs/#/dwc/dwc-icon?id=custom-pools
    sb.append("(window.Dwc ??= {}).IconsPools ??= [];");
    sb.append("window.Dwc.IconsPools.push({");
    sb.append(String.format("name: '%s',", name));
    sb.append(String.format(
        "resolver: function(iconName) { " + "return %s[iconName] ? "
            + "`data:image/svg+xml,${encodeURIComponent(%s[iconName])}` : ''; " + "},",
        validName, validName));

    sb.append("});");

    Page.getCurrent().executeJsVoidAsync(sb.toString());
  }

  /**
   * Registers an icon pool with the specified name by scanning a folder for SVG files.
   *
   * <p>
   * All SVG files found in the specified folder are registered as icons in the pool, where the icon
   * name corresponds to the SVG file name (without the extension).
   * </p>
   *
   * @param name the name of the icon pool to register. This name must not be null or empty.
   * @param src the folder path where SVG files are located. If the path begins with
   *        {@code context://}, it will be treated as a resource URL within the application's
   *        context. Otherwise, it will be treated as a standard directory path.
   *
   * @throws IllegalArgumentException if an I/O error occurs during folder scanning or if the path
   *         is invalid
   */
  public static void fromDirectory(String name, String src) {
    try {
      fromMap(name, scanFolderForSvgFiles(src));
    } catch (IOException e) {
      throw new IllegalArgumentException("Failed to scan the icon pool folder", e);
    }
  }

  private static Map<String, String> scanFolderForSvgFiles(String folder) throws IOException {
    Map<String, String> icons = new HashMap<>();

    if (Assets.isContextUrl(folder)) {
      String resolvedPath = Assets.resolveContextUrl(folder);
      try (ScanResult scanResult =
          new ClassGraph().acceptPaths(folder).acceptPathsNonRecursive(resolvedPath).scan()) {

        // Find all resources in the folder that end with .svg
        for (Resource resource : scanResult.getResourcesWithExtension("svg")) {
          String fileName = resource.getPath();
          String iconName =
              fileName.substring(fileName.lastIndexOf('/') + 1, fileName.lastIndexOf('.'));

          String content = new String(resource.load());
          icons.put(iconName, content);
        }
      }

      return icons;
    } else {
      try (DirectoryStream<Path> directoryStream =
          Files.newDirectoryStream(Paths.get(folder), "*.svg")) {
        for (Path svgFile : directoryStream) {
          String fileName = svgFile.getFileName().toString();
          String iconName = fileName.substring(0, fileName.lastIndexOf('.'));
          String content = Files.readString(svgFile);

          icons.put(iconName, content);
        }
      }
    }

    return icons;
  }

  private static String buildValidName(String name) {
    String validName = name.replaceAll("[^a-zA-Z0-9_$]", "_");
    return "__" + validName + "__webforj__icon__pool__";
  }
}
