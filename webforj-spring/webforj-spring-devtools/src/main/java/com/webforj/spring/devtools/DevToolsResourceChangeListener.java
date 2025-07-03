package com.webforj.spring.devtools;

import java.util.Set;
import org.springframework.boot.devtools.classpath.ClassPathChangedEvent;
import org.springframework.boot.devtools.filewatch.ChangedFile;
import org.springframework.boot.devtools.filewatch.ChangedFiles;
import org.springframework.context.ApplicationListener;

/**
 * Listens for static resource changes detected by Spring DevTools and sends updates through the
 * DevTools WebSocket server.
 *
 * <p>
 * This listener hooks into Spring DevTools' file watching mechanism to detect changes to CSS,
 * JavaScript, and image files. When changes are detected, it sends the updated path through the
 * WebSocket connection, enabling hot reloading.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class DevToolsResourceChangeListener implements ApplicationListener<ClassPathChangedEvent> {

  private static final System.Logger logger =
      System.getLogger(DevToolsResourceChangeListener.class.getName());

  private static final Set<String> CSS_EXTENSIONS = Set.of(".css");
  private static final Set<String> JS_EXTENSIONS = Set.of(".js");
  private static final Set<String> IMAGE_EXTENSIONS =
      Set.of(".png", ".jpg", ".jpeg", ".gif", ".svg", ".webp", ".ico");

  /**
   * Creates a new DevTools resource change listener.
   */
  public DevToolsResourceChangeListener() {
    logger.log(System.Logger.Level.INFO,
        "DevToolsResourceChangeListener created - ready to monitor static resource changes");
  }

  /**
   * Handles ClassPathChangedEvent fired by Spring DevTools when files change.
   *
   * @param event the classpath changed event containing information about changed files
   */
  @Override
  public void onApplicationEvent(ClassPathChangedEvent event) {
    logger.log(System.Logger.Level.INFO, "ClassPathChangedEvent received");

    DevToolsServer server = DevToolsState.getWebSocketServer();
    if (server == null || !server.isOpen()) {
      logger.log(System.Logger.Level.WARNING,
          "DevTools WebSocket server not available, skipping resource update");
      return;
    }

    int totalFiles = 0;
    int staticResourceFiles = 0;

    for (ChangedFiles changedFiles : event.getChangeSet()) {
      for (ChangedFile file : changedFiles) {
        totalFiles++;
        logger.log(System.Logger.Level.INFO,
            "Changed file: " + file.getFile().getPath() + " (" + file.getType() + ")");

        if (isStaticResource(file)) {
          staticResourceFiles++;
          handleResourceChange(file, server);
        }
      }
    }

    logger.log(System.Logger.Level.INFO, "Processed " + totalFiles + " changed files, "
        + staticResourceFiles + " were static resources");
  }

  /**
   * Checks if the changed file is a static resource that should trigger a hot reload.
   *
   * @param file the changed file
   * @return true if the file is a static resource (CSS, JS, or image)
   */
  private boolean isStaticResource(ChangedFile file) {
    String path = file.getFile().getPath();
    return path.contains("static");
  }

  /**
   * Handles a changed static resource file.
   *
   * @param file the changed file
   * @param server the DevTools WebSocket server
   */
  private void handleResourceChange(ChangedFile file, DevToolsServer server) {
    String resourceType = getResourceType(file);
    String resourcePath = getResourcePath(file);

    logger.log(System.Logger.Level.INFO,
        "Detected " + file.getType() + " of " + resourceType + " resource: " + resourcePath);

    switch (file.getType()) {
      case ADD:
      case MODIFY:
      case DELETE:
        // Send notification to reload resources matching this path
        server.sendResourceUpdateMessage(resourceType, resourcePath, null);
        break;

      default:
        // Ignore other change types
        break;
    }
  }

  /**
   * Determines the resource type based on file extension.
   *
   * @param file the changed file
   * @return the resource type (css, js, or image)
   */
  private String getResourceType(ChangedFile file) {
    String fileName = file.getFile().getName().toLowerCase();

    if (CSS_EXTENSIONS.stream().anyMatch(fileName::endsWith)) {
      return "css";
    } else if (JS_EXTENSIONS.stream().anyMatch(fileName::endsWith)) {
      return "js";
    } else if (IMAGE_EXTENSIONS.stream().anyMatch(fileName::endsWith)) {
      return "image";
    }

    return "other";
  }

  /**
   * Extracts the resource path relative to the static directory.
   *
   * @param file the changed file
   * @return the relative resource path
   */
  private String getResourcePath(ChangedFile file) {
    String fullPath = file.getFile().getPath();

    // Find the static directory and return everything after it
    int index = fullPath.lastIndexOf("static");
    if (index >= 0) {
      // Move past "static" and any following separator
      int startIndex = index + "static".length();
      if (startIndex < fullPath.length()
          && (fullPath.charAt(startIndex) == '/' || fullPath.charAt(startIndex) == '\\')) {
        startIndex++;
      }

      // Return path after static/ (e.g., "css/style.css")
      return fullPath.substring(startIndex).replace('\\', '/');
    }

    // Fallback to file name if no static directory found
    return file.getFile().getName();
  }
}
