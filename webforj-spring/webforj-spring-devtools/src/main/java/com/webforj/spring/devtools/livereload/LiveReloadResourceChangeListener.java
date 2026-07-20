package com.webforj.spring.devtools.livereload;

import com.webforj.devtools.livereload.LiveReloadLifecycle;
import java.util.Set;
import org.springframework.boot.devtools.classpath.ClassPathChangedEvent;
import org.springframework.boot.devtools.filewatch.ChangedFile;
import org.springframework.boot.devtools.filewatch.ChangedFiles;
import org.springframework.context.ApplicationListener;

/**
 * Turns static resource changes detected by Spring DevTools into resource updates for the connected
 * browsers.
 *
 * <p>
 * Spring DevTools watches the classpath and leaves static resources out of the restart, so a
 * stylesheet or image change fires a classpath change event without a restart. This listener
 * classifies those files and sends each one through the live reload, so the browser applies a
 * stylesheet or image change in place and reloads for the rest.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class LiveReloadResourceChangeListener
    implements ApplicationListener<ClassPathChangedEvent> {

  private static final Set<String> BUNDLE_OUTPUT_DIRS =
      Set.of("static/frontend/", "static/webforj/");
  private static final Set<String> CSS_EXTENSIONS = Set.of(".css");
  private static final Set<String> JS_EXTENSIONS = Set.of(".js");
  private static final Set<String> IMAGE_EXTENSIONS =
      Set.of(".png", ".jpg", ".jpeg", ".gif", ".svg", ".webp", ".ico");

  private final LiveReloadLifecycle lifecycle;

  /**
   * Creates the listener bound to the given lifecycle.
   *
   * @param lifecycle the live reload lifecycle owned by the context
   */
  public LiveReloadResourceChangeListener(LiveReloadLifecycle lifecycle) {
    this.lifecycle = lifecycle;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onApplicationEvent(ClassPathChangedEvent event) {
    for (ChangedFiles changedFiles : event.getChangeSet()) {
      for (ChangedFile file : changedFiles) {
        if (isStaticResource(file)) {
          lifecycle.sendResourceUpdate(getResourceType(file), getResourcePath(file));
        }
      }
    }
  }

  private static boolean isStaticResource(ChangedFile file) {
    String path = file.getFile().getPath().replace('\\', '/');

    // The bundler owns these folders and reports its own writes through the watch, so a change
    // there would otherwise reach the browser twice.

    return path.contains("static") && BUNDLE_OUTPUT_DIRS.stream().noneMatch(path::contains);
  }

  private static String getResourceType(ChangedFile file) {
    String fileName = file.getFile().getName().toLowerCase();
    String type;

    if (CSS_EXTENSIONS.stream().anyMatch(fileName::endsWith)) {
      type = "css";
    } else if (JS_EXTENSIONS.stream().anyMatch(fileName::endsWith)) {
      type = "js";
    } else if (IMAGE_EXTENSIONS.stream().anyMatch(fileName::endsWith)) {
      type = "image";
    } else {
      type = "other";
    }

    return type;
  }

  /**
   * Extracts the served resource path, the part after the static directory, which is how the page
   * references the resource.
   *
   * @param file the changed file
   * @return the served resource path
   */
  private static String getResourcePath(ChangedFile file) {
    String fullPath = file.getFile().getPath();

    int index = fullPath.lastIndexOf("static");
    if (index >= 0) {
      int startIndex = index + "static".length();
      if (startIndex < fullPath.length()
          && (fullPath.charAt(startIndex) == '/' || fullPath.charAt(startIndex) == '\\')) {
        startIndex++;
      }

      return fullPath.substring(startIndex).replace('\\', '/');
    }

    return file.getFile().getName();
  }
}
