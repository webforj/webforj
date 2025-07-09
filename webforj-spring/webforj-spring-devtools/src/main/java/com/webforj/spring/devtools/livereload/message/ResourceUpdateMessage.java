package com.webforj.spring.devtools.livereload.message;

/**
 * Message sent when a static resource is updated.
 *
 * <p>
 * This message contains information about resource changes detected by Spring DevTools, including
 * the resource type, path, and optionally the new content for CSS files.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class ResourceUpdateMessage extends LiveReloadMessage {
  private final String resourceType;
  private final String path;
  private final String content;
  private final long timestamp;

  /**
   * Creates a new resource update message.
   *
   * @param resourceType the type of resource (css, js, image)
   * @param path the resource path relative to static folder
   * @param content the resource content (for CSS files) or null
   */
  public ResourceUpdateMessage(String resourceType, String path, String content) {
    super("resource-update");
    this.resourceType = resourceType;
    this.path = path;
    this.content = content;
    this.timestamp = System.currentTimeMillis();
  }

  /**
   * Gets the resource type.
   *
   * @return the resource type
   */
  public String getResourceType() {
    return resourceType;
  }

  /**
   * Gets the resource path.
   *
   * @return the resource path
   */
  public String getPath() {
    return path;
  }

  /**
   * Gets the resource content.
   *
   * @return the resource content or null
   */
  public String getContent() {
    return content;
  }

  /**
   * Gets the update timestamp.
   *
   * @return the timestamp when the update was detected
   */
  public long getTimestamp() {
    return timestamp;
  }
}
