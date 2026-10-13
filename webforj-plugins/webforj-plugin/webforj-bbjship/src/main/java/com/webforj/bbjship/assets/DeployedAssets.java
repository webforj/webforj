package com.webforj.bbjship.assets;

import com.webforj.bbjship.DeploymentFolder;
import com.webforj.bbjship.assets.model.AssetRequest;
import com.webforj.bbjship.assets.model.AssetResponse;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLConnection;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.imageio.ImageIO;

/**
 * Serves the static files and icons of published applications from their deployments.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class DeployedAssets {

  private static final int OK = 200;
  private static final int NOT_MODIFIED = 304;
  private static final int NOT_FOUND = 404;
  private static final int UNSUPPORTED_MEDIA_TYPE = 415;
  private static final int MAX_ICON_SIZE = 1024;
  private static final String STATIC_PACKAGE = "static/";
  private static final String ICONS_PACKAGE = "icons/";
  private static final String DEFAULT_ICON = "/icons/webforj.png";
  private static final String DEFAULT_ICON_TYPE = "image/png";
  private static final String IMAGE_TYPES = "image/";
  private static final Pattern ICON_SIZE =
      Pattern.compile("^(?<base>.*)-(?<width>\\d+)x(?<height>\\d+)(?<ext>\\..*)$"); // NOSONAR
  private static final Map<String, String> CONTENT_TYPES =
      Map.of("js", "text/javascript", "mjs", "text/javascript", "css", "text/css", "html",
          "text/html", "htm", "text/html", "txt", "text/plain", "pdf", "application/pdf");

  private static final ResourceIndexes INDEXES = new ResourceIndexes();

  private DeployedAssets() {}

  /**
   * Answers an asset request of a published application.
   *
   * @param root the deployments root path
   * @param request the asset request
   *
   * @return the response, 404 when the name is invalid or the application or the asset does not
   *         exist
   * @throws IOException if a deployment cannot be read
   */
  public static AssetResponse read(String root, AssetRequest request) throws IOException {
    return read(root, request, INDEXES);
  }

  static AssetResponse read(String root, AssetRequest request, ResourceIndexes indexes)
      throws IOException {
    indexes.dropRemoved();
    Optional<Path> current =
        DeploymentFolder.findCurrentDirectory(Path.of(root), request.getApplicationName());
    if (current.isEmpty()) {
      return AssetResponse.create(NOT_FOUND).build();
    }

    DeployedApplication application = DeployedApplication.load(current.get(), indexes);
    String path = request.getPath();
    String assetsPrefix = application.getAssetsPrefix();
    if (!assetsPrefix.isEmpty() && path.startsWith(assetsPrefix)) {
      return readStatic(application, path.substring(assetsPrefix.length()));
    }

    if (path.startsWith(application.getIconsPrefix())) {
      return readIcon(application, path.substring(application.getIconsPrefix().length()), request);
    }

    if (assetsPrefix.isEmpty()) {
      return readStatic(application, path);
    }

    return AssetResponse.create(NOT_FOUND).build();
  }

  private static AssetResponse readStatic(DeployedApplication application, String path)
      throws IOException {
    String folder = path.isEmpty() || path.endsWith("/") ? path : path + "/";
    List<String> candidates = application.getIndexes().stream().map(index -> folder + index)
        .collect(Collectors.toCollection(ArrayList::new));
    if (!folder.equals(path)) {
      candidates.add(path);
    }

    for (String candidate : candidates) {
      for (String extension : application.getExtensions()) {
        String name = candidate + extension;
        Optional<byte[]> body = application.readResource(STATIC_PACKAGE + name);
        if (body.isPresent()) {
          return AssetResponse.create(OK)
              .setContentType(getContentType(name, application.getMimeExtensions()))
              .setCacheControl(application.getCacheControl()).setBody(body.get()).build();
        }
      }
    }

    return AssetResponse.create(NOT_FOUND).build();
  }

  private static AssetResponse readIcon(DeployedApplication application, String path,
      AssetRequest request) throws IOException {
    String contentType = getContentType(path, application.getMimeExtensions());
    if (contentType == null || !contentType.startsWith(IMAGE_TYPES)) {
      return AssetResponse.create(NOT_FOUND).build();
    }

    byte[] resource = application.readResource(ICONS_PACKAGE + path).orElse(new byte[0]);
    if (resource.length > 0) {
      return createImageResponse(contentType, resource, toEntityTag(getDigest(resource)),
          request.getIfNoneMatch());
    }

    Matcher size = ICON_SIZE.matcher(path);
    if (!size.matches()) {
      if (!DEFAULT_ICON_TYPE.equalsIgnoreCase(contentType)) {
        return AssetResponse.create(NOT_FOUND).build();
      }

      byte[] fallback = readDefaultIcon();
      return createImageResponse(contentType, fallback, toEntityTag(getDigest(fallback)),
          request.getIfNoneMatch());
    }

    if (!isIconSize(size.group("width")) || !isIconSize(size.group("height"))) {
      return AssetResponse.create(NOT_FOUND).build();
    }

    byte[] base = application.readResource(ICONS_PACKAGE + size.group("base") + size.group("ext"))
        .orElse(new byte[0]);
    if (base.length == 0) {
      base = readDefaultIcon();
    }

    int width = Integer.parseInt(size.group("width"));
    int height = Integer.parseInt(size.group("height"));
    String entityTag = toEntityTag(getDerivedDigest(getDigest(base), width, height,
        request.getPadding(), request.getBackground(), contentType));
    if (entityTag.equals(request.getIfNoneMatch())) {
      return AssetResponse.create(NOT_MODIFIED).setEntityTag(entityTag).build();
    }

    BufferedImage baseImage = ImageIO.read(new ByteArrayInputStream(base));
    if (baseImage == null) {
      return AssetResponse.create(UNSUPPORTED_MEDIA_TYPE).build();
    }

    BufferedImage icon = IconsGenerator.createIcon(baseImage, width, height,
        toPadding(request.getPadding()), toBackground(baseImage, request.getBackground()));
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    if (!ImageIO.write(icon, contentType.substring(IMAGE_TYPES.length()), output)) {
      return AssetResponse.create(UNSUPPORTED_MEDIA_TYPE).build();
    }

    return AssetResponse.create(OK).setContentType(contentType).setEntityTag(entityTag)
        .setBody(output.toByteArray()).build();
  }

  private static AssetResponse createImageResponse(String contentType, byte[] body,
      String entityTag, String ifNoneMatch) {
    if (entityTag.equals(ifNoneMatch)) {
      return AssetResponse.create(NOT_MODIFIED).setEntityTag(entityTag).build();
    }

    return AssetResponse.create(OK).setContentType(contentType).setEntityTag(entityTag)
        .setBody(body).build();
  }

  private static byte[] readDefaultIcon() throws IOException {
    try (InputStream input = DeployedAssets.class.getResourceAsStream(DEFAULT_ICON)) {
      return input.readAllBytes();
    }
  }

  private static boolean isIconSize(String value) {
    try {
      int size = Integer.parseInt(value);
      return size > 0 && size <= MAX_ICON_SIZE;
    } catch (NumberFormatException invalid) {
      return false;
    }
  }

  private static float toPadding(String padding) {
    if (padding == null) {
      return 0f;
    }

    try {
      return Math.clamp(Float.parseFloat(padding), 0f, 1f);
    } catch (NumberFormatException invalid) {
      return 0f;
    }
  }

  private static Optional<Color> toBackground(BufferedImage image, String background) {
    if (background == null) {
      return IconsGenerator.findBackgroundColor(image);
    }

    try {
      return IconsGenerator.resolveBackgroundColor(image, background);
    } catch (NumberFormatException invalid) {
      return IconsGenerator.findBackgroundColor(image);
    }
  }

  private static String getContentType(String name, Map<String, String> mimeExtensions) {
    int dot = name.lastIndexOf('.');
    if (dot >= 0) {
      String extension = name.substring(dot + 1);
      String configured = mimeExtensions.get(extension);
      if (configured == null) {
        configured = mimeExtensions.get(extension.toLowerCase(Locale.ROOT));
      }

      if (configured != null) {
        return configured;
      }

      String known = CONTENT_TYPES.get(extension.toLowerCase(Locale.ROOT));
      if (known != null) {
        return known;
      }
    }

    return URLConnection.guessContentTypeFromName(name);
  }

  private static String getDigest(byte[] content) {
    return Base64.getUrlEncoder().encodeToString(createDigest().digest(content));
  }

  private static String getDerivedDigest(String baseDigest, int width, int height, String padding,
      String background, String contentType) throws IOException {
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    try (DataOutputStream data = new DataOutputStream(bytes)) {
      data.writeUTF(baseDigest);
      data.writeInt(width);
      data.writeInt(height);
      if (padding != null) {
        data.writeUTF(padding);
      }

      if (background != null) {
        data.writeUTF(background);
      }

      data.writeUTF(contentType);
    }

    return getDigest(bytes.toByteArray());
  }

  private static String toEntityTag(String digest) {
    return "\"" + digest + "\"";
  }

  private static MessageDigest createDigest() {
    try {
      return MessageDigest.getInstance("SHA-256");
    } catch (NoSuchAlgorithmException missing) {
      throw new IllegalStateException(missing);
    }
  }
}
