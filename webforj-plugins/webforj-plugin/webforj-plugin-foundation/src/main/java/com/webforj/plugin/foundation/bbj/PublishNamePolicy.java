package com.webforj.plugin.foundation.bbj;

import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Decides whether a name may publish an application and name its deployment folder.
 *
 * <p>
 * A publish name starts with a letter or digit, holds only letters, digits, underscores, dots and
 * hyphens, and does not end with a dot. A device name Windows reserves is refused in any letter
 * case, with or without an extension. A name BBjServices maps in every web context, as a first path
 * segment or as a file extension, is refused in any letter case.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class PublishNamePolicy {

  /**
   * Says which publish names are valid.
   */
  public static final String INVALID_NAME_ADVICE = "Use a name that starts with a letter or digit, "
      + "holds only letters, digits, underscores, dots and hyphens, and is not a reserved device "
      + "name such as con or nul";

  private static final Pattern NAME = Pattern.compile("[A-Za-z0-9][A-Za-z0-9_.-]*");
  private static final Pattern RESERVED_DEVICES =
      Pattern.compile("(con|prn|aux|nul|com[1-9]|lpt[1-9])(\\..*)?", Pattern.CASE_INSENSITIVE);
  private static final Set<String> RESERVED_PATHS =
      Set.of("apps", "webapp", "servlet", "webservice", "webconfig", "files", "buires",
          "gwtwebclient", "webapprmi", "filedownloadservlet", "fileuploadservlet", "imageservlet",
          "printservlet", "basis", "dwcembed", "embedbui", "help");
  private static final List<String> RESERVED_EXTENSIONS = List.of(".jnlp", ".css", ".js",
      ".cache.html", ".cache.png", ".cache.jpg", ".cache.gif", ".gwt.rpc");

  private PublishNamePolicy() {}

  /**
   * Indicates whether a name may publish an application.
   *
   * @param name the publish name, may be {@code null}
   *
   * @return {@code true} when the name may publish an application
   */
  public static boolean isValid(String name) {
    return name != null && NAME.matcher(name).matches() && !name.endsWith(".")
        && !RESERVED_DEVICES.matcher(name).matches() && !isReserved(name);
  }

  /**
   * Finds the reason a name may not publish an application.
   *
   * @param name the publish name
   *
   * @return the message that names the publish name and the reason, empty when the name is valid
   */
  public static Optional<String> findRefusal(String name) {
    if (isReserved(name)) {
      return Optional.of("The publish name " + name
          + " is reserved by BBjServices for its own web paths. Choose another publish name");
    }

    if (!isValid(name)) {
      return Optional.of("Publish name " + name + " is invalid. " + INVALID_NAME_ADVICE);
    }

    return Optional.empty();
  }

  /**
   * Indicates whether BBjServices maps a name in every web context.
   *
   * @param name the publish name
   *
   * @return {@code true} when the name is a reserved path segment or ends in a reserved extension
   */
  public static boolean isReserved(String name) {
    String lowerCase = name.toLowerCase(Locale.ROOT);

    return RESERVED_PATHS.contains(lowerCase)
        || RESERVED_EXTENSIONS.stream().anyMatch(lowerCase::endsWith);
  }
}
