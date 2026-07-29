package com.webforj.devtools.craftforj.styles.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.styles.StylesheetModifier;
import com.webforj.devtools.craftforj.styles.StylesheetRegions;
import com.webforj.devtools.craftforj.styles.StylesheetResolver;
import com.webforj.devtools.craftforj.styles.model.StylesheetInfo;
import java.nio.file.Path;

/**
 * Reads the application stylesheet.
 *
 * <p>
 * Returns the resolved path, the content, the version a write compares against, and the regions the
 * file already carries.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ReadStylesheetAction implements CraftforjActionHandler<StylesheetInfo> {

  /** Action name. */
  public static final String ACTION = "styles.read";

  private final StylesheetResolver resolver;
  private final StylesheetModifier modifier;

  /**
   * Creates the action.
   *
   * @param resolver the stylesheet resolver
   * @param modifier the stylesheet modifier used to read content
   */
  public ReadStylesheetAction(StylesheetResolver resolver, StylesheetModifier modifier) {
    this.resolver = resolver;
    this.modifier = modifier;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAction() {
    return ACTION;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public StylesheetInfo handle(JsonObject params) {
    String configured = params != null && params.has("file") && !params.get("file").isJsonNull()
        ? params.get("file").getAsString()
        : null;

    Path path = resolver.resolve(configured);
    String content = modifier.read(path);

    StylesheetInfo info = new StylesheetInfo();
    info.setPath(path.toString());
    info.setExists(content != null);
    info.setContent(content);
    info.setDefaultUsed(configured == null || configured.isBlank());
    info.setVersion(StylesheetModifier.version(content));
    info.setRegions(StylesheetRegions.findAll(content));

    return info;
  }
}
