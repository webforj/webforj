package com.webforj.devtools.craftforj.styles.action;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.styles.StylesheetModifier;
import com.webforj.devtools.craftforj.styles.StylesheetResolver;
import com.webforj.devtools.craftforj.styles.model.StylesheetChange;
import com.webforj.devtools.craftforj.styles.model.StylesheetResult;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

/**
 * Writes the application stylesheet.
 *
 * <p>
 * The type on each change selects the operation, an exact match edit, a prepend, an append, a named
 * region replaced whole, or the whole file replaced. Changes apply in list order and the whole list
 * fails atomically.
 * </p>
 *
 * <p>
 * A {@code baseVersion} turns the write into a compare and swap. A concurrent change makes the
 * write return a conflict carrying the current content instead of overwriting it. It is required
 * for a whole file replace.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class WriteStylesheetAction implements CraftforjActionHandler<StylesheetResult> {

  /** Action name. */
  public static final String ACTION = "styles.write";

  private static final Gson GSON = new Gson();
  private static final String PARAM_DRY_RUN = "dryRun";
  private static final String PARAM_BASE_VERSION = "baseVersion";
  private static final String PARAM_CHANGES = "changes";
  private final StylesheetResolver resolver;
  private final StylesheetModifier modifier;

  /**
   * Creates the action.
   *
   * @param resolver the stylesheet resolver
   * @param modifier the stylesheet modifier
   */
  public WriteStylesheetAction(StylesheetResolver resolver, StylesheetModifier modifier) {
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
  public StylesheetResult handle(JsonObject params) {
    if (params == null) {
      throw new CraftforjActionException("changes are required");
    }

    String configured =
        params.has("file") && !params.get("file").isJsonNull() ? params.get("file").getAsString()
            : null;
    boolean dryRun = params.has(PARAM_DRY_RUN) && !params.get(PARAM_DRY_RUN).isJsonNull()
        && params.get(PARAM_DRY_RUN).getAsBoolean();
    String baseVersion =
        params.has(PARAM_BASE_VERSION) && !params.get(PARAM_BASE_VERSION).isJsonNull()
            ? params.get(PARAM_BASE_VERSION).getAsString()
            : null;

    List<StylesheetChange> changes = parseChanges(params);
    if (hasReplace(changes) && (baseVersion == null || baseVersion.isBlank())) {
      throw new CraftforjActionException("baseVersion is required for a REPLACE change");
    }

    Path path = resolver.resolve(configured);

    return modifier.write(path, changes, dryRun, baseVersion);
  }

  private static boolean hasReplace(List<StylesheetChange> changes) {
    for (StylesheetChange change : changes) {
      if (change != null && change.getType() == StylesheetChange.Type.REPLACE) {
        return true;
      }
    }

    return false;
  }

  private List<StylesheetChange> parseChanges(JsonObject params) {
    if (!params.has(PARAM_CHANGES) || params.get(PARAM_CHANGES).isJsonNull()) {
      return List.of();
    }

    StylesheetChange[] changes = GSON.fromJson(params.get(PARAM_CHANGES), StylesheetChange[].class);

    return Arrays.asList(changes);
  }
}
