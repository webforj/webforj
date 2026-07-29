package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.FilePatch;
import com.webforj.devtools.craftforj.inspector.source.parser.SourceParserService;
import java.util.Arrays;
import java.util.List;

/**
 * Returns the before and after content of the files a set of changes would touch.
 *
 * <p>
 * This never writes. It exists so the client can show the patched code before the user commits to
 * {@link ApplyChangesAction}, for a single change or for everything pending in a file.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class PreviewPatchAction implements CraftforjActionHandler<List<FilePatch>> {

  /** Action name. */
  public static final String ACTION = "inspector.previewPatch";

  private static final Gson GSON = new Gson();
  private final SourceCodeModifier modifier;

  /** Creates action with default modifier. */
  public PreviewPatchAction() {
    this(new SourceCodeModifier(new FeatureHandlerRegistry(), SourceParserService.getCurrent()));
  }

  /**
   * Creates action with custom modifier.
   *
   * @param modifier the source code modifier
   */
  public PreviewPatchAction(SourceCodeModifier modifier) {
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
  public List<FilePatch> handle(JsonObject params) {
    if (!params.has("changes")) {
      return List.of();
    }

    ChangeRequest[] changes = GSON.fromJson(params.get("changes"), ChangeRequest[].class);
    return modifier.previewPatches(Arrays.asList(changes));
  }
}
