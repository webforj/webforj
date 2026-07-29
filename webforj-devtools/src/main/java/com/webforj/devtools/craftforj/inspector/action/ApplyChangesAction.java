package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import com.webforj.devtools.craftforj.inspector.source.SourceCodeModifier;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeRequest;
import com.webforj.devtools.craftforj.inspector.source.model.ChangeResult;
import com.webforj.devtools.craftforj.inspector.source.parser.SourceParserService;
import java.util.Arrays;
import java.util.List;

/**
 * Previews or applies source code changes. dryRun=true validates only, dryRun=false writes files.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ApplyChangesAction implements CraftforjActionHandler<List<ChangeResult>> {

  /** Action name. */
  public static final String ACTION = "inspector.applyChanges";

  private static final Gson GSON = new Gson();
  private final SourceCodeModifier modifier;

  /** Creates action with default modifier. */
  public ApplyChangesAction() {
    this.modifier =
        new SourceCodeModifier(new FeatureHandlerRegistry(), SourceParserService.getCurrent());
  }

  /**
   * Creates action with custom modifier.
   *
   * @param modifier the source code modifier
   */
  public ApplyChangesAction(SourceCodeModifier modifier) {
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
  public List<ChangeResult> handle(JsonObject params) {
    List<ChangeRequest> changes = parseChanges(params);
    boolean dryRun = params.has("dryRun") && params.get("dryRun").getAsBoolean();
    return dryRun ? modifier.preview(changes) : modifier.apply(changes);
  }

  private List<ChangeRequest> parseChanges(JsonObject params) {
    if (!params.has("changes")) {
      return List.of();
    }

    ChangeRequest[] changes = GSON.fromJson(params.get("changes"), ChangeRequest[].class);
    return Arrays.asList(changes);
  }
}
