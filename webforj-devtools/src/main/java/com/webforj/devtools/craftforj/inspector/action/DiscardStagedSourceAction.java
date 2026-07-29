package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.inspector.source.staging.SourceStagingArea;

/**
 * Drops one staged free form file, or the whole staging area when no path is given.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class DiscardStagedSourceAction
    implements CraftforjActionHandler<DiscardStagedSourceAction.Response> {

  /** Action name. */
  public static final String ACTION = "inspector.discardStagedSource";

  private final SourceStagingArea stagingArea;

  /**
   * Creates the action.
   *
   * @param stagingArea the session staging area
   */
  public DiscardStagedSourceAction(SourceStagingArea stagingArea) {
    this.stagingArea = stagingArea;
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
  public Response handle(JsonObject params) {
    String path = params.has("path") ? params.get("path").getAsString() : null;
    if (path == null || path.isEmpty()) {
      stagingArea.clear();
      return new Response(true);
    }

    return new Response(stagingArea.discard(path));
  }

  /**
   * Response for the discard staged source action.
   */
  public static class Response {

    private final boolean discarded;

    Response(boolean discarded) {
      this.discarded = discarded;
    }

    /**
     * Checks whether anything was discarded.
     *
     * @return {@code true} when at least one entry was removed
     */
    public boolean isDiscarded() {
      return discarded;
    }
  }
}
