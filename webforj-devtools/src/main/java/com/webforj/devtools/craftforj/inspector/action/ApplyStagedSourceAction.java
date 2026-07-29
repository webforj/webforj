package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourcePathRegistry;
import com.webforj.devtools.craftforj.inspector.source.staging.SourceStagingArea;
import com.webforj.devtools.craftforj.inspector.source.staging.StagingException;
import java.util.List;

/**
 * Writes every staged free form file to disk atomically after user approval.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ApplyStagedSourceAction
    implements CraftforjActionHandler<ApplyStagedSourceAction.Response> {

  /** Action name. */
  public static final String ACTION = "inspector.applyStagedSource";

  private final SourceStagingArea stagingArea;

  /**
   * Creates the action.
   *
   * @param stagingArea the session staging area
   */
  public ApplyStagedSourceAction(SourceStagingArea stagingArea) {
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
    try {
      List<String> applied = stagingArea.apply();
      applied.forEach(SourcePathRegistry::addPath);

      return Response.applied(applied);
    } catch (StagingException e) {
      return Response.failed(e.getCode(), e.getMessage());
    }
  }

  /**
   * Response for the apply staged source action.
   */
  public static class Response {

    private final List<String> applied;
    private final String code;
    private final String message;
    private final boolean restored;

    private Response(List<String> applied, String code, String message, boolean restored) {
      this.applied = applied;
      this.code = code;
      this.message = message;
      this.restored = restored;
    }

    static Response applied(List<String> applied) {
      return new Response(applied, null, null, false);
    }

    static Response failed(String code, String message) {
      return new Response(List.of(), code, message, true);
    }

    /**
     * Gets the applied file paths.
     *
     * @return the paths, empty on failure
     */
    public List<String> getApplied() {
      return applied;
    }

    /**
     * Gets the failure code.
     *
     * @return the code, or {@code null} on success
     */
    public String getCode() {
      return code;
    }

    /**
     * Gets the failure message.
     *
     * @return the message, or {@code null} on success
     */
    public String getMessage() {
      return message;
    }

    /**
     * Checks whether disk state was restored after a failure.
     *
     * @return {@code true} when pre images were restored
     */
    public boolean isRestored() {
      return restored;
    }
  }
}
