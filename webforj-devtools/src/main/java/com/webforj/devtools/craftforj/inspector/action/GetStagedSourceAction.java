package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.inspector.source.staging.SourceStagingArea;
import com.webforj.devtools.craftforj.inspector.source.staging.model.StagedFile;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Lists the staged free form files with the before and after content the client diffs.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetStagedSourceAction
    implements CraftforjActionHandler<GetStagedSourceAction.Response> {

  /** Action name. */
  public static final String ACTION = "inspector.getStagedSource";

  private final SourceStagingArea stagingArea;

  /**
   * Creates the action.
   *
   * @param stagingArea the session staging area
   */
  public GetStagedSourceAction(SourceStagingArea stagingArea) {
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
    List<StagedFileView> files = new ArrayList<>();
    for (StagedFile staged : stagingArea.list()) {
      String original = staged.isNew() ? "" : readFile(staged.getPath());
      files.add(new StagedFileView(staged.getPath(), staged.isNew(), staged.isVerified(), original,
          staged.getContent()));
    }

    return new Response(files);
  }

  private static String readFile(String path) {
    try {
      return Files.readString(Path.of(path), StandardCharsets.UTF_8);
    } catch (IOException e) {

      return "";
    }
  }

  /**
   * Response listing the staged files.
   */
  public static class Response {

    private final List<StagedFileView> files;

    Response(List<StagedFileView> files) {
      this.files = files;
    }

    /**
     * Gets the staged files.
     *
     * @return the staged file views
     */
    public List<StagedFileView> getFiles() {
      return files;
    }
  }

  /**
   * One staged file with the content pair the client renders as a diff.
   */
  public static class StagedFileView {

    private final String path;
    private final boolean isNew;
    private final boolean verified;
    private final String original;
    private final String patched;

    StagedFileView(String path, boolean isNew, boolean verified, String original, String patched) {
      this.path = path;
      this.isNew = isNew;
      this.verified = verified;
      this.original = original;
      this.patched = patched;
    }

    /**
     * Gets the absolute file path.
     *
     * @return the path
     */
    public String getPath() {
      return path;
    }

    /**
     * Checks whether the file does not exist on disk yet.
     *
     * @return {@code true} when new
     */
    public boolean isNewFile() {
      return isNew;
    }

    /**
     * Checks whether the staged content passed full compile validation.
     *
     * @return {@code true} when compile verified
     */
    public boolean isVerified() {
      return verified;
    }

    /**
     * Gets the on disk content the diff starts from.
     *
     * @return the original content, empty for a new file
     */
    public String getOriginal() {
      return original;
    }

    /**
     * Gets the staged content the diff ends at.
     *
     * @return the staged content
     */
    public String getPatched() {
      return patched;
    }
  }
}
