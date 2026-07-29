package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourceFileResolver;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourcePathRegistry;
import com.webforj.devtools.craftforj.inspector.source.staging.CompileValidator;
import com.webforj.devtools.craftforj.inspector.source.staging.SourceHasher;
import com.webforj.devtools.craftforj.inspector.source.staging.SourceStagingArea;
import com.webforj.devtools.craftforj.inspector.source.staging.model.CompileDiagnostic;
import com.webforj.devtools.craftforj.inspector.source.staging.model.StagedFile;
import com.webforj.devtools.craftforj.inspector.source.staging.model.ValidationResult;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Stages one free form source file behind the compile gate, without touching disk.
 *
 * <p>
 * The candidate joins every already staged file in a single validation, so multi file changes
 * validate as the unit they will be applied as. A rejected candidate never enters the staging area.
 * Existing files must have been resolved server side and must still match the content hash captured
 * when they were read. New files must land under a known source root.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class StageSourceAction implements CraftforjActionHandler<StageSourceAction.Response> {

  /** Action name. */
  public static final String ACTION = "inspector.stageSource";

  /** Rejection code for compile failures. */
  public static final String CODE_COMPILE_ERROR = "COMPILE_ERROR";

  /** Rejection code for a file that changed on disk after it was read. */
  public static final String CODE_SOURCE_CHANGED = "SOURCE_CHANGED";

  /** Rejection code for a path outside the allowed write surface. */
  public static final String CODE_PATH_REFUSED = "PATH_REFUSED";

  private final SourceStagingArea stagingArea;
  private final CompileValidator validator;
  private final Path projectRoot;

  /**
   * Creates the action.
   *
   * @param stagingArea the session staging area
   * @param validator the compile validator
   * @param projectRoot the project root used to confine new files to source roots
   */
  public StageSourceAction(SourceStagingArea stagingArea, CompileValidator validator,
      Path projectRoot) {
    this.stagingArea = stagingArea;
    this.validator = validator;
    this.projectRoot = projectRoot;
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
    String content = params.has("content") ? params.get("content").getAsString() : null;

    if (path == null || path.isEmpty() || content == null) {
      return Response.refused(CODE_PATH_REFUSED, "Missing path or content");
    }

    if (path.endsWith(".kt")) {
      return Response.refused(CODE_PATH_REFUSED,
          "Kotlin sources cannot be staged, only Java files are supported");
    }

    if (!path.endsWith(".java")) {
      return Response.refused(CODE_PATH_REFUSED, "Only Java source files can be staged");
    }

    Path target = Path.of(path).toAbsolutePath().normalize();
    boolean isNew = !Files.exists(target);
    String baseHash = params.has("baseHash") ? params.get("baseHash").getAsString() : null;
    Response refusal = isNew ? checkNewFile(target) : checkExistingFile(target, baseHash);
    if (refusal != null) {
      return refusal;
    }

    ValidationResult result = validate(target.toString(), content, isNew);
    if (!result.isSuccess()) {
      return Response.rejected(CODE_COMPILE_ERROR, result.getErrors());
    }

    boolean verified = result.isVerified(target.toString());
    stagingArea.stage(new StagedFile(target.toString(), baseHash, content, isNew, verified));

    return Response.staged(verified, isNew);
  }

  private Response checkNewFile(Path target) {
    if (!SourceFileResolver.isUnderSourceRoot(projectRoot, target)) {
      return Response.refused(CODE_PATH_REFUSED,
          "New files must be created under a source root of the running project");
    }

    return null;
  }

  private Response checkExistingFile(Path target, String baseHash) {
    if (!SourcePathRegistry.isRecorded(target.toString())) {
      return Response.refused(CODE_PATH_REFUSED,
          "File is not a recorded component source, read it through the inspector first");
    }

    if (baseHash == null || baseHash.isEmpty()) {
      return Response.refused(CODE_SOURCE_CHANGED,
          "Missing baseHash, re-read the file to capture its current content hash");
    }

    String diskHash = readDiskHash(target);
    if (!baseHash.equals(diskHash)) {
      return Response.refused(CODE_SOURCE_CHANGED,
          "File changed on disk since it was read, re-read the file");
    }

    return null;
  }

  private ValidationResult validate(String path, String content, boolean isNew) {
    Map<String, String> sources = new HashMap<>();
    Set<String> newFiles = new HashSet<>();
    for (StagedFile staged : stagingArea.list()) {
      sources.put(staged.getPath(), staged.getContent());
      if (staged.isNew()) {
        newFiles.add(staged.getPath());
      }
    }

    sources.put(path, content);
    if (isNew) {
      newFiles.add(path);
    }

    return validator.validate(sources, newFiles);
  }

  private static String readDiskHash(Path path) {
    try {
      return SourceHasher.hash(Files.readString(path, StandardCharsets.UTF_8));
    } catch (IOException e) {

      return null;
    }
  }

  /**
   * Response for the stage source action.
   */
  public static class Response {

    private final boolean staged;
    private final boolean verified;
    private final boolean isNew;
    private final String code;
    private final String message;
    private final List<CompileDiagnostic> errors;

    private Response(boolean staged, boolean verified, boolean isNew, String code, String message,
        List<CompileDiagnostic> errors) {
      this.staged = staged;
      this.verified = verified;
      this.isNew = isNew;
      this.code = code;
      this.message = message;
      this.errors = errors;
    }

    static Response staged(boolean verified, boolean isNew) {
      return new Response(true, verified, isNew, null, null, List.of());
    }

    static Response refused(String code, String message) {
      return new Response(false, false, false, code, message, List.of());
    }

    static Response rejected(String code, List<CompileDiagnostic> errors) {
      return new Response(false, false, false, code, null, errors);
    }

    /**
     * Checks whether the file entered the staging area.
     *
     * @return {@code true} when staged
     */
    public boolean isStaged() {
      return staged;
    }

    /**
     * Checks whether the file passed full compile validation.
     *
     * @return {@code true} when compile verified
     */
    public boolean isVerified() {
      return verified;
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
     * Gets the rejection code.
     *
     * @return the code, or {@code null} when staged
     */
    public String getCode() {
      return code;
    }

    /**
     * Gets the rejection message.
     *
     * @return the message, or {@code null}
     */
    public String getMessage() {
      return message;
    }

    /**
     * Gets the diagnostics behind a compile rejection.
     *
     * @return the diagnostics, possibly empty
     */
    public List<CompileDiagnostic> getErrors() {
      return errors;
    }
  }
}
