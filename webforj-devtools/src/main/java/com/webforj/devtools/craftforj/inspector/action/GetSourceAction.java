package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourcePathRegistry;
import com.webforj.devtools.craftforj.inspector.source.staging.SourceHasher;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.UnaryOperator;

/**
 * Action handler that reads source file content.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetSourceAction implements CraftforjActionHandler<GetSourceAction.Response> {

  public static final String ACTION = "inspector.getSource";
  private final UnaryOperator<String> fileReader;

  /**
   * Creates a new GetSourceAction with the default file reader.
   */
  public GetSourceAction() {
    this(GetSourceAction::readFile);
  }

  /**
   * Creates a new GetSourceAction with a custom file reader.
   *
   * @param fileReader function to read file content by path
   */
  public GetSourceAction(UnaryOperator<String> fileReader) {
    this.fileReader = fileReader;
  }

  @Override
  public String getAction() {
    return ACTION;
  }

  @Override
  public Response handle(JsonObject params) {
    String file = params.has("file") ? params.get("file").getAsString() : null;

    if (file == null || file.isEmpty()) {
      throw new CraftforjActionException("Missing file parameter");
    }

    if (!SourcePathRegistry.isRecorded(file)) {
      throw new CraftforjActionException("File is not a recorded component source: " + file);
    }

    String content = fileReader.apply(file);
    if (content == null) {
      throw new CraftforjActionException("File not found: " + file);
    }

    return new Response(content, SourceHasher.hash(content));
  }

  private static String readFile(String filePath) {
    try {
      Path path = Paths.get(filePath);
      if (!Files.exists(path)) {
        return null;
      }

      return Files.readString(path, StandardCharsets.UTF_8);
    } catch (IOException e) {
      throw new CraftforjActionException("Failed to read file: " + filePath);
    }
  }

  /**
   * Response for get source action.
   */
  public static class Response {
    private final String content;
    private final String contentHash;

    Response(String content, String contentHash) {
      this.content = content;
      this.contentHash = contentHash;
    }

    /**
     * Gets the source file content.
     *
     * @return the content
     */
    public String getContent() {
      return content;
    }

    /**
     * Gets the SHA-256 hash of the content, used to detect edits between read and write.
     *
     * @return the content hash
     */
    public String getContentHash() {
      return contentHash;
    }
  }
}
