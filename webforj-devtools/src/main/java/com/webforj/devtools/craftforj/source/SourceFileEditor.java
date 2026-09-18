package com.webforj.devtools.craftforj.source;

import com.github.javaparser.ast.CompilationUnit;
import com.webforj.devtools.craftforj.source.model.FilePatch;
import com.webforj.devtools.craftforj.source.parser.ImportWriter;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.source.parser.StatementWrapper;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Edits one Java source file on behalf of a feature.
 *
 * <p>
 * The editor reads and parses the file, hands the parsed file to the feature's {@link SourceEdit},
 * then prints it with formatting preserved, syncs the imports, wraps the statements the edit made
 * too long and writes the result. Every feature that changes source goes through this one pass.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class SourceFileEditor {

  private final SourceParserService parserService;

  /**
   * Creates an editor.
   *
   * @param parserService the parser service
   */
  public SourceFileEditor(SourceParserService parserService) {
    this.parserService = parserService;
  }

  /**
   * Edits the given file.
   *
   * @param file the source file
   * @param imports the imports the edit keeps in step with the file
   * @param dryRun {@code true} to produce the patch without writing the file
   * @param edit the change to apply
   *
   * @return the patch, its patched content is {@code null} when the file content stays the same
   *
   * @throws IOException when the file cannot be read or written
   * @throws SourceModificationException when the file cannot be parsed
   */
  public FilePatch edit(Path file, SourceImports imports, boolean dryRun, SourceEdit edit)
      throws IOException {
    String original = readSource(file);
    CompilationUnit cu = parserService.parseWithLexicalPreservation(original)
        .orElseThrow(() -> new SourceModificationException("Failed to parse source file: " + file));

    if (!edit.apply(cu)) {
      return new FilePatch(file.toString(), original, null);
    }

    String patched = StatementWrapper.wrap(original,
        ImportWriter.sync(parserService.print(cu), imports.getCandidates(), imports.getUsed()));
    if (patched.equals(original)) {
      return new FilePatch(file.toString(), original, null);
    }

    if (!dryRun) {
      writeSource(file, patched);
    }

    return new FilePatch(file.toString(), original, patched);
  }

  private String readSource(Path file) throws IOException {
    try {
      return Files.readString(file);
    } catch (IOException e) {
      throw new IOException("Failed to read source file: " + file, e);
    }
  }

  private void writeSource(Path file, String content) throws IOException {
    try {
      Files.writeString(file, content);
    } catch (IOException e) {
      throw new IOException("Failed to write source file: " + file, e);
    }
  }
}
