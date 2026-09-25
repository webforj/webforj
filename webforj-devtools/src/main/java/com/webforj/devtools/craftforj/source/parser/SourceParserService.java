package com.webforj.devtools.craftforj.source.parser;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ParseResult;
import com.github.javaparser.ParserConfiguration;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.DataKey;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.printer.lexicalpreservation.LexicalPreservingPrinter;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.model.TargetContext;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/**
 * Central service for parsing Java source files.
 *
 * <p>
 * Every piece of craftforJ that parses Java goes through this service, no other class builds a
 * {@link JavaParser} of its own. The language level is resolved once from the running JVM, so all
 * parsing agrees on it and a newer Java version lifts everything at once.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class SourceParserService {

  private static final int CACHE_CAPACITY = 32;
  private static final DataKey<Map<BlockStmt, String>> BLOCK_INDENTS = new DataKey<>() {};

  private final JavaParser parser;
  private final JavaParser lexicalParser;
  // Bounded and instance scoped: the entries die with the service, and the eldest file is evicted
  // past the cap, so a long session can never accumulate compilation units.
  private final Map<Path, CachedUnit> cache =
      Collections.synchronizedMap(new LinkedHashMap<Path, CachedUnit>(16, 0.75f, true) {
        @Override
        protected boolean removeEldestEntry(Map.Entry<Path, CachedUnit> eldest) {
          return size() > CACHE_CAPACITY;
        }
      });

  /**
   * Creates a new parser service.
   */
  public SourceParserService() {
    this.parser = new JavaParser(ParserConfigurations.create());

    ParserConfiguration lexicalConfig = ParserConfigurations.create();
    lexicalConfig.setLexicalPreservationEnabled(true);
    this.lexicalParser = new JavaParser(lexicalConfig);
  }

  /**
   * Gets the current parser service.
   *
   * <p>
   * One instance serves the whole JVM so every parsing consumer rides the same file cache; the
   * cache validates entries against the file's modification time and size, so sharing it across
   * sessions is safe. The public constructor stays for tests that want an isolated cache.
   * </p>
   *
   * @return the current parser service
   */
  public static SourceParserService getCurrent() {
    return Holder.INSTANCE;
  }

  /**
   * Parses a source file, reusing the last parse while the file on disk is unchanged.
   *
   * <p>
   * The component map resolves a variable name for every component on every build, which lands
   * repeatedly on the same handful of files. Re-reading and re-parsing each time costs hundreds of
   * milliseconds per build, so parses are memoized and validated against the file's modification
   * time and size. A write through {@code parseWithLexicalPreservation} is never served from here.
   * </p>
   *
   * @param file the source file to parse
   * @return the compilation unit, or empty when the source does not parse
   * @throws IOException if the file cannot be read
   */
  public Optional<CompilationUnit> parse(Path file) throws IOException {
    BasicFileAttributes attributes = Files.readAttributes(file, BasicFileAttributes.class);
    long modified = attributes.lastModifiedTime().toMillis();
    long size = attributes.size();

    CachedUnit cached = cache.get(file);
    if (cached != null && cached.matches(modified, size)) {
      return Optional.of(cached.getUnit());
    }

    Optional<CompilationUnit> parsed = parse(Files.readString(file));
    parsed.ifPresent(unit -> cache.put(file, new CachedUnit(modified, size, unit)));

    return parsed;
  }

  /**
   * Parses source code.
   */
  public Optional<CompilationUnit> parse(String content) {
    ParseResult<CompilationUnit> result = parser.parse(content);
    return result.getResult();
  }

  /**
   * Parses source code and returns the full result including problems.
   *
   * @param content the source code
   * @return the parse result carrying either the unit or its problems
   */
  public ParseResult<CompilationUnit> parseWithProblems(String content) {
    return parser.parse(content == null ? "" : content);
  }

  /**
   * Parses a single statement.
   *
   * @param code the statement source
   * @return the statement, or {@code null} when it does not parse
   */
  public Statement parseStatement(String code) {
    try {
      return parser.parseStatement(code).getResult().orElse(null);
    } catch (Exception e) {
      return null;
    }
  }

  /**
   * Parses source code with lexical preservation.
   *
   * @param content the source code
   * @return the parsed file, or empty when the source has a syntax error
   */
  public Optional<CompilationUnit> parseWithLexicalPreservation(String content) {
    ParseResult<CompilationUnit> result = lexicalParser.parse(content);
    if (!result.isSuccessful()) {
      return Optional.empty();
    }

    return result.getResult().map(cu -> {
      LexicalPreservingPrinter.setup(cu);
      Map<BlockStmt, String> indents = new IdentityHashMap<>();
      String[] lines = content.split("\n", -1);
      for (BlockStmt block : cu.findAll(BlockStmt.class)) {
        if (!block.getStatements().isEmpty() && block.getEnd().isPresent()) {
          String line = lines[block.getEnd().get().line - 1];
          String stripped = line.stripLeading();
          if (stripped.startsWith("}")) {
            indents.put(block, line.substring(0, line.length() - stripped.length()));
          }
        }
      }
      cu.setData(BLOCK_INDENTS, indents);
      return cu;
    });
  }

  /**
   * Prints a compilation unit preserving formatting.
   */
  public String print(CompilationUnit cu) {
    String printed = LexicalPreservingPrinter.print(cu);
    return preserveEmptiedBlockIndentation(cu, printed);
  }

  /**
   * Extracts the variable name at the given line in the source file.
   *
   * @param sourceFile the absolute path to the source file
   * @param lineNumber the line number where the component was created
   * @return the variable name, or null if not found
   */
  public String extractVariableName(Path sourceFile, int lineNumber) {
    return extractVariableName(sourceFile, lineNumber, "");
  }

  /**
   * Extracts the variable name at the given line, requiring a compatible declaration type.
   *
   * <p>
   * The type name guards against stale line numbers: when the recorded line no longer points at the
   * component's declaration, a declaration of a different type at that line is rejected instead of
   * silently resolved.
   * </p>
   *
   * @param sourceFile the absolute path to the source file
   * @param lineNumber the line number where the component was created
   * @param typeName the expected simple type name, or empty to skip the type check
   * @return the variable name, or null if not found
   */
  public String extractVariableName(Path sourceFile, int lineNumber, String typeName) {
    return extractVariableName(sourceFile, lineNumber,
        typeName == null || typeName.isEmpty() ? Set.of() : Set.of(typeName));
  }

  /**
   * Extracts the variable name at the given line, requiring a declaration compatible with any of
   * the given type names.
   *
   * @param sourceFile the absolute path to the source file
   * @param lineNumber the line number where the component was created
   * @param typeNames the acceptable simple type names, or empty to skip the type check
   * @return the variable name, or null if not found
   */
  public String extractVariableName(Path sourceFile, int lineNumber, Set<String> typeNames) {
    if (sourceFile == null || lineNumber <= 0) {
      return null;
    }

    try {
      Optional<CompilationUnit> unit = parse(sourceFile);
      if (unit.isEmpty()) {
        return null;
      }

      // Components cluster in a few files, so the same unit is walked once per component. The
      // names are held by the cache entry and go away with it when the file changes.
      CachedUnit entry = cache.get(sourceFile);
      if (entry == null) {
        return AstFinder.extractVariableNameAt(unit.get(), targetFor(lineNumber, typeNames));
      }

      String cacheKey = lineNumber + ":" + String.join("|", new TreeSet<>(typeNames));

      return entry.variableNameAt(cacheKey,
          () -> AstFinder.extractVariableNameAt(unit.get(), targetFor(lineNumber, typeNames)));
    } catch (IOException | SourceModificationException e) {

      return null;
    }
  }

  private String preserveEmptiedBlockIndentation(CompilationUnit cu, String printed) {
    if (!cu.containsData(BLOCK_INDENTS)) {
      return printed;
    }
    Map<BlockStmt, String> indents = cu.getData(BLOCK_INDENTS);
    List<BlockStmt> blocks = cu.findAll(BlockStmt.class);
    if (blocks.stream()
        .noneMatch(block -> block.getStatements().isEmpty() && indents.containsKey(block))) {
      return printed;
    }
    Optional<CompilationUnit> reparsed = parser.parse(printed).getResult();
    if (reparsed.isEmpty()) {
      return printed;
    }
    List<BlockStmt> printedBlocks = reparsed.get().findAll(BlockStmt.class);
    if (blocks.size() != printedBlocks.size()) {
      return printed;
    }
    String[] lines = printed.split("\n", -1);
    for (int index = 0; index < blocks.size(); index++) {
      BlockStmt block = blocks.get(index);
      String indent = indents.get(block);
      if (indent != null && block.getStatements().isEmpty()) {
        int lineIndex = printedBlocks.get(index).getEnd().orElseThrow().line - 1;
        String stripped = lines[lineIndex].stripLeading();
        if (stripped.startsWith("}")) {
          lines[lineIndex] = indent + stripped;
        }
      }
    }
    return String.join("\n", lines);
  }

  private static TargetContext targetFor(int lineNumber, Set<String> typeNames) {
    TargetContext target = new TargetContext(lineNumber, "");
    if (typeNames != null && !typeNames.isEmpty()) {
      target.setAcceptableTypes(typeNames);
    }

    return target;
  }

  /**
   * Lazy holder so the parsers are only built when the service is first used.
   *
   * @author Hyyan Abo Fakher
   * @since 26.02
   */
  private static final class Holder {

    private static final SourceParserService INSTANCE = new SourceParserService();

    private Holder() {}
  }

  /**
   * A parsed compilation unit together with the file stamp it was parsed from.
   *
   * @author Hyyan Abo Fakher
   * @since 26.02
   */
  private static final class CachedUnit {

    private final long modified;
    private final long size;
    private final CompilationUnit unit;
    private final Map<String, Optional<String>> variableNames = new ConcurrentHashMap<>();

    private CachedUnit(long modified, long size, CompilationUnit unit) {
      this.modified = modified;
      this.size = size;
      this.unit = unit;
    }

    /**
     * Gets the variable name for the given lookup key, computing it once per key.
     *
     * @param cacheKey the line and type-name lookup key
     * @param finder computes the name when this key has not been looked up yet
     * @return the variable name, or null when the line declares none
     */
    private String variableNameAt(String cacheKey, Supplier<String> finder) {
      return variableNames.computeIfAbsent(cacheKey, key -> Optional.ofNullable(finder.get()))
          .orElse(null);
    }

    /**
     * Checks whether this entry still reflects the file on disk.
     *
     * @param currentModified the file's current modification time in milliseconds
     * @param currentSize the file's current size in bytes
     * @return true when the entry is still valid
     */
    private boolean matches(long currentModified, long currentSize) {
      return modified == currentModified && size == currentSize;
    }

    /**
     * Gets the cached compilation unit.
     *
     * @return the compilation unit
     */
    private CompilationUnit getUnit() {
      return unit;
    }
  }
}
