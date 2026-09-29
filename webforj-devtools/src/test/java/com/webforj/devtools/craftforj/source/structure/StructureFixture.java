package com.webforj.devtools.craftforj.source.structure;

import com.github.javaparser.ast.CompilationUnit;
import com.webforj.devtools.craftforj.source.SourceFileEditor;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.source.structure.model.AttachPoint;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Shared setup of the structure editor tests.
 *
 * @author Hyyan Abo Fakher
 */
final class StructureFixture {

  static final String BUTTON = "com.webforj.component.button.Button";
  static final String TEXT_FIELD = "com.webforj.component.field.TextField";
  static final String FLEX = "com.webforj.component.layout.flexlayout.FlexLayout";
  static final String TOOLBAR = "com.webforj.component.layout.toolbar.Toolbar";
  static final String SPLITTER = "com.webforj.component.layout.splitter.Splitter";
  static final String DIV = "com.webforj.component.html.elements.Div";

  private final Path directory;

  StructureFixture(Path directory) {
    this.directory = directory;
  }

  static StructureModifier createEditor() {
    SourceParserService parserService = new SourceParserService();
    return new StructureModifier(new SourceFileEditor(parserService), parserService);
  }

  static CompilationUnit parse(String source) {
    return new SourceParserService().parse(source).orElseThrow();
  }

  Path write(String name, String content) throws IOException {
    Path file = directory.resolve(name);
    Files.writeString(file, content);

    return file;
  }

  static SourceLocation variable(Path file, String name, String type) {
    return new SourceLocation(file.toString(), null, null, name, type);
  }

  static SourceLocation line(Path file, int line, String type) {
    return new SourceLocation(file.toString(), line, null, null, type);
  }

  static AttachPoint point(SourceLocation parent, String... methods) {
    return new AttachPoint(parent, List.of(methods));
  }

  static AttachPoint after(SourceLocation parent, SourceLocation anchor, String... methods) {
    AttachPoint point = point(parent, methods);
    point.setAnchor(anchor);

    return point;
  }

  static AttachPoint before(SourceLocation parent, SourceLocation anchor, String... methods) {
    AttachPoint point = after(parent, anchor, methods);
    point.setBefore(true);

    return point;
  }
}
