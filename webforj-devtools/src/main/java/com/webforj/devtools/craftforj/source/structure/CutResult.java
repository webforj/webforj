package com.webforj.devtools.craftforj.source.structure;

import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.stmt.Statement;
import java.util.ArrayList;
import java.util.List;

/**
 * Copies of the source a cut took out of a file.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class CutResult {

  private final List<FieldDeclaration> fields = new ArrayList<>();
  private final List<Statement> leading = new ArrayList<>();
  private final List<Statement> trailing = new ArrayList<>();
  private Expression inline;

  /**
   * Gets the live list of field declarations.
   *
   * @return the fields, open for additions
   */
  List<FieldDeclaration> getFields() {
    return fields;
  }

  /**
   * Gets the live list of statements that stood before the attach call.
   *
   * @return the statements, open for additions
   */
  List<Statement> getLeading() {
    return leading;
  }

  /**
   * Gets the live list of statements that stood after the attach call.
   *
   * @return the statements, open for additions
   */
  List<Statement> getTrailing() {
    return trailing;
  }

  /**
   * Gets every copy, the fields first, then the statements in source order.
   *
   * @return the copied nodes
   */
  List<Node> getNodes() {
    List<Node> nodes = new ArrayList<>(fields);
    nodes.addAll(leading);
    nodes.addAll(trailing);
    if (inline != null) {
      nodes.add(inline);
    }

    return nodes;
  }

  /**
   * Gets the expression of a component created inline.
   *
   * @return the expression, or {@code null} for a declared component
   */
  Expression getInlineCreation() {
    return inline;
  }

  /**
   * Sets the expression of a component created inline.
   *
   * @param inline the expression
   */
  void setInlineCreation(Expression inline) {
    this.inline = inline;
  }
}
