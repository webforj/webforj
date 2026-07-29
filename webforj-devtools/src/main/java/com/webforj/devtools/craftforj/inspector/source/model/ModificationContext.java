package com.webforj.devtools.craftforj.inspector.source.model;

import com.webforj.devtools.craftforj.inspector.source.generator.SourceChange;
import java.util.ArrayList;
import java.util.List;

/**
 * Context for applying modifications to a component in the AST.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ModificationContext {

  private TargetContext target;
  private String variableName;
  private List<SourceChange> sourceChanges = new ArrayList<>();

  /**
   * Creates a ModificationContext with the given values.
   *
   * @param target the target context
   * @param variableName the variable name
   * @param sourceChanges the source changes
   */
  public ModificationContext(TargetContext target, String variableName,
      List<SourceChange> sourceChanges) {
    this.target = target;
    this.variableName = variableName;
    this.sourceChanges = sourceChanges;
  }

  /**
   * Gets the target context identifying the component.
   *
   * @return the target context
   */
  public TargetContext getTarget() {
    return target;
  }

  /**
   * Sets the target context identifying the component.
   *
   * @param target the target context
   */
  public void setTarget(TargetContext target) {
    this.target = target;
  }

  /**
   * Gets the known variable name.
   *
   * @return the variable name (may be null)
   */
  public String getVariableName() {
    return variableName;
  }

  /**
   * Sets the known variable name.
   *
   * @param variableName the variable name
   */
  public void setVariableName(String variableName) {
    this.variableName = variableName;
  }

  /**
   * Gets the source changes to apply.
   *
   * @return the source changes
   */
  public List<SourceChange> getSourceChanges() {
    return sourceChanges;
  }

  /**
   * Sets the source changes to apply.
   *
   * @param sourceChanges the source changes
   */
  public void setSourceChanges(List<SourceChange> sourceChanges) {
    this.sourceChanges = sourceChanges;
  }

  /**
   * Gets the line number from target.
   *
   * @return the line number
   */
  public int getLineNumber() {
    return target != null ? target.getLineNumber() : 0;
  }

  /**
   * Gets the type name from target.
   *
   * @return the type name
   */
  public String getTypeName() {
    return target != null ? target.getTypeName() : null;
  }

}
