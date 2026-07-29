package com.webforj.devtools.craftforj.inspector.source.model;

import java.util.Set;

/**
 * Context for identifying a component target in the AST.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class TargetContext {

  private int lineNumber;
  private String typeName;
  private Set<String> acceptableTypes;

  /**
   * Creates a TargetContext with the given values.
   *
   * @param lineNumber the line number
   * @param typeName the type name
   */
  public TargetContext(int lineNumber, String typeName) {
    this.lineNumber = lineNumber;
    this.typeName = typeName;
  }

  /**
   * Gets the line number where the component was created.
   *
   * @return the line number
   */
  public int getLineNumber() {
    return lineNumber;
  }

  /**
   * Sets the line number where the component was created.
   *
   * @param lineNumber the line number
   */
  public void setLineNumber(int lineNumber) {
    this.lineNumber = lineNumber;
  }

  /**
   * Gets the component type simple name.
   *
   * @return the type name
   */
  public String getTypeName() {
    return typeName;
  }

  /**
   * Sets the component type simple name.
   *
   * @param typeName the type name
   */
  public void setTypeName(String typeName) {
    this.typeName = typeName;
  }


  /**
   * Gets the simple type names that are acceptable matches for this target.
   *
   * <p>
   * When set, declarations matching any of these names (typically the component's runtime class and
   * its superclasses) are accepted; otherwise only {@link #getTypeName()} matches.
   * </p>
   *
   * @return the acceptable type names, or null when only the type name applies
   */
  public Set<String> getAcceptableTypes() {
    return acceptableTypes;
  }

  /**
   * Sets the simple type names that are acceptable matches for this target.
   *
   * @param acceptableTypes the acceptable type names
   */
  public void setAcceptableTypes(Set<String> acceptableTypes) {
    this.acceptableTypes = acceptableTypes;
  }
}
