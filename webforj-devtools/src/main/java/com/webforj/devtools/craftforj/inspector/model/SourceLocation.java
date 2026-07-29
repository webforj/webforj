package com.webforj.devtools.craftforj.inspector.model;

/**
 * Source code location where a component was instantiated.
 *
 * <p>
 * This class captures all information needed to locate and modify a component's creation point in
 * source code. It is used for both displaying source info in craftforJ and for applying changes
 * when the runtime component no longer exists.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class SourceLocation {

  private String file;
  private Integer line;
  private String declaringClass;
  private String variableName;
  private String componentType;

  /**
   * Creates a SourceLocation with the given values.
   *
   * @param file the file path
   * @param line the line number
   * @param declaringClass the declaring class
   * @param variableName the variable name
   * @param componentType the component type
   */
  public SourceLocation(String file, Integer line, String declaringClass, String variableName,
      String componentType) {
    this.file = file;
    this.line = line;
    this.declaringClass = declaringClass;
    this.variableName = variableName;
    this.componentType = componentType;
  }

  /**
   * Gets the absolute path to the source file.
   *
   * @return the file path
   */
  public String getFile() {
    return file;
  }

  /**
   * Sets the absolute path to the source file.
   *
   * @param file the file path
   */
  public void setFile(String file) {
    this.file = file;
  }

  /**
   * Gets the line number where the component was created.
   *
   * @return the line number
   */
  public Integer getLine() {
    return line;
  }

  /**
   * Sets the line number where the component was created.
   *
   * @param line the line number
   */
  public void setLine(Integer line) {
    this.line = line;
  }

  /**
   * Gets the fully qualified name of the class containing the component.
   *
   * @return the declaring class
   */
  public String getDeclaringClass() {
    return declaringClass;
  }

  /**
   * Sets the fully qualified name of the class containing the component.
   *
   * @param declaringClass the declaring class
   */
  public void setDeclaringClass(String declaringClass) {
    this.declaringClass = declaringClass;
  }

  /**
   * Gets the variable name assigned to the component.
   *
   * @return the variable name (e.g., "btn", "submitButton")
   */
  public String getVariableName() {
    return variableName;
  }

  /**
   * Sets the variable name assigned to the component.
   *
   * @param variableName the variable name
   */
  public void setVariableName(String variableName) {
    this.variableName = variableName;
  }

  /**
   * Gets the fully qualified component class name.
   *
   * @return the component type (e.g., "com.webforj.component.button.Button")
   */
  public String getComponentType() {
    return componentType;
  }

  /**
   * Sets the fully qualified component class name.
   *
   * @param componentType the component type
   */
  public void setComponentType(String componentType) {
    this.componentType = componentType;
  }

  /**
   * Gets the simple class name from the component type.
   *
   * @return the simple class name, or null if componentType is null
   */
  public String getSimpleTypeName() {
    if (componentType == null) {
      return null;
    }

    int lastDot = componentType.lastIndexOf('.');
    return lastDot >= 0 ? componentType.substring(lastDot + 1) : componentType;
  }

  /**
   * Checks if this source location has enough information for source modification.
   *
   * @return true if file, line, and variableName are all present
   */
  public boolean isComplete() {
    return file != null && !file.isEmpty() && line != null && line > 0 && variableName != null
        && !variableName.isEmpty();
  }

  /**
   * Checks if this source location has basic info (file and line).
   *
   * @return true if file and line are present
   */
  public boolean hasBasicInfo() {
    return file != null && !file.isEmpty() && line != null && line > 0;
  }

}
