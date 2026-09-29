package com.webforj.devtools.craftforj.source.structure.model;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * The source a new component is created from.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class ComponentCreation {

  private final String type;
  private final String expression;
  private final List<String> calls = new ArrayList<>();
  private final Set<String> imports = new LinkedHashSet<>();

  /**
   * Creates a component creation.
   *
   * @param type the fully qualified component type
   * @param expression the creation expression, for example {@code new Button("Button")}
   */
  public ComponentCreation(String type, String expression) {
    this.type = type;
    this.expression = expression;
  }

  /**
   * Gets the fully qualified component type.
   *
   * @return the component type
   */
  public String getType() {
    return type;
  }

  /**
   * Gets the simple component type name.
   *
   * @return the simple type name
   */
  public String getSimpleType() {
    return type.substring(type.lastIndexOf('.') + 1);
  }

  /**
   * Gets the creation expression.
   *
   * @return the creation expression source
   */
  public String getExpression() {
    return expression;
  }

  /**
   * Gets the live list of calls made on the new component, for example {@code setWidth("10em")}.
   *
   * @return the calls without a receiver, open for additions
   */
  public List<String> getCalls() {
    return calls;
  }

  /**
   * Gets the live set of imports the creation and the calls need beside the component type.
   *
   * @return the fully qualified names, open for additions
   */
  public Set<String> getImports() {
    return imports;
  }
}
