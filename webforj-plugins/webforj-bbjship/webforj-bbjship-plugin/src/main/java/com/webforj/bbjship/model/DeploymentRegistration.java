package com.webforj.bbjship.model;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Holds the properties and arguments of a published BBj application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@SuppressWarnings("java:S6206")
public final class DeploymentRegistration {

  private final Map<String, Object> properties;
  private final List<String> arguments;
  private final List<String> javaArguments;

  /**
   * Creates a registration snapshot.
   *
   * @param properties the application properties
   * @param arguments the application arguments
   * @param javaArguments the Java arguments
   *
   * @throws NullPointerException if a collection is null or an argument is null
   */
  public DeploymentRegistration(Map<String, Object> properties, List<String> arguments,
      List<String> javaArguments) {
    this.properties = Collections.unmodifiableMap(new LinkedHashMap<>(properties));
    this.arguments = List.copyOf(arguments);
    this.javaArguments = List.copyOf(javaArguments);
  }

  /**
   * Gets the application properties.
   *
   * @return the application properties
   */
  public Map<String, Object> getProperties() {
    return properties;
  }

  /**
   * Gets the application arguments.
   *
   * @return the application arguments
   */
  public List<String> getArguments() {
    return arguments;
  }

  /**
   * Gets the Java arguments.
   *
   * @return the Java arguments
   */
  public List<String> getJavaArguments() {
    return javaArguments;
  }
}
