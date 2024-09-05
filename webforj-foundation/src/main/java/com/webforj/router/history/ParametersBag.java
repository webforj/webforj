package com.webforj.router.history;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * ParametersBag is a container query parameters.
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public class ParametersBag implements Serializable, Iterable<Map.Entry<String, String>> {
  private Map<String, String> parameters;

  /**
   * Constructs an empty ParametersBag object.
   */
  public ParametersBag() {
    this.parameters = new HashMap<>();
  }

  /**
   * Constructs a ParametersBag object from a query string.
   *
   * @param query the query string
   */
  public ParametersBag(String query) {
    this.parameters = new HashMap<>();
    if (query != null && !query.isEmpty()) {
      String[] pairs = query.split("&");
      for (String pair : pairs) {
        String[] keyValue = pair.split("=", 2);
        parameters.put(keyValue[0], keyValue.length > 1 ? keyValue[1] : "");
      }
    }
  }

  /**
   * Constructs a ParametersBag object from a map of parameters.
   *
   * @param parameters the map of query parameters
   */
  public ParametersBag(Map<String, String> parameters) {
    this.parameters = new HashMap<>(parameters);
  }

  /**
   * Creates a new ParametersBag object.
   *
   * @return a new ParametersBag object
   */
  public static ParametersBag of(String query) {
    return new ParametersBag(query);
  }

  /**
   * Creates a new ParametersBag object.
   *
   * @return a new ParametersBag object
   */
  public static ParametersBag of(Map<String, String> parameters) {
    return new ParametersBag(parameters);
  }

  /**
   * Adds a new parameter.
   *
   * @param key the parameter key
   * @param value the parameter value
   */
  public void put(String key, String value) {
    parameters.put(key, value);
  }

  /**
   * Adds parameters.
   *
   * @param parameters the parameters to add
   */
  public void putAll(Map<String, String> parameters) {
    this.parameters.putAll(parameters);
  }

  /**
   * Removes a specific parameter.
   *
   * @param key the parameter key
   */
  public void remove(String key) {
    parameters.remove(key);
  }

  /**
   * Checks if a specific parameter is present.
   *
   * @param key the parameter key
   * @return true if the parameter is present, false otherwise
   */
  public boolean containsKey(String key) {
    return parameters.containsKey(key);
  }

  /**
   * Returns the number of parameters.
   *
   * @return the number of parameters
   */
  public int size() {
    return parameters.size();
  }

  /**
   * Returns all parameters.
   *
   * @return the parameters map
   */
  public Map<String, String> all() {
    return Collections.unmodifiableMap(parameters);
  }

  /**
   * Returns the value for a specific parameter.
   *
   * @param key the parameter key
   * @return an {@code Optional} containing the parameter value if present, or an empty
   *         {@code Optional} otherwise
   */
  public Optional<String> get(String key) {
    return Optional.ofNullable(parameters.get(key));
  }

  /**
   * Returns the alphabetic characters of the parameter value.
   *
   * @param key the parameter key
   * @return an {@code Optional} containing the alphabetic characters of the parameter value if
   *         present, or an empty {@code Optional} otherwise
   */
  public Optional<String> getAlpha(String key) {
    return get(key).map(value -> value.replaceAll("[^a-zA-Z]", ""));
  }

  /**
   * Returns the alphabetic characters and digits of the parameter value.
   *
   * @param key the parameter key
   * @return an {@code Optional} containing the alphabetic characters and digits of the parameter
   *         value if present, or an empty {@code Optional} otherwise
   */
  public Optional<String> getAlnum(String key) {
    return get(key).map(value -> value.replaceAll("[^a-zA-Z0-9]", ""));
  }

  /**
   * Returns the digits of the parameter value.
   *
   * @param key the parameter key
   * @return an {@code Optional} containing the digits of the parameter value if present, or an
   *         empty {@code Optional} otherwise
   */
  public Optional<String> getDigits(String key) {
    return get(key).map(value -> value.replaceAll("\\D", ""));
  }

  /**
   * Returns the parameter value as an integer.
   *
   * @param key the parameter key
   * @return an {@code Optional} containing the parameter value as an integer if present and
   *         parsable, or an empty {@code Optional} otherwise
   */
  public Optional<Integer> getInt(String key) {
    return get(key).flatMap(value -> {
      try {
        return Optional.of(Integer.parseInt(value));
      } catch (NumberFormatException e) {
        return Optional.empty();
      }
    });
  }

  /**
   * Returns the parameter value as a float.
   *
   * @param key the parameter key
   * @return an {@code Optional} containing the parameter value as a float if present and parsable,
   *         or an empty {@code Optional} otherwise
   */
  public Optional<Float> getFloat(String key) {
    return get(key).flatMap(value -> {
      try {
        return Optional.of(Float.parseFloat(value));
      } catch (NumberFormatException e) {
        return Optional.empty();
      }
    });
  }

  /**
   * Returns the parameter value as a double.
   *
   * @param key the parameter key
   * @return an {@code Optional} containing the parameter value as a double if present and parsable,
   *         or an empty {@code Optional} otherwise
   */
  public Optional<Double> getDouble(String key) {
    return get(key).flatMap(value -> {
      try {
        return Optional.of(Double.parseDouble(value));
      } catch (NumberFormatException e) {
        return Optional.empty();
      }
    });
  }

  /**
   * Returns the parameter value as a boolean.
   *
   * @param key the parameter key
   * @return an {@code Optional} containing the parameter value as a boolean if present and
   *         parsable, or an empty {@code Optional} otherwise
   */
  public Optional<Boolean> getBoolean(String key) {
    return get(key).map(Boolean::parseBoolean);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Iterator<Map.Entry<String, String>> iterator() {
    return Collections.unmodifiableMap(parameters).entrySet().iterator();
  }

  /**
   * Returns the query string representation of the parameters.
   *
   * @return the query string
   */
  public String getQueryString() {
    return parameters.entrySet().stream().map(entry -> entry.getKey() + "=" + entry.getValue())
        .collect(Collectors.joining("&"));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return getQueryString();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((parameters == null) ? 0 : parameters.hashCode());
    return result;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    ParametersBag other = (ParametersBag) obj;
    if (parameters == null) {
      if (other.parameters != null) {
        return false;
      }
    } else if (!parameters.equals(other.parameters)) {
      return false;
    }
    return true;
  }
}
