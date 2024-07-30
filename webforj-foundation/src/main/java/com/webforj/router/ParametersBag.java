package com.webforj.router;

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
 * @since 24.11
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
   * @param defaultValue the default value
   *
   * @return the parameter value
   */
  public String get(String key, String defaultValue) {
    return parameters.getOrDefault(key, defaultValue);
  }

  /**
   * Returns the value for a specific parameter.
   *
   * @param key the parameter key
   * @return the parameter value
   */
  public Optional<String> get(String key) {
    return Optional.ofNullable(get(key, null));
  }

  /**
   * Returns the alphabetic characters of the parameter value.
   *
   * @param key the parameter key
   * @param defaultValue the default value
   *
   * @return the alphabetic characters of the parameter value
   */
  public String getAlpha(String key, String defaultValue) {
    String value = get(key).orElse(defaultValue);
    if (value == null) {
      return null;
    }

    return value.replaceAll("[^a-zA-Z]", "");
  }

  /**
   * Returns the alphabetic characters of the parameter value.
   *
   * @param key the parameter key
   * @return the alphabetic characters of the parameter value
   */
  public String getAlpha(String key) {
    return getAlpha(key, null);
  }

  /**
   * Returns the alphabetic characters and digits of the parameter value.
   *
   * @param key the parameter key
   * @param defaultValue the default value
   *
   * @return the alphabetic characters and digits of the parameter value
   */
  public String getAlnum(String key, String defaultValue) {
    String value = get(key).orElse(defaultValue);
    if (value == null) {
      return null;
    }

    return value.replaceAll("[^a-zA-Z0-9]", "");
  }

  /**
   * Returns the alphabetic characters and digits of the parameter value.
   *
   * @param key the parameter key
   * @return the alphabetic characters and digits of the parameter value
   */
  public Optional<String> getAlnum(String key) {
    return Optional.ofNullable(getAlnum(key, null));
  }

  /**
   * Returns the digits of the parameter value.
   *
   * @param key the parameter key
   * @param defaultValue the default value
   *
   * @return the digits of the parameter value
   */
  public String getDigits(String key, String defaultValue) {
    String value = get(key).orElse(defaultValue);
    if (value == null) {
      return null;
    }

    return value.replaceAll("\\D", "");
  }

  /**
   * Returns the digits of the parameter value.
   *
   * @param key the parameter key
   * @return the digits of the parameter value
   */
  public Optional<String> getDigits(String key) {
    return Optional.ofNullable(getDigits(key, null));
  }

  /**
   * Returns the parameter value as an integer.
   *
   * @param key the parameter key
   * @param defaultValue the default value
   * @return the parameter value as an integer
   */
  public Integer getInt(String key, Integer defaultValue) {
    try {
      return Integer.parseInt(get(key, String.valueOf(defaultValue)));
    } catch (NumberFormatException e) {
      return defaultValue;
    }
  }

  /**
   * Returns the parameter value as an integer.
   *
   * @param key the parameter key
   * @return the parameter value as an integer
   */
  public Optional<Integer> getInt(String key) {
    return Optional.ofNullable(getInt(key, null));
  }

  /**
   * Returns the parameter value as a float.
   *
   * @param key the parameter key
   * @param defaultValue the default value
   * @return the parameter value as a float
   */
  public Float getFloat(String key, Float defaultValue) {
    try {
      return Float.parseFloat(get(key, String.valueOf(defaultValue)));
    } catch (NumberFormatException e) {
      return defaultValue;
    }
  }

  /**
   * Returns the parameter value as a float.
   *
   * @param key the parameter key
   * @return the parameter value as a float
   */
  public Optional<Float> getFloat(String key) {
    return Optional.ofNullable(getFloat(key, null));
  }

  /**
   * Returns the parameter value as a double.
   *
   * @param key the parameter key
   * @param defaultValue the default value
   * @return the parameter value as a double
   */
  public Double getDouble(String key, Double defaultValue) {
    try {
      return Double.parseDouble(get(key, String.valueOf(defaultValue)));
    } catch (NumberFormatException e) {
      return defaultValue;
    }
  }

  /**
   * Returns the parameter value as a double.
   *
   * @param key the parameter key
   * @return the parameter value as a double
   */
  public Optional<Double> getDouble(String key) {
    return Optional.ofNullable(getDouble(key, null));
  }

  /**
   * Returns the parameter value as a boolean.
   *
   * @param key the parameter key
   * @param defaultValue the default value
   * @return the parameter value as a boolean
   */
  public Boolean getBoolean(String key, Boolean defaultValue) {
    return Boolean.parseBoolean(get(key, String.valueOf(defaultValue)));
  }

  /**
   * Returns the parameter value as a boolean.
   *
   * @param key the parameter key
   * @return the parameter value as a boolean
   */
  public Optional<Boolean> getBoolean(String key) {
    return Optional.ofNullable(getBoolean(key, null));
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
}
