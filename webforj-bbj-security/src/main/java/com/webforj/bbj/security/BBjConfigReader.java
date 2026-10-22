package com.webforj.bbj.security;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigException;
import com.typesafe.config.ConfigObject;
import com.typesafe.config.ConfigUtil;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Predicate;

/**
 * Reads typed values from a {@link Config} and logs what it finds.
 *
 * <p>
 * A key that is not set, or is set to {@code null}, is logged at debug level and gives the default.
 * The {@code read} methods log an invalid value once as a warning, naming the key and never the
 * value, and give the safe value. The {@code require} methods throw an
 * {@link IllegalArgumentException} naming the key and never the value.
 * </p>
 *
 * <p>
 * Keys below a path that are not known are logged once each as a warning, naming the key and never
 * the value.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjConfigReader {
  private static final String MISSING_MESSAGE = "{0} is not set, using {1}";
  private static final String UNKNOWN_MESSAGE = "{0} is not a known key and is ignored";
  private static final String BOOLEAN_EXPECTED = "true or false";
  private final Config config;
  private final Logger logger;

  BBjConfigReader(Config config, Logger logger) {
    this.config = config;
    this.logger = logger;
  }

  /**
   * Reads a boolean.
   *
   * @param key the key to read
   * @param missingValue the value for a key that is not set
   * @param invalidValue the value for an invalid value
   * @return the boolean
   */
  boolean readBoolean(String key, boolean missingValue, boolean invalidValue) {
    try {
      return find(key, Config::getBoolean, String.valueOf(missingValue), BOOLEAN_EXPECTED,
          value -> true).orElse(missingValue);
    } catch (IllegalArgumentException e) {
      logger.log(Level.WARNING, "{0}, using {1}", e.getMessage(), invalidValue);
      return invalidValue;
    }
  }

  /**
   * Reads a boolean and throws for an invalid value.
   *
   * @param key the key to read
   * @param missingValue the value for a key that is not set
   * @return the boolean
   *
   * @throws IllegalArgumentException when the value is not a boolean
   */
  boolean requireBoolean(String key, boolean missingValue) {
    return find(key, Config::getBoolean, String.valueOf(missingValue), BOOLEAN_EXPECTED,
        value -> true).orElse(missingValue);
  }

  /**
   * Reads a whole number within bounds.
   *
   * @param key the key to read
   * @param missingValue the value for a key that is not set and for an invalid value
   * @param min the lowest valid value
   * @param max the highest valid value
   * @return the number
   */
  int readInt(String key, int missingValue, int min, int max) {
    try {
      return requireInt(key, missingValue, min, max);
    } catch (IllegalArgumentException e) {
      logger.log(Level.WARNING, "{0}, using {1}", e.getMessage(), String.valueOf(missingValue));
      return missingValue;
    }
  }

  /**
   * Reads a whole number within bounds and throws for an invalid value.
   *
   * @param key the key to read
   * @param missingValue the value for a key that is not set
   * @param min the lowest valid value
   * @param max the highest valid value
   * @return the number
   *
   * @throws IllegalArgumentException when the value is not a whole number from min to max
   */
  int requireInt(String key, int missingValue, int min, int max) {
    return find(key, BBjConfigReader::getWholeNumber, String.valueOf(missingValue),
        getIntegerExpected(min, max), value -> value >= min && value <= max).map(Long::intValue)
        .orElse(missingValue);
  }

  /**
   * Reads a whole number and lowers a value above the maximum to the maximum.
   *
   * <p>
   * A value above the maximum is logged once as a warning. A value below the minimum or not a whole
   * number is logged once as a warning and gives the default.
   * </p>
   *
   * @param key the key to read
   * @param missingValue the value for a key that is not set and for an invalid value
   * @param min the lowest valid value
   * @param max the highest valid value
   * @return the number
   */
  int readClampedInt(String key, int missingValue, int min, int max) {
    try {
      Optional<Long> value = find(key, BBjConfigReader::getWholeNumber,
          String.valueOf(missingValue), getIntegerExpected(min, max), number -> number >= min);
      if (value.isPresent() && value.get() > max) {
        logger.log(Level.WARNING, "Value of {0} is above {1}, using {1}", key, String.valueOf(max));
        return max;
      }

      return value.map(Long::intValue).orElse(missingValue);
    } catch (IllegalArgumentException e) {
      logger.log(Level.WARNING, "{0}, using {1}", e.getMessage(), String.valueOf(missingValue));
      return missingValue;
    }
  }

  /**
   * Reads a string.
   *
   * @param key the key to read
   * @param defaultText the text of the value used for a key that is not set and for an invalid
   *        value
   * @param validator tells whether a string is valid
   * @param expected what a valid string looks like
   * @return the string, or empty when the key is not set or the value is invalid
   */
  Optional<String> readString(String key, String defaultText, Predicate<String> validator,
      String expected) {
    try {
      return requireString(key, defaultText, validator, expected);
    } catch (IllegalArgumentException e) {
      logger.log(Level.WARNING, "{0}, using {1}", e.getMessage(), defaultText);
      return Optional.empty();
    }
  }

  /**
   * Reads a string and throws for an invalid value.
   *
   * @param key the key to read
   * @param defaultText the text of the value used for a key that is not set
   * @param validator tells whether a string is valid
   * @param expected what a valid string looks like
   * @return the string, or empty when the key is not set
   *
   * @throws IllegalArgumentException when the value is not a string or the validator rejects it
   */
  Optional<String> requireString(String key, String defaultText, Predicate<String> validator,
      String expected) {
    return find(key, Config::getString, defaultText, expected, validator);
  }

  /**
   * Logs a warning for every key below a path that is not known.
   *
   * <p>
   * Only the names of the keys are read, so a value that cannot be resolved never throws. A path
   * that is not set, or does not hold an object, logs nothing.
   * </p>
   *
   * @param parentPath the path of the object to check
   * @param knownKeys the full paths of the known keys
   */
  void warnUnknownKeys(String parentPath, Set<String> knownKeys) {
    List<String> keys = new ArrayList<>();
    try {
      if (config.hasPath(parentPath)) {
        findKeys(config.getObject(parentPath), ConfigUtil.splitPath(parentPath), keys);
      }
    } catch (ConfigException e) {
      return;
    }

    keys.stream().filter(key -> !knownKeys.contains(key))
        .forEach(key -> logger.log(Level.WARNING, UNKNOWN_MESSAGE, key));
  }

  private <T> Optional<T> find(String key, BiFunction<Config, String, T> getter, String missingText,
      String expected, Predicate<T> validator) {
    T value;
    try {
      if (!config.hasPath(key)) {
        logger.log(Level.DEBUG, MISSING_MESSAGE, key, missingText);
        return Optional.empty();
      }

      value = getter.apply(config, key);
    } catch (ConfigException e) {
      throw createInvalidException(key, expected);
    }

    if (!validator.test(value)) {
      throw createInvalidException(key, expected);
    }

    return Optional.of(value);
  }

  private static void findKeys(ConfigObject object, List<String> path, List<String> keys) {
    for (String name : object.keySet()) {
      List<String> keyPath = new ArrayList<>(path);
      keyPath.add(name);
      if (object.get(name) instanceof ConfigObject child) {
        findKeys(child, keyPath, keys);
      } else {
        keys.add(ConfigUtil.joinPath(keyPath));
      }
    }
  }

  private static IllegalArgumentException createInvalidException(String key, String expected) {
    return new IllegalArgumentException("Invalid value for " + key + ", expected " + expected);
  }

  private static String getIntegerExpected(int min, int max) {
    return "a whole number from " + min + " to " + max;
  }

  private static Long getWholeNumber(Config config, String key) {
    Number number = config.getNumber(key);
    if (number.doubleValue() != number.longValue()) {
      throw new ConfigException.BadValue(key, "not a whole number");
    }

    return number.longValue();
  }
}
