package com.webforj.devtools.craftforj.inspector.model;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;
import java.io.IOException;

/**
 * Gson adapter for Class serialization.
 *
 * <p>
 * Serializes Class to its fully qualified name. Deserialization resolves only the scalar types the
 * source generators support, never arbitrary classes, so a client-supplied name cannot load
 * classpath classes. Unknown names fall back to String.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ClassTypeAdapter extends TypeAdapter<Class<?>> {

  /**
   * {@inheritDoc}
   */
  @Override
  public void write(JsonWriter out, Class<?> value) throws IOException {
    if (value == null) {
      out.nullValue();
    } else {
      out.value(value.getName());
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<?> read(JsonReader in) throws IOException {
    if (in.peek() == JsonToken.NULL) {
      in.nextNull();
      return String.class;
    }

    return resolveClass(in.nextString());
  }

  private Class<?> resolveClass(String className) {
    return switch (className) {
      case "int" -> int.class;
      case "long" -> long.class;
      case "double" -> double.class;
      case "float" -> float.class;
      case "boolean" -> boolean.class;
      case "byte" -> byte.class;
      case "short" -> short.class;
      case "char" -> char.class;
      case "java.lang.Integer" -> Integer.class;
      case "java.lang.Long" -> Long.class;
      case "java.lang.Double" -> Double.class;
      case "java.lang.Float" -> Float.class;
      case "java.lang.Boolean" -> Boolean.class;
      default -> String.class;
    };
  }
}
