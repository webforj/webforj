package com.webforj.devtools.craftforj.inspector.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ClassTypeAdapterTest {

  private final ClassTypeAdapter adapter = new ClassTypeAdapter();

  @Test
  @DisplayName("Should resolve primitive names")
  void shouldResolvePrimitives() throws IOException {
    assertEquals(int.class, adapter.fromJson("\"int\""));
    assertEquals(long.class, adapter.fromJson("\"long\""));
    assertEquals(double.class, adapter.fromJson("\"double\""));
    assertEquals(float.class, adapter.fromJson("\"float\""));
    assertEquals(boolean.class, adapter.fromJson("\"boolean\""));
  }

  @Test
  @DisplayName("Should resolve boxed scalar names")
  void shouldResolveBoxedScalars() throws IOException {
    assertEquals(Integer.class, adapter.fromJson("\"java.lang.Integer\""));
    assertEquals(Long.class, adapter.fromJson("\"java.lang.Long\""));
    assertEquals(Double.class, adapter.fromJson("\"java.lang.Double\""));
    assertEquals(Float.class, adapter.fromJson("\"java.lang.Float\""));
    assertEquals(Boolean.class, adapter.fromJson("\"java.lang.Boolean\""));
  }

  @Test
  @DisplayName("Should never load arbitrary classes and fall back to String")
  void shouldFallBackToStringForUnknownNames() throws IOException {
    assertEquals(String.class, adapter.fromJson("\"java.lang.String\""));
    assertEquals(String.class, adapter.fromJson("\"com.example.NotAllowed\""));
    assertEquals(String.class, adapter.fromJson("\"java.lang.Runtime\""));
    assertEquals(String.class, adapter.fromJson("null"));
  }

  @Test
  @DisplayName("Should write the fully qualified class name")
  void shouldWriteClassName() throws IOException {
    assertEquals("\"java.lang.Integer\"", adapter.toJson(Integer.class));
  }
}
