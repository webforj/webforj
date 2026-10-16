package com.webforj.bbjship.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class DeploymentRegistrationTest {

  @Test
  void shouldPreserveRegistrationDataAfterTheSuppliedCollectionsChange() {
    Map<String, Object> properties = new LinkedHashMap<>();
    properties.put("name", "sample");
    properties.put("enabled", true);
    List<String> arguments = new ArrayList<>(List.of("argument"));
    List<String> javaArguments = new ArrayList<>(List.of("java-argument"));

    final DeploymentRegistration registration =
        new DeploymentRegistration(properties, arguments, javaArguments);
    properties.clear();
    arguments.clear();
    javaArguments.clear();

    assertEquals(Map.of("name", "sample", "enabled", true), registration.getProperties());
    assertEquals(List.of("argument"), registration.getArguments());
    assertEquals(List.of("java-argument"), registration.getJavaArguments());
    Map<String, Object> storedProperties = registration.getProperties();
    assertThrows(UnsupportedOperationException.class, storedProperties::clear);
    List<String> storedArguments = registration.getArguments();
    assertThrows(UnsupportedOperationException.class, storedArguments::clear);
    List<String> storedJavaArguments = registration.getJavaArguments();
    assertThrows(UnsupportedOperationException.class, storedJavaArguments::clear);
  }
}
