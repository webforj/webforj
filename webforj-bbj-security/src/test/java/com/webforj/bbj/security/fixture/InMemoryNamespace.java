package com.webforj.bbj.security.fixture;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.environment.namespace.GlobalNamespace;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * Holds a global namespace whose values live in a map of the test.
 */
public final class InMemoryNamespace {
  private final Map<String, Object> values = new LinkedHashMap<>();
  private final GlobalNamespace namespace = mock(GlobalNamespace.class);

  /**
   * Creates an empty namespace.
   */
  public InMemoryNamespace() {
    try {
      doAnswer(call -> {
        values.put(call.getArgument(0), call.getArgument(1));
        return null;
      }).when(namespace).put(anyString(), any());
      doAnswer(call -> {
        values.remove(call.<String>getArgument(0));
        return null;
      }).when(namespace).remove(anyString());
    } catch (Exception e) {
      throw new IllegalStateException(e);
    }

    when(namespace.getOrDefault(anyString(), any()))
        .thenAnswer(call -> values.getOrDefault(call.<String>getArgument(0), call.getArgument(1)));
    when(namespace.keySet()).thenAnswer(call -> new LinkedHashSet<>(values.keySet()));
  }

  /**
   * Gets the namespace to hand to the code under test.
   *
   * @return the namespace
   */
  public GlobalNamespace getNamespace() {
    return namespace;
  }

  /**
   * Gets the values the namespace holds. The map can be changed by the test.
   *
   * @return the values by key
   */
  public Map<String, Object> getValues() {
    return values;
  }
}
