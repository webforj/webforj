package com.webforj.component.element;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;

import com.webforj.environment.ObjectTable;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class DefinedElementNamesTest {
  private MockedStatic<ObjectTable> objectTable;
  private Map<String, Object> session;

  @BeforeEach
  void setUp() {
    session = new HashMap<>();
    objectTable = mockStatic(ObjectTable.class);
    objectTable.when(() -> ObjectTable.contains(anyString()))
        .thenAnswer(call -> session.containsKey(call.getArgument(0)));
    objectTable.when(() -> ObjectTable.get(anyString()))
        .thenAnswer(call -> session.get(call.getArgument(0)));
    objectTable.when(() -> ObjectTable.put(anyString(), any()))
        .thenAnswer(call -> session.put(call.getArgument(0), call.getArgument(1)));
  }

  @AfterEach
  void tearDown() {
    objectTable.close();
  }

  @Test
  void shouldContainAddedName() {
    DefinedElementNames.add("x-one");
    DefinedElementNames.add("x-two");

    assertTrue(DefinedElementNames.contains("x-one"));
    assertTrue(DefinedElementNames.contains("x-two"));
  }

  @Test
  void shouldNotContainUnknownName() {
    DefinedElementNames.add("x-one");

    assertFalse(DefinedElementNames.contains("x-other"));
  }

  @Test
  void shouldIgnoreNullName() {
    DefinedElementNames.add(null);

    assertFalse(DefinedElementNames.contains(null));
    assertTrue(session.isEmpty());
  }

  @Test
  void shouldKeepNamesInOneSessionEntry() {
    DefinedElementNames.add("x-one");
    DefinedElementNames.add("x-two");

    assertEquals(1, session.size());
  }
}
