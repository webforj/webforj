package com.webforj.testbed.runtime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

class ScriptAnswerRegistryTest {

  @Test
  void shouldAnswerTheWaitingScriptsInTheOrderTheyStarted() {
    ScriptAnswerRegistry registry = new ScriptAnswerRegistry();
    List<Object> results = new ArrayList<>();
    registry.await(result -> results.add("first " + result));
    registry.await(result -> results.add("second " + result));

    registry.answerExecuteJsAsync(1);
    registry.answerExecuteJsAsync(null);

    assertEquals(List.of("first 1", "second null"), results);
  }

  @Test
  void shouldRefuseAnAnswerNoScriptWaitsFor() {
    ScriptAnswerRegistry registry = new ScriptAnswerRegistry();

    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> registry.answerExecuteJsAsync(1));

    assertTrue(failure.getMessage().contains("executeJsAsync"));
  }

  @Test
  void shouldReturnTheScriptedResultsOfExecuteJsInOrderAndNullWithoutOne() {
    ScriptAnswerRegistry registry = new ScriptAnswerRegistry();
    registry.answerExecuteJs(320);
    registry.answerExecuteJs(null);

    assertEquals(320, registry.nextExecuteJsResult());
    assertNull(registry.nextExecuteJsResult());
    assertNull(registry.nextExecuteJsResult());
  }
}
