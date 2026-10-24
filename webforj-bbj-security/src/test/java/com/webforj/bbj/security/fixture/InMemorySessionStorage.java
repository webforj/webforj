package com.webforj.bbj.security.fixture;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.webstorage.SessionStorage;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Holds a session storage whose values live in a map of the test. A failing write throws an
 * exception whose message holds the values, like the real storage.
 */
public final class InMemorySessionStorage {
  private final Map<String, String> values = new LinkedHashMap<>();
  private final List<Map<String, String>> rejectedValues = new ArrayList<>();
  private final SessionStorage storage = mock(SessionStorage.class);
  private int failingWrites;

  /**
   * Creates an empty storage.
   */
  public InMemorySessionStorage() {
    when(storage.add(anyMap())).thenAnswer(call -> {
      Map<String, String> added = call.getArgument(0);
      if (failingWrites > 0) {
        failingWrites--;
        rejectedValues.add(Map.copyOf(added));
        throw new WebforjRuntimeException("Failed to add values '" + added + "' to the storage.");
      }

      values.putAll(added);
      return storage;
    });
    when(storage.get(anyList())).thenAnswer(call -> {
      Map<String, String> found = new LinkedHashMap<>();
      call.<List<String>>getArgument(0).forEach(key -> {
        if (values.containsKey(key)) {
          found.put(key, values.get(key));
        }
      });
      return found;
    });
    doAnswer(call -> {
      for (Object key : call.getArguments()) {
        values.remove(key);
      }

      return null;
    }).when(storage).remove(any(String[].class));
  }

  /**
   * Makes the next writes fail.
   *
   * @param count how many of the next writes fail
   */
  public void setFailingWrites(int count) {
    failingWrites = count;
  }

  /**
   * Gets the storage to hand to the code under test.
   *
   * @return the storage
   */
  public SessionStorage getStorage() {
    return storage;
  }

  /**
   * Gets the values the storage holds. The map can be changed by the test.
   *
   * @return the values by key
   */
  public Map<String, String> getValues() {
    return values;
  }

  /**
   * Gets the values of every write that failed.
   *
   * @return the values in the order of the writes
   */
  public List<Map<String, String>> getRejectedValues() {
    return rejectedValues;
  }
}
