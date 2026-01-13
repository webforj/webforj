package com.webforj.environment;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjObjectTable;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import java.util.NoSuchElementException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class ObjectTableTest {
  BBjObjectTable mockObjectTable;

  private final String testKey = "testKey";
  private final Object testValue = "testValue";
  private final String nonExistentKey = "nonExistentKey";

  @BeforeEach
  void setUp() throws BBjException {
    BBjAPI mockApi = mock(BBjAPI.class);
    mockObjectTable = mock(BBjObjectTable.class);
    when(mockApi.getObjectTable()).thenReturn(mockObjectTable);
    Environment.init(mockApi, 0);
  }

  @AfterEach
  void tearDown() {
    Environment.cleanup();
  }

  @Test
  void shouldPutValue() {
    ObjectTable.put(testKey, testValue);
    verify(mockObjectTable).put(testKey, testValue);
  }

  @Test
  void shouldGetValue() throws BBjException {
    when(mockObjectTable.get(testKey)).thenReturn(testValue);
    Object actualValue = ObjectTable.get(testKey);
    assertEquals(testValue, actualValue);
  }

  @Test
  void shouldThrowExceptionWhenKeyNotFound() throws BBjException {
    when(mockObjectTable.get(nonExistentKey)).thenThrow(new NoSuchElementException());
    assertThrows(NoSuchElementException.class, () -> ObjectTable.get(nonExistentKey));
  }

  @Test
  void shouldReturnTrueIfKeyExists() throws BBjException {
    when(mockObjectTable.get(testKey)).thenReturn(testValue);
    assertTrue(ObjectTable.contains(testKey));
  }

  @Test
  void shouldReturnFalseIfKeyNotFound() throws BBjException {
    when(mockObjectTable.get(nonExistentKey)).thenThrow(new NoSuchElementException());
    assertFalse(ObjectTable.contains(nonExistentKey));
  }

  @Test
  void shouldClearValueFromObjectTable() {
    ObjectTable.clear(testKey);
    verify(mockObjectTable).remove(testKey);
  }

  @Test
  void shouldReturnSizeOfObjectTable() {
    int expectedSize = 5;
    when(mockObjectTable.size()).thenReturn(expectedSize);

    int actualSize = ObjectTable.size();
    assertEquals(expectedSize, actualSize);
  }
}
