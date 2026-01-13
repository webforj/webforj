package com.webforj.environment;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import java.util.NoSuchElementException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class StringTableTest {
  BBjAPI mockApi;

  private final String testKey = "testKey";
  private final String testValue = "testValue";
  private final String nonExistentKey = "nonExistentKey";

  @BeforeEach
  void setUp() throws BBjException {
    mockApi = mock(BBjAPI.class);
    when(mockApi.getStbl(testKey)).thenReturn(testValue);
    Environment.init(mockApi, 0);
  }

  @AfterEach
  void tearDown() {
    Environment.cleanup();
  }

  @Test
  void shouldPutValueInStringTable() {
    StringTable.put(testKey, testValue);
    assertTrue(StringTable.contains(testKey));
  }

  @Test
  void shouldGetValueFromStringTable() throws BBjException {
    when(mockApi.getStbl(testKey)).thenReturn(testValue);
    String actualValue = StringTable.get(testKey);
    assertEquals(testValue, actualValue);
  }

  @Test
  void shouldThrowExceptionWhenKeyNotFound() throws BBjException {
    when(mockApi.getStbl(nonExistentKey)).thenThrow(new NoSuchElementException());
    assertThrows(NoSuchElementException.class, () -> StringTable.get(nonExistentKey));
  }

  @Test
  void shouldReturnTrueIfKeyExists() throws BBjException {
    when(mockApi.getStbl(testKey)).thenReturn(testValue);
    assertTrue(StringTable.contains(testKey));
  }

  @Test
  void shouldReturnFalseIfKeyNotFound() throws BBjException {
    when(mockApi.getStbl(nonExistentKey)).thenThrow(new NoSuchElementException());
    assertFalse(StringTable.contains(nonExistentKey));
  }
}
