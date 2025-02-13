package com.webforj.environment.namespace;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjNamespace;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.NoSuchElementException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class PrivateNamespaceTest {

  private Environment environment;
  private BBjNamespace bbjNamespace;

  @BeforeEach
  void setUp() throws BBjException {
    environment = mock(Environment.class);
    bbjNamespace = mock(BBjNamespace.class);
    Environment.init(mock(BBjAPI.class), mock(WebforjBBjBridge.class), 0);
    environment = Environment.getCurrent();
  }

  @AfterEach
  void tearDown() {
    Environment.cleanup();
  }

  @Test
  void shouldCreateNamespaceWhenMissing() throws BBjException {
    when(environment.getBBjAPI().getNamespace(anyString(), anyString(), eq(true)))
        .thenReturn(bbjNamespace);
    PrivateNamespace privateNamespace = new PrivateNamespace("prefix", "name", true);
    assertNotNull(privateNamespace);
  }

  @Test
  void shouldThrowExceptionWhenNamespaceCreationFails() throws BBjException {
    when(environment.getBBjAPI().getNamespace(anyString(), anyString(), eq(true)))
        .thenThrow(BBjException.class);
    assertThrows(WebforjRuntimeException.class, () -> new PrivateNamespace("prefix", "name", true));
  }

  @Test
  void shouldRetrieveExistingNamespace() throws BBjException {
    when(environment.getBBjAPI().getNamespace(anyString(), anyString(), eq(false)))
        .thenReturn(bbjNamespace);
    PrivateNamespace privateNamespace = new PrivateNamespace("prefix", "name", false);
    assertNotNull(privateNamespace);
  }

  @Test
  void shouldThrowExceptionWhenNamespaceNotFound() throws BBjException {
    when(environment.getBBjAPI().getNamespace(anyString(), anyString(), eq(false)))
        .thenThrow(BBjException.class);
    assertThrows(NoSuchElementException.class, () -> new PrivateNamespace("prefix", "name", false));
  }

  @Test
  void shouldCreateNamespaceWithDefaultCreateIfMissing() throws BBjException {
    when(environment.getBBjAPI().getNamespace(anyString(), anyString(), eq(true)))
        .thenReturn(bbjNamespace);
    PrivateNamespace privateNamespace = new PrivateNamespace("prefix", "name");
    assertNotNull(privateNamespace);
  }

  @Test
  void shouldCreateNamespaceWithPrefixOnly() throws BBjException {
    when(environment.getBBjAPI().getNewNamespace(anyString())).thenReturn(bbjNamespace);
    PrivateNamespace privateNamespace = new PrivateNamespace("prefix");
    assertNotNull(privateNamespace);
  }

  @Test
  void shouldThrowExceptionWhenNamespaceCreationWithPrefixFails() throws BBjException {
    when(environment.getBBjAPI().getNewNamespace(anyString())).thenThrow(BBjException.class);
    assertThrows(WebforjRuntimeException.class, () -> new PrivateNamespace("prefix"));
  }

  @Test
  void shouldRetrieveExistingNamespaceByName() throws BBjException {
    when(environment.getBBjAPI().getExistingNamespace(anyString())).thenReturn(bbjNamespace);
    PrivateNamespace privateNamespace = PrivateNamespace.ofExisting("name");
    assertNotNull(privateNamespace);
  }

  @Test
  void shouldThrowExceptionWhenExistingNamespaceNotFound() throws BBjException {
    when(environment.getBBjAPI().getExistingNamespace(anyString())).thenThrow(BBjException.class);
    assertThrows(NoSuchElementException.class, () -> PrivateNamespace.ofExisting("name"));
  }

  @Test
  void shouldReturnTrueWhenNamespaceIsPresent() throws BBjException {
    when(environment.getBBjAPI().getExistingNamespace(anyString())).thenReturn(bbjNamespace);
    assertTrue(PrivateNamespace.isPresent("name"));
  }

  @Test
  void shouldReturnFalseWhenNamespaceIsNotPresent() throws BBjException {
    when(environment.getBBjAPI().getExistingNamespace(anyString())).thenThrow(BBjException.class);
    assertFalse(PrivateNamespace.isPresent("name"));
  }

  @Test
  void shouldExecuteActionWhenNamespaceIsPresent() throws BBjException {
    when(environment.getBBjAPI().getExistingNamespace(anyString())).thenReturn(bbjNamespace);
    PrivateNamespace.ifPresent("name", namespace -> assertNotNull(namespace));
  }

  @Test
  void shouldNotExecuteActionWhenNamespaceIsNotPresent() throws BBjException {
    when(environment.getBBjAPI().getExistingNamespace(anyString())).thenThrow(BBjException.class);
    PrivateNamespace.ifPresent("name", namespace -> {
      throw new AssertionError("Action should not be executed");
    });
  }
}
