package com.webforj.environment.namespace;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjNamespace;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.bridge.WebforjBBjBridge;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class GroupNamespaceTest {

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
  void shouldRetrieveGroupNamespace() {
    when(environment.getBBjAPI().getGroupNamespace()).thenReturn(bbjNamespace);
    GroupNamespace groupNamespace = new GroupNamespace();
    assertNotNull(groupNamespace);
  }
}
