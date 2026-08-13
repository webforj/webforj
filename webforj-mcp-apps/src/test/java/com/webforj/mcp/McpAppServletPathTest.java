package com.webforj.mcp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletRegistration;
import java.util.Map;
import java.util.Set;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class McpAppServletPathTest {

  @Test
  @DisplayName("Should contribute no prefix for a servlet on the root")
  void shouldContributeNoPrefixForRoot() {
    assertEquals("", McpAppServletPath.normalize("/*"));
    assertEquals("", McpAppServletPath.normalize("/"));
    assertEquals("", McpAppServletPath.normalize(null));
  }

  @Test
  @DisplayName("Should contribute the sub path a remapped servlet answers on")
  void shouldContributeSubPath() {
    assertEquals("/ui", McpAppServletPath.normalize("/ui/*"));
    assertEquals("/webforjServlet", McpAppServletPath.normalize("webforjServlet"));
  }

  @Test
  @DisplayName("Should read the prefix out of the deployment")
  void shouldReadPrefixOutOfDeployment() {
    ServletRegistration registration = mock(ServletRegistration.class);
    when(registration.getClassName()).thenReturn("com.webforj.servlet.WebforjServlet");
    when(registration.getMappings()).thenReturn(Set.of("/ui/*"));

    ServletContext context = mock(ServletContext.class);
    doReturn(Map.of("WebforjServlet", registration)).when(context).getServletRegistrations();

    assertEquals("/ui", McpAppServletPath.of(context));
  }

  @Test
  @DisplayName("Should contribute no prefix when the deployment holds no webforJ servlet")
  void shouldContributeNoPrefixWithoutWebforjServlet() {
    ServletContext context = mock(ServletContext.class);
    doReturn(Map.of()).when(context).getServletRegistrations();

    assertEquals("", McpAppServletPath.of(context));
    assertEquals("", McpAppServletPath.of(null));
  }
}
