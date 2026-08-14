package com.webforj.spring.mcp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.component.window.Window;
import com.webforj.mcp.McpAppContribution;
import com.webforj.mcp.annotation.McpApp;
import com.webforj.mcp.filter.CorsFilter;
import com.webforj.router.annotation.Route;
import com.webforj.servlet.WebforjServlet;
import com.webforj.spring.WebforjServletConfiguration;
import io.modelcontextprotocol.server.McpServerFeatures.SyncResourceSpecification;
import io.modelcontextprotocol.spec.McpSchema.ReadResourceRequest;
import io.modelcontextprotocol.spec.McpSchema.ReadResourceResult;
import io.modelcontextprotocol.spec.McpSchema.TextResourceContents;
import jakarta.servlet.DispatcherType;
import jakarta.servlet.FilterRegistration;
import jakarta.servlet.ServletContext;
import jakarta.servlet.SessionCookieConfig;
import java.util.EnumSet;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.context.PropertyPlaceholderAutoConfiguration;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.boot.web.servlet.ServletContextInitializer;
import org.springframework.boot.web.servlet.ServletRegistrationBean;

class McpAppsAutoConfigurationTest {

  private final ApplicationContextRunner runner =
      new ApplicationContextRunner().withConfiguration(AutoConfigurations
          .of(PropertyPlaceholderAutoConfiguration.class, McpAppsAutoConfiguration.class))
          .withBean("webforjServletRegistration", ServletRegistrationBean.class,
              () -> new ServletRegistrationBean<>(new WebforjServlet(),
                  WebforjServletConfiguration.WEBFORJ_SERVLET_MAPPING));

  @Test
  @DisplayName("Should publish the contribution beans Spring AI collects")
  void shouldPublishContributionBeans() {
    runner.run(context -> {
      McpAppContribution contribution = context.getBean(McpAppContribution.class);

      assertTrue(context.containsBean("webforjMcpTools"));
      assertTrue(context.containsBean("webforjMcpResources"));
      assertTrue(contribution.getResourceSpecifications().isEmpty(),
          "a deployment without marked views contributes no resources");
    });
  }

  @Test
  @DisplayName("Should carry the origin from the application configuration")
  void shouldCarryOriginFromConfiguration() {
    runner.withPropertyValues("webforj.origin=https://demo.example").run(context -> {
      McpAppContribution contribution = context.getBean(McpAppContribution.class);

      assertEquals("https://demo.example", contribution.getOrigin().resolve());
    });
  }

  @Test
  @DisplayName("Should address the embed bootstrap under the path of the servlet registration")
  void shouldAddressEmbedBootstrapUnderServletRegistrationPath() {
    runner.withPropertyValues("webforj.origin=https://demo.example")
        .withInitializer(context -> ((BeanDefinitionRegistry) context.getBeanFactory())
            .registerBeanDefinition("probeView", new RootBeanDefinition(ProbeView.class)))
        .run(context -> {
          McpAppContribution contribution = context.getBean(McpAppContribution.class);
          List<SyncResourceSpecification> resources = contribution.getResourceSpecifications();

          assertFalse(resources.isEmpty(), "the marked view contributes a resource");

          ReadResourceResult result = resources.get(0).readHandler().apply(null,
              ReadResourceRequest.builder(resources.get(0).resource().uri()).build());
          String rendered = ((TextResourceContents) result.contents().get(0)).text();

          assertTrue(rendered.contains("https://demo.example/webforjServlet/dwcembed/webforj.js"),
              "the embed bootstrap loads from the path of the servlet registration");
        });
  }

  @Test
  @DisplayName("Should install the embedding support into the deployment")
  void shouldInstallEmbeddingSupport() {
    runner.withPropertyValues("webforj.origin=https://demo.example",
        "webforj.components=https://cdn.example/components").run(context -> {
          ServletContextInitializer installer =
              context.getBean("webforjMcpAppInstaller", ServletContextInitializer.class);
          ServletContext servletContext = mock(ServletContext.class);
          FilterRegistration.Dynamic cors = mock(FilterRegistration.Dynamic.class);
          SessionCookieConfig cookies = mock(SessionCookieConfig.class);
          when(servletContext.addFilter(eq("webforjMcpCors"), any(CorsFilter.class)))
              .thenReturn(cors);
          when(servletContext.getSessionCookieConfig()).thenReturn(cookies);

          installer.onStartup(servletContext);

          verify(cors).addMappingForUrlPatterns(EnumSet.of(DispatcherType.REQUEST), false, "/*");
          verify(cookies).setSecure(true);
          verify(cookies).setAttribute("SameSite", "None");
          verify(cookies).setAttribute("Partitioned", "");
        });
  }

  @Route("/probe")
  @McpApp(description = "Probe view")
  public static class ProbeView extends Component {

    @Override
    protected void onCreate(Window window) {
      // Fixture, never rendered
    }

    @Override
    protected void onDestroy() {
      // Fixture, never rendered
    }
  }
}
