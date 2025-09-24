package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigValue;
import com.webforj.servlet.WebforjServlet;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.web.servlet.ServletRegistrationBean;

@ExtendWith(MockitoExtension.class)
class SpringAutoConfigurationTest {

  @Mock
  private SpringConfigurationProperties properties;

  @Mock
  private Config config;

  private SpringAutoConfiguration autoConfiguration;

  @BeforeEach
  void setUp() {
    autoConfiguration = new SpringAutoConfiguration();
  }

  @Nested
  class ServletRegistration {

    @Test
    void shouldCreateServletRegistrationBeanWithRootMapping() {
      when(properties.getServletMapping()).thenReturn("/*");
      when(config.withValue(eq("webforj.router.root"), any(ConfigValue.class))).thenReturn(config);

      try (MockedStatic<WebforjServlet> mockedServlet = mockStatic(WebforjServlet.class)) {
        ServletRegistrationBean<WebforjServlet> registrationBean =
            autoConfiguration.webforjServletRegistration(properties, config);

        assertNotNull(registrationBean);
        assertNotNull(registrationBean.getServlet());
        assertEquals(WebforjServlet.class, registrationBean.getServlet().getClass());

        // Should use servlet name for forwarding
        assertEquals(WebforjServletConfiguration.WEBFORJ_SERVLET_NAME,
            registrationBean.getServletName());

        // Verify setConfig was called with the provided config
        mockedServlet.verify(() -> WebforjServlet.setConfig(config), times(1));
      }
    }

    @Test
    void shouldCreateServletRegistrationBeanWithCustomMapping() {
      String customMapping = "/webforj/*";
      when(properties.getServletMapping()).thenReturn(customMapping);

      try (MockedStatic<WebforjServlet> mockedServlet = mockStatic(WebforjServlet.class)) {
        ServletRegistrationBean<WebforjServlet> registrationBean =
            autoConfiguration.webforjServletRegistration(properties, config);

        assertNotNull(registrationBean);
        assertNotNull(registrationBean.getServlet());
        assertEquals(WebforjServlet.class, registrationBean.getServlet().getClass());

        // Should still use servlet name for consistency
        assertEquals(WebforjServletConfiguration.WEBFORJ_SERVLET_NAME,
            registrationBean.getServletName());

        // Verify setConfig was called with the provided config
        mockedServlet.verify(() -> WebforjServlet.setConfig(config), times(1));
      }
    }

    @Test
    void shouldCreateNewWebforjServletInstance() {
      when(properties.getServletMapping()).thenReturn("/*");
      when(config.withValue(eq("webforj.router.root"), any(ConfigValue.class))).thenReturn(config);

      try (MockedStatic<WebforjServlet> mockedServlet = mockStatic(WebforjServlet.class)) {
        ServletRegistrationBean<WebforjServlet> registrationBean1 =
            autoConfiguration.webforjServletRegistration(properties, config);
        ServletRegistrationBean<WebforjServlet> registrationBean2 =
            autoConfiguration.webforjServletRegistration(properties, config);

        assertNotNull(registrationBean1.getServlet());
        assertNotNull(registrationBean2.getServlet());
        assert registrationBean1.getServlet() != registrationBean2.getServlet();

        // Verify setConfig was called twice (once for each registration)
        mockedServlet.verify(() -> WebforjServlet.setConfig(config), times(2));
      }
    }
  }

  @Nested
  class ConfigBean {

    @Test
    void shouldCreateWebforjConfigFromProperties() {
      Config configResult = autoConfiguration.webforjConfig(properties);

      assertNotNull(configResult);
    }
  }

  @Nested
  class ContextInjectorBean {

    @Test
    void shouldCreateContextInjectorBean() {
      ContextInjector contextInjector = autoConfiguration.webforjContextInjector();

      assertNotNull(contextInjector);
      assertEquals(ContextInjector.class, contextInjector.getClass());
    }

    @Test
    void shouldCreateNewContextInjectorInstanceEachTime() {
      ContextInjector contextInjector1 = autoConfiguration.webforjContextInjector();
      ContextInjector contextInjector2 = autoConfiguration.webforjContextInjector();

      assertNotNull(contextInjector1);
      assertNotNull(contextInjector2);
      assert contextInjector1 != contextInjector2;
    }
  }

  @Nested
  class ComponentRegistrarBean {

    @Test
    void shouldCreateComponentRegistrarBean() {
      ComponentRegistrar registrar = SpringAutoConfiguration.webforjComponentRegistrar();

      assertNotNull(registrar);
      assertEquals(ComponentRegistrar.class, registrar.getClass());
    }

    @Test
    void shouldCreateNewComponentRegistrarInstanceEachTime() {
      ComponentRegistrar registrar1 = SpringAutoConfiguration.webforjComponentRegistrar();
      ComponentRegistrar registrar2 = SpringAutoConfiguration.webforjComponentRegistrar();

      assertNotNull(registrar1);
      assertNotNull(registrar2);
      assert registrar1 != registrar2;
    }
  }
}
