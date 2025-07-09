package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

import com.webforj.servlet.WebforjServlet;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.web.servlet.ServletRegistrationBean;

/**
 * Unit tests for {@link SpringAutoConfiguration}.
 *
 * @author Hyyan Abo Fakher
 */
@ExtendWith(MockitoExtension.class)
class SpringAutoConfigurationTest {

  @Mock
  private SpringConfigurationProperties properties;

  private SpringAutoConfiguration autoConfiguration;

  @BeforeEach
  void setUp() {
    autoConfiguration = new SpringAutoConfiguration();
  }

  @Nested
  class ServletRegistration {

    @Test
    void shouldCreateServletRegistrationBeanWithDefaultMapping() {
      when(properties.getServletMapping()).thenReturn("/*");

      ServletRegistrationBean<WebforjServlet> registrationBean =
          autoConfiguration.webforjServletRegistration(properties);

      assertNotNull(registrationBean);
      assertNotNull(registrationBean.getServlet());
      assertEquals(WebforjServlet.class, registrationBean.getServlet().getClass());
    }

    @Test
    void shouldCreateServletRegistrationBeanWithCustomMapping() {
      String customMapping = "/webforj/*";
      when(properties.getServletMapping()).thenReturn(customMapping);

      ServletRegistrationBean<WebforjServlet> registrationBean =
          autoConfiguration.webforjServletRegistration(properties);

      assertNotNull(registrationBean);
      assertNotNull(registrationBean.getServlet());
      assertEquals(WebforjServlet.class, registrationBean.getServlet().getClass());
    }

    @Test
    void shouldCreateNewWebforjServletInstance() {
      when(properties.getServletMapping()).thenReturn("/*");

      ServletRegistrationBean<WebforjServlet> registrationBean1 =
          autoConfiguration.webforjServletRegistration(properties);
      ServletRegistrationBean<WebforjServlet> registrationBean2 =
          autoConfiguration.webforjServletRegistration(properties);

      assertNotNull(registrationBean1.getServlet());
      assertNotNull(registrationBean2.getServlet());
      assert registrationBean1.getServlet() != registrationBean2.getServlet();
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
      ComponentRegistrar registrar = autoConfiguration.webforjComponentRegistrar();

      assertNotNull(registrar);
      assertEquals(ComponentRegistrar.class, registrar.getClass());
    }

    @Test
    void shouldCreateNewComponentRegistrarInstanceEachTime() {
      ComponentRegistrar registrar1 = autoConfiguration.webforjComponentRegistrar();
      ComponentRegistrar registrar2 = autoConfiguration.webforjComponentRegistrar();

      assertNotNull(registrar1);
      assertNotNull(registrar2);
      assert registrar1 != registrar2;
    }
  }
}
