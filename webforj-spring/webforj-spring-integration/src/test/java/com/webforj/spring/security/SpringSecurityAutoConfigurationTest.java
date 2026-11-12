package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import com.webforj.spring.SpringConfigurationProperties;
import com.webforj.spring.security.evaluator.SpringRouteAccessEvaluator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.context.SecurityContextHolderStrategy;

@ExtendWith(MockitoExtension.class)
class SpringSecurityAutoConfigurationTest {

  private ApplicationContextRunner contextRunner;

  @Mock
  private SpringConfigurationProperties springConfigurationProperties;

  @BeforeEach
  void setUp() {
    contextRunner = new ApplicationContextRunner()
        .withConfiguration(AutoConfigurations.of(SpringSecurityAutoConfiguration.class))
        .withBean(SpringConfigurationProperties.class, () -> springConfigurationProperties);
  }

  @Nested
  class BeanRegistration {

    @Test
    void shouldRegisterSpringRouteSecurityConfiguration() {
      contextRunner.run(context -> {
        assertTrue(context.containsBean("springSecurityConfiguration"));
        SpringRouteSecurityConfiguration bean =
            context.getBean(SpringRouteSecurityConfiguration.class);
        assertNotNull(bean);
      });
    }

    @Test
    void shouldRegisterSpringRouteSecurityManager() {
      contextRunner.run(context -> {
        assertTrue(context.containsBean("springSecurityManager"));
        SpringRouteSecurityManager bean = context.getBean(SpringRouteSecurityManager.class);
        assertNotNull(bean);
      });
    }

    @Test
    void shouldRegisterSpringRouteAccessEvaluator() {
      contextRunner.run(context -> {
        assertTrue(context.containsBean("springRouteAccessEvaluator"));
        SpringRouteAccessEvaluator bean = context.getBean(SpringRouteAccessEvaluator.class);
        assertNotNull(bean);
      });
    }

    @Test
    void shouldRegisterWebforjFrameworkRequestMatcher() {
      contextRunner.run(context -> {
        assertTrue(context.containsBean("webforjFrameworkRequestMatcher"));
        WebforjFrameworkRequestMatcher bean = context.getBean(WebforjFrameworkRequestMatcher.class);
        assertNotNull(bean);
      });
    }

    @Test
    void shouldRegisterWebforjAuthenticationSuccessHandler() {
      contextRunner.run(context -> {
        assertTrue(context.containsBean("webforjAuthenticationSuccessHandler"));
        WebforjAuthenticationSuccessHandler bean =
            context.getBean(WebforjAuthenticationSuccessHandler.class);
        assertNotNull(bean);
      });
    }

    @Test
    void shouldInstallWebforjSecurityContextHolderStrategyWithoutBean() {
      SecurityContextHolderStrategy originalStrategy =
          SecurityContextHolder.getContextHolderStrategy();

      try {
        contextRunner.run(context -> {
          assertEquals(0, context.getBeanNamesForType(SecurityContextHolderStrategy.class).length);
          assertInstanceOf(WebforjSecurityContextHolderStrategy.class,
              SecurityContextHolder.getContextHolderStrategy());
        });
      } finally {
        SecurityContextHolder.setContextHolderStrategy(originalStrategy);
      }
    }

    @Test
    void shouldInstallStrategyBeforeSingletonsAreCreated() {
      SecurityContextHolderStrategy originalStrategy =
          SecurityContextHolder.getContextHolderStrategy();

      try {
        contextRunner
            .withBean(StrategyReader.class,
                () -> new StrategyReader(SecurityContextHolder.getContextHolderStrategy()))
            .run(context -> assertInstanceOf(WebforjSecurityContextHolderStrategy.class,
                context.getBean(StrategyReader.class).getStrategy()));
      } finally {
        SecurityContextHolder.setContextHolderStrategy(originalStrategy);
      }
    }

    @Test
    void shouldKeepStrategyWhenApplicationDefinesStrategyBean() {
      SecurityContextHolderStrategy originalStrategy =
          SecurityContextHolder.getContextHolderStrategy();
      SecurityContextHolderStrategy applicationStrategy = mock(SecurityContextHolderStrategy.class);

      try {
        contextRunner.withBean(SecurityContextHolderStrategy.class, () -> applicationStrategy)
            .run(context -> {
              assertSame(applicationStrategy, context.getBean(SecurityContextHolderStrategy.class));
              assertSame(originalStrategy, SecurityContextHolder.getContextHolderStrategy());
            });
      } finally {
        SecurityContextHolder.setContextHolderStrategy(originalStrategy);
      }
    }
  }

  static final class StrategyReader {
    private final SecurityContextHolderStrategy strategy;

    StrategyReader(SecurityContextHolderStrategy strategy) {
      this.strategy = strategy;
    }

    SecurityContextHolderStrategy getStrategy() {
      return strategy;
    }
  }
}
