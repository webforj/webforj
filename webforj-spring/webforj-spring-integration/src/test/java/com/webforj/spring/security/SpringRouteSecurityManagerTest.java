package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.SecurityEvaluatorChain;
import com.webforj.router.security.evaluator.AnonymousAccessEvaluator;
import com.webforj.router.security.evaluator.AuthenticationRequiredEvaluator;
import com.webforj.router.security.evaluator.DenyAllEvaluator;
import com.webforj.router.security.evaluator.PermitAllEvaluator;
import com.webforj.router.security.evaluator.RolesAllowedEvaluator;
import com.webforj.spring.security.annotation.RegisteredEvaluator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;

@ExtendWith(MockitoExtension.class)
class SpringRouteSecurityManagerTest {

  @InjectMocks
  private SpringRouteSecurityManager securityManager;

  @Mock
  private SpringRouteSecurityConfiguration configuration;

  @Mock
  private ApplicationContext applicationContext;


  @Test
  void shouldReturnSecurityContext() {
    RouteSecurityContext context = securityManager.getSecurityContext();
    assertNotNull(context);
    assertTrue(context instanceof SpringRouteSecurityContext);
  }

  @Nested
  class BuiltInEvaluators {

    @Test
    void shouldRegisterBuiltInEvaluatorsWithCorrectPriorities() {
      securityManager.initialize();
      List<RouteSecurityEvaluator> evaluators = securityManager.getEvaluators();

      assertTrue(evaluators.get(0) instanceof DenyAllEvaluator);
      assertTrue(evaluators.get(1) instanceof AnonymousAccessEvaluator);
      assertTrue(evaluators.get(2) instanceof AuthenticationRequiredEvaluator);
      assertTrue(evaluators.get(3) instanceof PermitAllEvaluator);
      assertTrue(evaluators.get(4) instanceof RolesAllowedEvaluator);
    }

    @Test
    void shouldRegisterSpringRouteAccessEvaluatorWhenAvailable() {
      RouteSecurityEvaluator springRouteAccessEvaluator = mock(RouteSecurityEvaluator.class);
      when(applicationContext.getBean("springRouteAccessEvaluator", RouteSecurityEvaluator.class))
          .thenReturn(springRouteAccessEvaluator);
      when(applicationContext.getBeansWithAnnotation(RegisteredEvaluator.class))
          .thenReturn(new HashMap<>());

      securityManager.initialize();

      List<RouteSecurityEvaluator> evaluators = securityManager.getEvaluators();
      assertSame(springRouteAccessEvaluator, evaluators.get(5));
    }
  }

  @Nested
  class CustomEvaluators {

    @Test
    void shouldRegisterCustomAnnotatedEvaluators() {
      // Create custom evaluator with annotation
      CustomEvaluator customEvaluator = new CustomEvaluator();
      Map<String, Object> evaluators = new HashMap<>();
      evaluators.put("customEvaluator", customEvaluator);

      when(applicationContext.getBeansWithAnnotation(RegisteredEvaluator.class))
          .thenReturn(evaluators);
      when(applicationContext.getBean("springRouteAccessEvaluator", RouteSecurityEvaluator.class))
          .thenThrow(new NoSuchBeanDefinitionException("springRouteAccessEvaluator"));

      securityManager.initialize();

      List<RouteSecurityEvaluator> registeredEvaluators = securityManager.getEvaluators();
      assertSame(customEvaluator, registeredEvaluators.get(5));
    }
  }

  // Test evaluator classes
  @RegisteredEvaluator(priority = 1.5)
  static class CustomEvaluator implements RouteSecurityEvaluator {
    @Override
    public boolean supports(Class<?> target) {
      return true;
    }

    @Override
    public RouteAccessDecision evaluate(Class<?> target,
        com.webforj.router.NavigationContext navContext, RouteSecurityContext securityContext,
        SecurityEvaluatorChain chain) {
      return RouteAccessDecision.grant();
    }
  }
}
