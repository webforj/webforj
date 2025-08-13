package com.webforj.spring.scope;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjObjectTable;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.bridge.WebforjBBjBridge;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class WebforjScopeAnnotationTest {

  @Mock
  private BBjAPI mockApi;

  @Mock
  private BBjObjectTable mockObjectTable;

  @Mock
  private WebforjBBjBridge mockBridge;

  private AnnotationConfigApplicationContext context;
  private Map<String, Object> objectTableStorage;

  @BeforeEach
  void setUp() throws BBjException {
    objectTableStorage = new HashMap<>();

    // Mock the ObjectTable behavior
    when(mockApi.getObjectTable()).thenReturn(mockObjectTable);
    // Mock put
    doAnswer(invocation -> {
      String key = invocation.getArgument(0);
      Object value = invocation.getArgument(1);
      objectTableStorage.put(key, value);
      return null;
    }).when(mockObjectTable).put(anyString(), any());
    when(mockObjectTable.get(anyString())).thenAnswer(invocation -> {
      String key = invocation.getArgument(0);
      Object value = objectTableStorage.get(key);
      if (value == null) {
        throw new BBjException("Key not found: " + key, 0);
      }
      return value;
    });
    // Mock remove
    doAnswer(invocation -> {
      String key = invocation.getArgument(0);
      objectTableStorage.remove(key);
      return null;
    }).when(mockObjectTable).remove(anyString());
    when(mockObjectTable.size()).thenAnswer(invocation -> objectTableStorage.size());

    Environment.init(mockApi, mockBridge, 0);
  }

  @AfterEach
  void tearDown() {
    if (context != null) {
      context.close();
    }

    Environment.cleanup();
    objectTableStorage.clear();
  }

  @Nested
  class EnvironmentScopeAnnotation {

    @Test
    void shouldCreateEnvironmentScopedBean() {
      context = new AnnotationConfigApplicationContext();
      context.register(WebforjScopeConfiguration.class);
      context.register(EnvironmentScopedConfig.class);
      context.refresh();

      EnvironmentScopedService service1 = context.getBean(EnvironmentScopedService.class);
      EnvironmentScopedService service2 = context.getBean(EnvironmentScopedService.class);

      assertNotNull(service1);
      assertSame(service1, service2);
      assertEquals(service1.getId(), service2.getId());
    }

    @Test
    void shouldCreateNewInstanceForNewEnvironment() throws BBjException {
      context = new AnnotationConfigApplicationContext();
      context.register(WebforjScopeConfiguration.class);
      context.register(EnvironmentScopedConfig.class);
      context.refresh();

      EnvironmentScopedService service1 = context.getBean(EnvironmentScopedService.class);
      int firstId = service1.getId();

      // new environment
      EnvironmentScope.cleanup();
      Environment.cleanup();
      Environment.init(mockApi, mockBridge, 0);

      EnvironmentScopedService service2 = context.getBean(EnvironmentScopedService.class);
      int secondId = service2.getId();

      assertNotNull(service1);
      assertNotNull(service2);
      assertTrue(firstId != secondId);
    }

    @Test
    void shouldSupportAnnotationOnComponent() {
      context = new AnnotationConfigApplicationContext();
      context.register(WebforjScopeConfiguration.class);
      context.scan("com.webforj.spring.scope");
      context.refresh();

      EnvironmentScopedComponent component1 = context.getBean(EnvironmentScopedComponent.class);
      EnvironmentScopedComponent component2 = context.getBean(EnvironmentScopedComponent.class);

      assertNotNull(component1);
      assertSame(component1, component2);
    }
  }

  @Nested
  class RequestScopeAnnotation {

    @Test
    void shouldCreateRequestScopedBean() {
      context = new AnnotationConfigApplicationContext();
      context.register(WebforjScopeConfiguration.class);
      context.register(RequestScopedConfig.class);
      context.refresh();

      RequestScopedService service1 = context.getBean(RequestScopedService.class);
      RequestScopedService service2 = context.getBean(RequestScopedService.class);

      assertNotNull(service1);
      assertSame(service1, service2);
      assertEquals(service1.getId(), service2.getId());
    }

    @Test
    void shouldBehaveSameAsEnvironmentScope() throws BBjException {
      context = new AnnotationConfigApplicationContext();
      context.register(WebforjScopeConfiguration.class);
      context.register(RequestScopedConfig.class);
      context.register(EnvironmentScopedConfig.class);
      context.refresh();

      RequestScopedService requestService = context.getBean(RequestScopedService.class);
      EnvironmentScopedService envService = context.getBean(EnvironmentScopedService.class);

      int requestId1 = requestService.getId();
      int envId1 = envService.getId();

      // new environment
      EnvironmentScope.cleanup();
      Environment.cleanup();
      Environment.init(mockApi, mockBridge, 0);

      RequestScopedService newRequestService = context.getBean(RequestScopedService.class);
      EnvironmentScopedService newEnvService = context.getBean(EnvironmentScopedService.class);

      int requestId2 = newRequestService.getId();
      int envId2 = newEnvService.getId();

      assertTrue(requestId1 != requestId2);
      assertTrue(envId1 != envId2);
    }
  }

  @Nested
  class MixedScopes {

    @Test
    void shouldHandleDependencyInjection() {
      context = new AnnotationConfigApplicationContext();
      context.register(WebforjScopeConfiguration.class);
      context.register(DependencyInjectionConfig.class);
      context.refresh();

      ComplexService service = context.getBean(ComplexService.class);

      assertNotNull(service);
      assertNotNull(service.getEnvironmentService());
      assertNotNull(service.getSingletonService());

      // Get another instance - should have same dependencies
      ComplexService service2 = context.getBean(ComplexService.class);
      assertSame(service, service2);
      assertSame(service.getEnvironmentService(), service2.getEnvironmentService());
      assertSame(service.getSingletonService(), service2.getSingletonService());
    }
  }

  // Test configurations and beans
  @Configuration
  static class EnvironmentScopedConfig {
    @Bean
    @WebforjEnvironmentScope
    public EnvironmentScopedService environmentScopedService() {
      return new EnvironmentScopedService();
    }
  }

  @Configuration
  static class RequestScopedConfig {
    @Bean
    @WebforjRequestScope
    public RequestScopedService requestScopedService() {
      return new RequestScopedService();
    }
  }

  @Configuration
  static class DependencyInjectionConfig {
    @Bean
    public SingletonService singletonService() {
      return new SingletonService();
    }

    @Bean
    @WebforjEnvironmentScope
    public EnvironmentScopedService environmentScopedService() {
      return new EnvironmentScopedService();
    }

    @Bean
    @WebforjEnvironmentScope
    public ComplexService complexService(EnvironmentScopedService environmentService,
        SingletonService singletonService) {
      return new ComplexService(environmentService, singletonService);
    }
  }

  // Test service classes
  static class EnvironmentScopedService {
    private static final AtomicInteger counter = new AtomicInteger(0);
    private final int id;

    public EnvironmentScopedService() {
      this.id = counter.incrementAndGet();
    }

    public int getId() {
      return id;
    }
  }

  static class RequestScopedService {
    private static final AtomicInteger counter = new AtomicInteger(0);
    private final int id;

    public RequestScopedService() {
      this.id = counter.incrementAndGet();
    }

    public int getId() {
      return id;
    }
  }

  static class SingletonService {
    private static final AtomicInteger counter = new AtomicInteger(0);
    private final int id;

    public SingletonService() {
      this.id = counter.incrementAndGet();
    }

    public int getId() {
      return id;
    }
  }

  static class PrototypeService {
    private static final AtomicInteger counter = new AtomicInteger(0);
    private final int id;

    public PrototypeService() {
      this.id = counter.incrementAndGet();
    }

    public int getId() {
      return id;
    }
  }

  static class ComplexService {
    private final EnvironmentScopedService environmentService;
    private final SingletonService singletonService;

    public ComplexService(EnvironmentScopedService environmentService,
        SingletonService singletonService) {
      this.environmentService = environmentService;
      this.singletonService = singletonService;
    }

    public EnvironmentScopedService getEnvironmentService() {
      return environmentService;
    }

    public SingletonService getSingletonService() {
      return singletonService;
    }
  }

  @Component
  @WebforjEnvironmentScope
  static class EnvironmentScopedComponent {
    private static final AtomicInteger counter = new AtomicInteger(0);
    private final int id;

    public EnvironmentScopedComponent() {
      this.id = counter.incrementAndGet();
    }

    public int getId() {
      return id;
    }
  }
}
