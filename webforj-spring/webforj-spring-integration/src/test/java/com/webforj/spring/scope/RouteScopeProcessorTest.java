package com.webforj.spring.scope;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.Environment;
import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.environment.ObjectTable;
import com.webforj.router.RouteRelation;
import com.webforj.router.RouteRenderer;
import com.webforj.router.Router;
import com.webforj.spring.scope.BeanStore;
import com.webforj.spring.scope.processor.RouteScopeProcessor;
import java.util.Collections;
import java.util.Optional;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.beans.factory.ObjectFactory;

class RouteScopeProcessorTest {

  private RouteScopeProcessor scopeProcessor;
  @SuppressWarnings("rawtypes")
  private ObjectFactory objectFactory;
  private MockedStatic<Environment> environmentMock;
  private MockedStatic<ObjectTable> objectTableMock;
  private MockedStatic<Router> routerMock;
  private Router mockRouter;
  private RouteRenderer mockRenderer;
  private BeanStore mockBeanStore;
  private Environment mockEnvironment;


  @BeforeEach
  void setUp() {
    scopeProcessor = new RouteScopeProcessor();
    objectFactory = mock(ObjectFactory.class);
    mockRouter = mock(Router.class);
    mockRenderer = mock(RouteRenderer.class);
    mockBeanStore = mock(BeanStore.class);
    mockEnvironment = mock(Environment.class);

    environmentMock = mockStatic(Environment.class);
    objectTableMock = mockStatic(ObjectTable.class);
    routerMock = mockStatic(Router.class);

    environmentMock.when(Environment::isPresent).thenReturn(true);
    environmentMock.when(Environment::getCurrent).thenReturn(mockEnvironment);
    routerMock.when(Router::getCurrent).thenReturn(mockRouter);
    when(mockRouter.getRenderer()).thenReturn(mockRenderer);
  }

  @AfterEach
  void tearDown() {
    if (environmentMock != null) {
      environmentMock.close();
    }
    if (objectTableMock != null) {
      objectTableMock.close();
    }
    if (routerMock != null) {
      routerMock.close();
    }
  }

  @Test
  void shouldCreateNewBeanForRoute() {
    String beanName = "testBean";
    Object expectedBean = new Object();


    @SuppressWarnings("unchecked")
    RouteRelation<Class<? extends Component>> routeRelation = mock(RouteRelation.class);
    when(routeRelation.getData()).thenReturn((Class) Component.class);
    when(mockRenderer.getActiveRoutePath()).thenReturn(Optional.of(routeRelation));

    when(objectFactory.getObject()).thenReturn(expectedBean);

    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      beanStoreMock.when(() -> BeanStore.getOrCreate(anyString())).thenReturn(mockBeanStore);
      when(mockBeanStore.get(anyString(), anyString(), any(ObjectFactory.class)))
          .thenReturn(expectedBean);

      Object result = scopeProcessor.get(beanName, objectFactory);

      assertNotNull(result);
      assertSame(expectedBean, result);
    }
  }

  @Test
  void shouldUseChildComponentWhenFrameIsRoot() {
    String beanName = "testBean";
    Object expectedBean = new Object();

    @SuppressWarnings("unchecked")
    RouteRelation<Class<? extends Component>> frameRelation = mock(RouteRelation.class);
    @SuppressWarnings("unchecked")
    RouteRelation<Class<? extends Component>> childRelation = mock(RouteRelation.class);

    when(frameRelation.getData()).thenReturn((Class) Frame.class);
    when(childRelation.getData()).thenReturn((Class) Component.class);
    when(frameRelation.getChildren()).thenReturn(Collections.singletonList(childRelation));
    when(mockRenderer.getActiveRoutePath()).thenReturn(Optional.of(frameRelation));

    when(objectFactory.getObject()).thenReturn(expectedBean);

    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      beanStoreMock.when(() -> BeanStore.getOrCreate(anyString())).thenReturn(mockBeanStore);
      when(mockBeanStore.get(anyString(), anyString(), any(ObjectFactory.class)))
          .thenReturn(expectedBean);

      Object result = scopeProcessor.get(beanName, objectFactory);

      assertNotNull(result);
      // Verify that the scope ID uses TestComponent (child) not Frame, and includes environment
      // identity
      String expectedScopeId = "webforj-route-" + System.identityHashCode(mockEnvironment) + "-"
          + Component.class.getName();
      verify(mockBeanStore).get(eq(expectedScopeId), eq(beanName), any(ObjectFactory.class));
    }
  }

  @Test
  void shouldThrowExceptionWhenNoRouterAvailable() {
    String beanName = "testBean";

    routerMock.when(Router::getCurrent).thenReturn(null);

    assertThrows(IllegalStateException.class, () -> {
      scopeProcessor.get(beanName, objectFactory);
    });
  }

  @Test
  void shouldThrowExceptionWhenNoActiveRoute() {
    String beanName = "testBean";

    when(mockRenderer.getActiveRoutePath()).thenReturn(Optional.empty());

    assertThrows(IllegalStateException.class, () -> {
      scopeProcessor.get(beanName, objectFactory);
    });
  }

  @Test
  void shouldRemoveBeanFromRouteScope() {
    String beanName = "testBean";
    Object expectedBean = new Object();


    @SuppressWarnings("unchecked")
    RouteRelation<Class<? extends Component>> routeRelation = mock(RouteRelation.class);
    when(routeRelation.getData()).thenReturn((Class) Component.class);
    when(mockRenderer.getActiveRoutePath()).thenReturn(Optional.of(routeRelation));

    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      beanStoreMock.when(() -> BeanStore.getOrCreate(anyString())).thenReturn(mockBeanStore);
      when(mockBeanStore.remove(anyString(), anyString())).thenReturn(expectedBean);

      Object result = scopeProcessor.remove(beanName);

      assertSame(expectedBean, result);
    }
  }

  @Test
  void shouldReturnNullWhenRemovingWithoutRoute() {
    String beanName = "testBean";

    when(mockRenderer.getActiveRoutePath()).thenReturn(Optional.empty());

    Object result = scopeProcessor.remove(beanName);

    assertNull(result);
  }

  @Test
  void shouldRegisterDestructionCallbackForRouteBean() {
    String beanName = "testBean";
    Runnable callback = mock(Runnable.class);


    @SuppressWarnings("unchecked")
    RouteRelation<Class<? extends Component>> routeRelation = mock(RouteRelation.class);
    when(routeRelation.getData()).thenReturn((Class) Component.class);
    when(mockRenderer.getActiveRoutePath()).thenReturn(Optional.of(routeRelation));

    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      beanStoreMock.when(() -> BeanStore.getOrCreate(anyString())).thenReturn(mockBeanStore);

      scopeProcessor.registerDestructionCallback(beanName, callback);

      verify(mockBeanStore).registerDestructionCallback(anyString(), eq(beanName), eq(callback));
    }
  }

  @Test
  void shouldIgnoreDestructionCallbackWhenNoRoute() {
    String beanName = "testBean";
    Runnable callback = mock(Runnable.class);

    when(mockRenderer.getActiveRoutePath()).thenReturn(Optional.empty());

    // Should not throw exception, just return
    scopeProcessor.registerDestructionCallback(beanName, callback);

    // Verify BeanStore was never accessed
    objectTableMock.verifyNoInteractions();
  }

  @Test
  void shouldResolveRouteAndRouterAsContextualObjects() {

    @SuppressWarnings("unchecked")
    RouteRelation<Class<? extends Component>> routeRelation = mock(RouteRelation.class);
    when(routeRelation.getData()).thenReturn((Class) Component.class);
    when(mockRenderer.getActiveRoutePath()).thenReturn(Optional.of(routeRelation));

    Object result = scopeProcessor.resolveContextualObject("webforj-route");
    assertEquals(Component.class, result);

    result = scopeProcessor.resolveContextualObject("webforj-router");
    assertSame(mockRouter, result);

    result = scopeProcessor.resolveContextualObject("unknown");
    assertNull(result);
  }

  @Test
  void shouldReturnRouteScopeIdAsConversationId() {

    @SuppressWarnings("unchecked")
    RouteRelation<Class<? extends Component>> routeRelation = mock(RouteRelation.class);
    when(routeRelation.getData()).thenReturn((Class) Component.class);
    when(mockRenderer.getActiveRoutePath()).thenReturn(Optional.of(routeRelation));

    String conversationId = scopeProcessor.getConversationId();

    assertNotNull(conversationId);
    String expectedScopeId = "webforj-route-" + System.identityHashCode(mockEnvironment) + "-"
        + Component.class.getName();
    assertEquals(expectedScopeId, conversationId);
  }

  @Test
  void shouldReturnNullConversationIdWhenNoRoute() {
    when(mockRenderer.getActiveRoutePath()).thenReturn(Optional.empty());

    String conversationId = scopeProcessor.getConversationId();

    assertNull(conversationId);
  }

  @Test
  void shouldCleanupSpecificRouteScope() {
    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      beanStoreMock.when(() -> BeanStore.cleanupScopeInstance(anyString(), anyString()))
          .thenReturn(true);

      RouteScopeProcessor.cleanupRoute(Component.class);

      String expectedScopeId = "webforj-route-" + System.identityHashCode(mockEnvironment) + "-"
          + Component.class.getName();
      beanStoreMock.verify(() -> BeanStore.cleanupScopeInstance(anyString(), eq(expectedScopeId)),
          times(1));
    }
  }

  @Test
  void shouldSkipRouteCleanupWhenNoEnvironment() {
    environmentMock.when(Environment::getCurrent).thenReturn(null);

    // Should not throw exception, just return
    RouteScopeProcessor.cleanupRoute(Component.class);

    // Verify BeanStore cleanup was never called
    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      beanStoreMock.verifyNoMoreInteractions();
    }
  }

  @Test
  void shouldKeepBeanStoreWhenOtherRouteScopesExist() {
    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      beanStoreMock.when(() -> BeanStore.cleanupScopeInstance(anyString(), anyString()))
          .thenReturn(false);

      RouteScopeProcessor.cleanupRoute(Component.class);

      String expectedScopeId = "webforj-route-" + System.identityHashCode(mockEnvironment) + "-"
          + Component.class.getName();
      beanStoreMock.verify(() -> BeanStore.cleanupScopeInstance(anyString(), eq(expectedScopeId)),
          times(1));
    }
  }

  @Test
  void shouldCleanupAllRouteScopes() {
    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      RouteScopeProcessor.cleanup();

      beanStoreMock.verify(() -> BeanStore.cleanupAllScopeInstances(anyString()), times(1));
    }
  }

  @Test
  void shouldSkipCleanupAllWhenNoEnvironment() {
    environmentMock.when(Environment::getCurrent).thenReturn(null);

    // Should not throw exception, just return
    RouteScopeProcessor.cleanup();

    // Verify BeanStore cleanup was never called
    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      beanStoreMock.verifyNoMoreInteractions();
    }
  }

  @Test
  void shouldCreateBeanStoreOnFirstRouteAccess() {
    // Mock the static BeanStore.getOrCreate method
    try (MockedStatic<BeanStore> beanStoreMock = mockStatic(BeanStore.class)) {
      BeanStore realStore = new BeanStore();
      beanStoreMock.when(() -> BeanStore.getOrCreate(anyString())).thenReturn(realStore);


      @SuppressWarnings("unchecked")
      RouteRelation<Class<? extends Component>> routeRelation = mock(RouteRelation.class);
      when(routeRelation.getData()).thenReturn((Class) Component.class);
      when(mockRenderer.getActiveRoutePath()).thenReturn(Optional.of(routeRelation));

      String beanName = "testBean";
      Object expectedBean = new Object();
      when(objectFactory.getObject()).thenReturn(expectedBean);

      // This should trigger BeanStore.getOrCreate
      Object result = scopeProcessor.get(beanName, objectFactory);

      assertNotNull(result);
      assertSame(expectedBean, result);

      // Verify BeanStore.getOrCreate was called
      beanStoreMock.verify(() -> BeanStore.getOrCreate(anyString()), times(1));
    }
  }
}
