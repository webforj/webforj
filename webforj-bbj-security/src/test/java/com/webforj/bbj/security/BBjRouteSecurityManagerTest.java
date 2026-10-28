package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.Environment;
import com.webforj.bbj.security.store.BBjSecurityStore;
import com.webforj.component.Component;
import com.webforj.router.NavigationContext;
import com.webforj.router.NavigationOptions;
import com.webforj.router.NavigationOptions.NavigationType;
import com.webforj.router.RouteRelation;
import com.webforj.router.RouteRenderer;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.evaluator.AnonymousAccessEvaluator;
import com.webforj.router.security.evaluator.AuthenticationRequiredEvaluator;
import com.webforj.router.security.evaluator.DenyAllEvaluator;
import com.webforj.router.security.evaluator.PermitAllEvaluator;
import com.webforj.router.security.evaluator.RolesAllowedEvaluator;
import com.webforj.webstorage.LocalStorage;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

class BBjRouteSecurityManagerTest {
  private static final String KEY = AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;

  private MockedStatic<Environment> environmentMock;
  private MockedStatic<LocalStorage> storageMock;
  private MockedStatic<Router> routerMock;
  private LocalStorage storage;
  private Router router;
  private BBjRouteSecurityConfiguration configuration;
  private BBjRouteSecurityContext securityContext;
  private BBjRouteSecurityManager manager;

  @BeforeEach
  void setUp() {
    storage = mock(LocalStorage.class);
    router = mock(Router.class);
    configuration = mock(BBjRouteSecurityConfiguration.class);
    securityContext = mock(BBjRouteSecurityContext.class);
    environmentMock = mockStatic(Environment.class);
    var mockEnviornment = mock(Environment.class);
    environmentMock.when(Environment::getCurrent).thenReturn(mockEnviornment);
    storageMock = mockStatic(LocalStorage.class);
    storageMock.when(LocalStorage::getCurrent).thenReturn(storage);
    routerMock = mockStatic(Router.class);
    routerMock.when(Router::getCurrent).thenReturn(router);
    when(configuration.getAuthenticationLocation()).thenReturn(Optional.of(new Location("/login")));
    when(configuration.getDenyLocation()).thenReturn(Optional.of(new Location("/denied")));
    manager = new BBjRouteSecurityManager(configuration, securityContext,
        BBjSecurityStore.create(mockEnviornment));
  }

  @AfterEach
  void tearDown() {
    routerMock.close();
    storageMock.close();
    environmentMock.close();
  }

  @Nested
  class Construction {

    @Test
    void shouldRejectNullConfiguration() {
      var store = mock(BBjSecurityStore.class);
      assertThrows(NullPointerException.class,
          () -> new BBjRouteSecurityManager(null, securityContext, store));
    }

    @Test
    void shouldRejectNullSecurityContext() {
      var store = mock(BBjSecurityStore.class);
      assertThrows(NullPointerException.class,
          () -> new BBjRouteSecurityManager(configuration, null, store));
    }

    @Test
    void shouldRejectNullStore() {
      assertThrows(NullPointerException.class,
          () -> new BBjRouteSecurityManager(configuration, securityContext, null));
    }

    @Test
    void shouldReturnConfigurationAndSecurityContext() {
      assertSame(configuration, manager.getConfiguration());
      assertSame(securityContext, manager.getSecurityContext());
    }

    @Test
    void shouldRegisterBuiltInEvaluatorsWithCorrectPriorities() {
      List<RouteSecurityEvaluator> evaluators = manager.getEvaluators();

      assertEquals(5, evaluators.size());
      assertInstanceOf(DenyAllEvaluator.class, evaluators.get(0));
      assertInstanceOf(AnonymousAccessEvaluator.class, evaluators.get(1));
      assertInstanceOf(AuthenticationRequiredEvaluator.class, evaluators.get(2));
      assertInstanceOf(PermitAllEvaluator.class, evaluators.get(3));
      assertInstanceOf(RolesAllowedEvaluator.class, evaluators.get(4));
    }
  }

  @Nested
  class PreAuthenticationLocation {

    @Test
    void shouldReadTheSavedLocation() {
      when(storage.get(KEY)).thenReturn("/protected?x=1");

      Optional<Location> location = manager.getPreAuthenticationLocation();

      assertEquals("/protected?x=1", location.orElseThrow().getFullURI());
    }

    @Test
    void shouldReadNothingWhenNoLocationIsSaved() {
      assertTrue(manager.getPreAuthenticationLocation().isEmpty());
    }

    @Test
    void shouldClearTheSavedLocation() {
      manager.clearPreAuthenticationLocation();

      verify(storage).remove(KEY);
    }
  }

  @Nested
  class AccessDenied {

    @Test
    void shouldSaveTheRequestedLocationWhenAuthenticationIsRequired() {
      NavigationContext navigationContext = mock(NavigationContext.class);
      when(navigationContext.getLocation()).thenReturn(new Location("/protected?x=1"));

      manager.onAccessDenied(RouteAccessDecision.denyAuthentication(), navigationContext);

      verify(storage).setItem(KEY, "/protected?x=1");
    }

    @Test
    void shouldSaveTheRequestedLocationWhenAccessIsDenied() {
      NavigationContext navigationContext = mock(NavigationContext.class);
      when(navigationContext.getLocation()).thenReturn(new Location("/admin"));

      manager.onAccessDenied(RouteAccessDecision.deny("insufficient_roles"), navigationContext);

      verify(storage).setItem(KEY, "/admin");
    }

    @Test
    void shouldNotSaveWithoutNavigationContext() {
      NavigationContext missing = null;
      RouteAccessDecision decision = RouteAccessDecision.denyAuthentication();

      assertDoesNotThrow(() -> manager.onAccessDenied(decision, missing));

      verify(storage, never()).setItem(any(), any());
    }

    @Test
    void shouldNotSaveWithoutLocation() {
      NavigationContext navigationContext = mock(NavigationContext.class);
      RouteAccessDecision decision = RouteAccessDecision.deny("insufficient_roles");

      manager.onAccessDenied(decision, navigationContext);

      verify(storage, never()).setItem(any(), any());
    }

    @Test
    void shouldRedirectToTheAuthenticationLocationAfterSaving() {
      NavigationContext navigationContext = mock(NavigationContext.class);
      when(navigationContext.getLocation()).thenReturn(new Location("/protected"));

      manager.onAccessDenied(RouteAccessDecision.denyAuthentication(), navigationContext);

      ArgumentCaptor<Location> target = ArgumentCaptor.forClass(Location.class);
      verify(router).navigate(target.capture(), any(NavigationOptions.class));
      assertEquals("/login", target.getValue().getFullURI());
    }
  }

  @Nested
  class PostAccessDenied {

    @Test
    void shouldNavigateToTheAuthenticationLocationWhenAuthenticationIsRequired() {
      manager.onPostAccessDenied(RouteAccessDecision.denyAuthentication("expired"), null);

      ArgumentCaptor<Location> target = ArgumentCaptor.forClass(Location.class);
      ArgumentCaptor<NavigationOptions> options = ArgumentCaptor.forClass(NavigationOptions.class);
      verify(router).navigate(target.capture(), options.capture());
      assertEquals("/login", target.getValue().getFullURI());
      assertEquals(NavigationType.REPLACE, options.getValue().getNavigationType());
    }

    @Test
    void shouldNavigateToTheDenyLocationWithReasonWhenAccessIsDenied() {
      manager.onPostAccessDenied(RouteAccessDecision.deny("insufficient_roles"), null);

      ArgumentCaptor<Location> target = ArgumentCaptor.forClass(Location.class);
      ArgumentCaptor<NavigationOptions> options = ArgumentCaptor.forClass(NavigationOptions.class);
      verify(router).navigate(target.capture(), options.capture());
      assertEquals("/denied?reason=insufficient_roles", target.getValue().getFullURI());
      assertEquals(NavigationType.REPLACE, options.getValue().getNavigationType());
    }

    @Test
    void shouldNavigateToTheDenyLocationWithoutReasonWhenReasonIsEmpty() {
      manager.onPostAccessDenied(RouteAccessDecision.deny(""), null);

      ArgumentCaptor<Location> target = ArgumentCaptor.forClass(Location.class);
      verify(router).navigate(target.capture(), any(NavigationOptions.class));
      assertEquals("/denied", target.getValue().getFullURI());
    }

    @Test
    void shouldNotNavigateWhenNoLocationIsConfigured() {
      when(configuration.getAuthenticationLocation()).thenReturn(Optional.empty());
      when(configuration.getDenyLocation()).thenReturn(Optional.empty());

      manager.onPostAccessDenied(RouteAccessDecision.denyAuthentication(), null);
      manager.onPostAccessDenied(RouteAccessDecision.deny("insufficient_roles"), null);

      verify(router, never()).navigate(any(Location.class), any(NavigationOptions.class));
    }

    @Test
    void shouldNotFailWithoutRouter() {
      routerMock.when(Router::getCurrent).thenReturn(null);
      RouteAccessDecision decision = RouteAccessDecision.denyAuthentication();

      assertDoesNotThrow(() -> manager.onPostAccessDenied(decision, null));
    }

    @Test
    void shouldNotRecreateFromRouteWhenNothingIsRendered() {
      RouteRenderer renderer = mock(RouteRenderer.class);
      when(router.getRenderer()).thenReturn(renderer);
      when(renderer.getActiveRoutePath()).thenReturn(Optional.empty());

      manager.onPostAccessDenied(RouteAccessDecision.denyAuthentication(), null);

      ArgumentCaptor<NavigationOptions> options = ArgumentCaptor.forClass(NavigationOptions.class);
      verify(router).navigate(any(Location.class), options.capture());
      assertNull(options.getValue().getRecreateFrom());
    }

    @Test
    void shouldRecreateFromTheRenderedRoute() {
      RouteRenderer renderer = mock(RouteRenderer.class);
      RouteRelation<Class<? extends Component>> active = new RouteRelation<>(Component.class);
      when(router.getRenderer()).thenReturn(renderer);
      when(renderer.getActiveRoutePath()).thenReturn(Optional.of(active));

      manager.onPostAccessDenied(RouteAccessDecision.denyAuthentication(), null);

      ArgumentCaptor<NavigationOptions> options = ArgumentCaptor.forClass(NavigationOptions.class);
      verify(router).navigate(any(Location.class), options.capture());
      assertEquals(Component.class, options.getValue().getRecreateFrom());
    }
  }
}
