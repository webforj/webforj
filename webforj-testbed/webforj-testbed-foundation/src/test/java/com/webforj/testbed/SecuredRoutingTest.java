package com.webforj.testbed;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Component;
import com.webforj.router.security.evaluator.RolesAllowedEvaluator;
import com.webforj.testbed.fixture.security.SecureAdminView;
import com.webforj.testbed.fixture.security.SecureDeniedView;
import com.webforj.testbed.fixture.security.SecureLoginView;
import com.webforj.testbed.fixture.security.SecurePublicView;
import com.webforj.testbed.fixture.security.SecuredApp;
import com.webforj.testbed.fixture.security.SecurityAppLifecycleListener;
import com.webforj.testbed.fixture.security.TestSecurityContextHolder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

class SecuredRoutingTest extends TestbedTestSupport {
  private static final String ADMIN_ROUTE = "admin";

  @Override
  protected WebforjTestbedOptions options() {
    return new WebforjTestbedOptions().setApp(SecuredApp.class);
  }

  @AfterEach
  void tearDown() {
    TestSecurityContextHolder.logout();
  }

  @Test
  void shouldGrantAccessToThePublicRoute() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    SecurePublicView view = context.navigate(SecurePublicView.class);

    assertTrue(view.isAttached());
  }

  @Test
  void shouldGrantAccessWhenTheRoleMatches() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    TestSecurityContextHolder.login("ada", "ADMIN");

    SecureAdminView view = context.navigate(SecureAdminView.class);

    assertTrue(view.isAttached());
  }

  @Test
  void shouldFailNamingTheRequestedViewAndTheDenialReason() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    TestSecurityContextHolder.login("ada", "USER");

    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> context.navigate(SecureAdminView.class));

    assertTrue(failure.getMessage().contains("SecureAdminView"));
    assertTrue(failure.getMessage().contains(RolesAllowedEvaluator.CODE));
  }

  @Test
  void shouldRedirectToTheDenyLocation() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    TestSecurityContextHolder.login("ada", "USER");

    Component rendered = context.navigate(ADMIN_ROUTE);

    assertInstanceOf(SecureDeniedView.class, rendered);
  }

  @Test
  void shouldRedirectToTheLoginLocationWhenUnauthenticated() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Component rendered = context.navigate(ADMIN_ROUTE);

    assertInstanceOf(SecureLoginView.class, rendered);
  }

  @Test
  void shouldRecordThePreAuthenticationLocationAfterTheRedirect() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    context.navigate(ADMIN_ROUTE);

    assertEquals("/admin", SecurityAppLifecycleListener.getCurrentManager()
        .getPreAuthenticationLocation().orElseThrow().getFullURI());
  }
}
