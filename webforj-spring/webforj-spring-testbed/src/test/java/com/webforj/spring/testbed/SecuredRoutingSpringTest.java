package com.webforj.spring.testbed;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Component;
import com.webforj.spring.security.SpringRouteSecurityManager;
import com.webforj.spring.testbed.fixture.security.SecuredFixtureApp;
import com.webforj.spring.testbed.fixture.security.SpringSecureAdminView;
import com.webforj.spring.testbed.fixture.security.SpringSecureDeniedView;
import com.webforj.spring.testbed.fixture.security.SpringSecureLoginView;
import com.webforj.spring.testbed.fixture.security.SpringSecurePublicView;
import com.webforj.testbed.WebforjTestbed;
import com.webforj.testbed.annotation.WebforjTest;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.security.test.context.support.WithMockUser;

@SpringBootTest
@WebforjTest(app = SecuredFixtureApp.class)
class SecuredRoutingSpringTest {

  @Autowired
  private SpringRouteSecurityManager securityManager;

  @Test
  void shouldGrantAccessToThePublicRoute(WebforjTestbed context) {
    SpringSecurePublicView view = context.navigate(SpringSecurePublicView.class);

    assertTrue(view.isAttached());
  }

  @Test
  @WithMockUser(username = "ada", roles = "ADMIN")
  void shouldGrantAccessWhenTheMockUserHasTheRole(WebforjTestbed context) {
    SpringSecureAdminView view = context.navigate(SpringSecureAdminView.class);

    assertTrue(view.isAttached());
  }

  @Test
  @WithMockUser(username = "ada", roles = "USER")
  void shouldRedirectToTheDenyLocationWhenTheRoleIsMissing(WebforjTestbed context) {
    Component rendered = context.navigate("admin");

    assertInstanceOf(SpringSecureDeniedView.class, rendered);
  }

  @Test
  void shouldRedirectToTheLoginLocationWhenUnauthenticated(WebforjTestbed context) {
    Component rendered = context.navigate("admin");

    assertInstanceOf(SpringSecureLoginView.class, rendered);
  }

  @Test
  void shouldRecordThePreAuthenticationLocationAfterTheRedirect(WebforjTestbed context) {
    context.navigate("admin");

    assertEquals("/admin",
        securityManager.getPreAuthenticationLocation().orElseThrow().getFullURI());
  }
}
