package com.webforj.testbed.fixture.security;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.router.Router;
import com.webforj.router.security.RouteSecurityObserver;
import com.webforj.router.security.evaluator.AnonymousAccessEvaluator;
import com.webforj.router.security.evaluator.DenyAllEvaluator;
import com.webforj.router.security.evaluator.PermitAllEvaluator;
import com.webforj.router.security.evaluator.RolesAllowedEvaluator;
import java.util.concurrent.atomic.AtomicReference;

/** Installs route security during test application startup. */
public class SecurityAppLifecycleListener implements AppLifecycleListener {
  private static final AtomicReference<TestRouteSecurityManager> current = new AtomicReference<>();

  public static TestRouteSecurityManager getCurrentManager() {
    return current.get();
  }

  @Override
  public void onWillRun(App app) {
    if (app instanceof SecuredApp) {
      register(new TestRouteSecurityManager(new TestRouteSecurityConfiguration()));
    } else if (app instanceof SecuredStrictApp) {
      register(new TestRouteSecurityManager(new TestRouteSecurityConfigurationNoRedirect()));
    }
  }

  private void register(TestRouteSecurityManager manager) {
    manager.registerEvaluator(new DenyAllEvaluator(), 0);
    manager.registerEvaluator(new AnonymousAccessEvaluator(), 1);
    manager.registerEvaluator(new PermitAllEvaluator(), 2);
    manager.registerEvaluator(new RolesAllowedEvaluator(), 3);

    Router router = Router.getCurrent();
    if (router != null) {
      router.getRenderer().addObserver(new RouteSecurityObserver(manager));
    }

    current.set(manager);
  }
}
