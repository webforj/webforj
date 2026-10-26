package com.webforj.spring.testbed.fixture.security;

import com.webforj.component.Composite;
import com.webforj.component.html.elements.Div;
import com.webforj.router.annotation.Route;
import com.webforj.router.security.annotation.AnonymousAccess;

/** Provides the access-denied route for Spring security tests. */
@Route("denied")
@AnonymousAccess
public class SpringSecureDeniedView extends Composite<Div> {
}
