package com.webforj.spring.testbed.fixture.security;

import com.webforj.component.Composite;
import com.webforj.component.html.elements.Div;
import com.webforj.router.annotation.Route;
import com.webforj.router.security.annotation.AnonymousAccess;

/** Provides the login route for Spring security tests. */
@Route("login")
@AnonymousAccess
public class SpringSecureLoginView extends Composite<Div> {
}
