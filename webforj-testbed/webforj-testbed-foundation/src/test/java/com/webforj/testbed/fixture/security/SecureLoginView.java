package com.webforj.testbed.fixture.security;

import com.webforj.component.Composite;
import com.webforj.component.html.elements.Div;
import com.webforj.router.annotation.Route;
import com.webforj.router.security.annotation.AnonymousAccess;

/** Provides the login route for security tests. */
@Route("login")
@AnonymousAccess
public class SecureLoginView extends Composite<Div> {
}
