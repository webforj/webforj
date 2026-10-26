package com.webforj.testbed.fixture.security;

import com.webforj.component.Composite;
import com.webforj.component.html.elements.Div;
import com.webforj.router.annotation.Route;
import com.webforj.router.security.annotation.AnonymousAccess;

/** Provides the access-denied route for security tests. */
@Route("denied")
@AnonymousAccess
public class SecureDeniedView extends Composite<Div> {
}
